#' @title Base Model Selection & Hyperparameter Tuning
#' 
#' @description  This script splits LC cleaned data and uses validaiton set to find optimal (accuracy + run time) base model and hyperparameters.
#' 
#' @details 
#' Using validation set, train / test XGB using CV and grid search. Train / test RF using CV and various levels of ntree.
#' Compare XGB / RF performance.
#' 
#' Uses intuitition from following source:
#' 
#' BADS Tutorials. (2018). Humboldt Universität Information Systems.

###################################
#                                 #
#  1. SPLIT CLEANED DATA          #  
#                                 #
###################################
library("mlr")
library("parallelMap")
library("parallel")
library("xgboost")
library("caret")

data <- readRDS('lc_data_for_loop_clean.rds')

### Split 80/10/10 for loop train/validation/holdout. ###
idx<- createDataPartition(data$BAD, p = 0.8, list = FALSE)
train <- data[idx,]
val_hold  <- data[-idx,]

### Split val_hold 50/50 for validation (hyperparameter tuning) and holdout set. ###
idx<- createDataPartition(val_hold$BAD, p = 0.5, list = FALSE)
holdout <- val_hold[idx,]
val  <- val_hold[-idx,]
rm(val_hold)

### Make loop training set a round number (if split resulted in 80001). ###
train <- train[1:80000,] 

###################################
#                                 #
#  2. XGB TRAIN / TEST            #  
#                                 #
###################################

val_mlr <- mlr::createDummyFeatures(val, target="BAD")

### Split validation set into train/test.
idx<- createDataPartition(val_mlr$BAD, p = 0.8, list = FALSE)
tr <- val_mlr[idx,]
ts  <- val_mlr[-idx,]

# Prepare the mlr:
task <- makeClassifTask(data = tr, target = "BAD", positive = "BAD")

xgb.learner <- makeLearner("classif.xgboost", predict.type = "prob", 
                           par.vals = list("verbose" = 0,
                                           "early_stopping_rounds"=20)) 

# Set tuning parameters:
xgb.parms <- makeParamSet(
  makeNumericParam("eta", lower = 0.01, upper = 0.05), #learning rate
  makeIntegerParam("nrounds", lower=80, upper=400), #number of trees 
  makeIntegerParam("max_depth", lower=2, upper=6), #max depth of tree 
  makeDiscreteParam("gamma", values = 0), 
  makeDiscreteParam("colsample_bytree", values = 1), 
  makeDiscreteParam("min_child_weight", values = 1), 
  makeDiscreteParam("subsample", values = 1) 
)

# Set limit of tuning parameters:
tuneControl <- makeTuneControlRandom(maxit=100, tune.threshold = FALSE) 

# 3-fold CV
rdesc <- makeResampleDesc(method = "RepCV", rep = 3, folds=2, stratify = TRUE) 

# Train in parallel
parallelStartSocket(4, level = "mlr.tuneParams")
RNGkind("L'Ecuyer-CMRG")  
clusterSetRNGStream(iseed = 1234567)
xgb.tuning <- tuneParams(xgb.learner, task = task, resampling = rdesc,
                         par.set = xgb.parms, control = tuneControl, measures = mlr::auc) 

parallelStop()

xgb.learner <- setHyperPars(xgb.learner, par.vals = c(xgb.tuning$x, "verbose" = 0))
model_library <- list()
model_library[["xgb"]] <- mlr::train(xgb.learner, task = task)

# Predict on Val_Test Data
pred <- sapply(model_library, predict, newdata = ts, simplify=FALSE)
auc <- sapply(pred, mlr::performance, measures = mlr::auc)
auc

# Results:
# [Tune] Result: eta=0.0411; nrounds=201; max_depth=4; gamma=0; colsample_bytree=1; 
# min_child_weight=1; subsample=1 : auc.test.mean=0.7505839
# xgb.auc on Test: 
# 0.775415


###################################
#                                 #
#  3. RF TRAIN / TEST             #  
#                                 #
###################################

### Re-define split validation set into train/test again (no mlr dummy cleaning)
idx<- createDataPartition(val$BAD, p = 0.8, list = FALSE)
tr <- val[idx,]
ts <- val[-idx,]

# Create Task
task <- makeClassifTask(data = tr, target = "BAD", positive = "BAD", fixup.data = "no", check.data = FALSE)
rf <- makeLearner("classif.randomForest", 
                  predict.type = "prob", 
                  par.vals = list("replace" = TRUE, "importance" = FALSE))
rf.parms <- makeParamSet(
  makeIntegerParam("mtry", lower = 4, upper = 4), #Set to Breiman rule of thumb, sqrt 18 ~ 4
  makeIntegerParam("ntree", lower = 200, upper = 600) #Range no. trees - keep small for faster run time.
) 

tuneControl <- makeTuneControlGrid(resolution = 3, tune.threshold = FALSE)
rdesc <- makeResampleDesc(method = "CV", iters = 5, stratify = TRUE)

parallelStartSocket(4, level = "mlr.resample")

tuning <- tuneParams(rf, task = task, resampling = rdesc,
                       par.set = rf.parms, control = tuneControl, measures = mlr::auc)

parallelStop()

#Best performance:
# [Tune] Result: mtry=4; ntree=600 : auc.test.mean=0.7675277

#Train on full set
rf_tuned <- setHyperPars(rf, par.vals = tuning$x)
modelLib <- list()
yhat <- list()
modelLib[["rf"]] <- mlr::train(rf_tuned, task = task)

#Predit on test set
yhat[["rf"]] <- predict(modelLib[["rf"]], newdata = ts)
mlr::performance(yhat[["rf"]], measures = mlr::auc)

# RF AUC 
# 0.7634941 

# => RF / XGB AUC similar (.77 / .76) - use RF for faster run time. mtry = 4, ntree = 600