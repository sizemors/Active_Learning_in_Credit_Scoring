#' @title LendingClub Acceptance Loop
#' 
#' @description  Runs iterative acceptance loop.  
#' 
#' @details 
#' At each iteration, 8 models are trained and predict on holdout set.  Subsequent applicant batch
#' is accepted-rejected based on predictions from base model. Outputs an .RDS file of predictions from each iteration 
#' (containing predictions from each of 8 models) and outputs .RDS file of instances selected by each method at each iteration to calculate
#' sampling costs.
#' 
#' Uses intuition from:
#' Kozodoi, N. (2020). Appendix of Unpublished Manuscript – Submitted to Management Science.


###################################
#                                 #
#  1. LOOP PREPARATION            #  
#                                 #
###################################
library("ggplot2")
library("reshape2")
library("rpart") 
library("caret")
library("hmeasure")
library("dplyr")
library("tidyr")
library("ggrepel")
library("randomForest")
library("raster")
library("pracma")
library("itertools2")
library("entropy")
library("gtools")
library("gtools")
library("logOfGamma")
library("doParallel")

### Create 200 applicant batches based on fold index. ###
folds <- createFolds(train$BAD, k = 200, list = FALSE) #createFolds does startified random sample
train$fold <- folds
rapp_list = split(train, f = train$fold) #Split by index.
rapp_list <- lapply(rapp_list, function(x) x[!(names(x) %in% c("fold"))]) #Remove fold variable from all sets.

### Manually name the first batch "app", then remove from list. Remaining list of batches will be fed into loop. ###
app <- rapp_list[[1]] 
rapp_list[[1]] <- NULL
names(rapp_list) <- 1:199
names <- 1:199

### Use validation set to find a continuous variable with significant correlation with default to accept-reject on opening day. ###
model_glm <- glm(formula = BAD ~ .,
                   family = binomial(link = "logit"),
                   data = val, na.action = na.exclude)
summary(model_glm) #Use annual income: negatively correlated with default = accept those with highest income.

### Opening Day: accept-reject applicants in Batch 1 - Accept lowest 20% risk based on annual income, reject lower 80%. ### 
acc <- 
  app %>% 
  slice_max(annual_inc, prop = 0.20, with_ties = FALSE)

rej<- dplyr::anti_join(app, acc)

### Set up dataframe from which OPAL stats (ls = p,n and density) will be pulled ###
app_meta <- train_cluster2
app_meta$label <- 0 # To begin, no points are labeled / no stats known:
app_meta$n <- NA
app_meta$pObs <- NA
app_meta$OPALG <- NA

### Set traincontrol to none, RF hyperparameters stable across iterations / models (mtry = 4, ntree = 600 from LOOP_HYPERPARAMETER.R)
ctrl <- trainControl(method = "none")  

### Create empty dataframes for Random and AL meta to accumulate. ###
ra_tr_meta <- data.frame()
al_us_tr_meta <- data.frame()
al_qbc_tr_meta <- data.frame()
al_opal_tr_meta <- data.frame()
rej_meta <- data.frame() 

### HCO/PAR at i=1 needs to have RF predictions, but don't have yet. Use again annual_income as proxy for this function at i=0 ###
rej_ri <- rej[,-c(18)]
acc_ri <- acc
acc_ri$rf <- acc$annual_inc
rej_ri$rf <- rej$annual_inc
#HCO
hco <- hco_func(acc_ri, rej_ri)
hco$rf <- NULL
#PAR
parc <- parcel(acc_ri, rej_ri)
parc$rf <- NULL

###################################
#                                 #
#  2. DEFINE TRAIN FUNCTION       #  
#                                 #
###################################

first_func <- function(acc, rej, app, hco, parc){
  
  #Random set-up
  ra_tr <- rbind(ra_tr_meta, sample_frac(rej, .1, replace = FALSE))
  
  #AL set-up for US and QBC
  rej_al <- rej
  rej_al$BAD <- replace(rej_al$BAD, rej_al$BAD == "GOOD" | rej_al$BAD == "BAD", NA)
  full_al <- rbind(rej_al, acc)
  x_al <- full_al[,-18]
  y_al <- full_al[,18]
  
  #US
  al_us <- uncertainty_sampling(x=x_al, y=y_al, num_query= (.1*nrow(rej_al)))  ##Train control, trees, mtry are defined in u_s function, other script.
  al_tr <- rbind(al_us_tr_meta, dplyr::left_join(as.data.frame(al_us[[1]]),rej))
  
  #QBC
  al_qbc <- query_bagging(x=x_al, y=y_al, num_query=(.1*nrow(rej_al)), C=5)  
  al_qbc_tr <- rbind(al_qbc_tr_meta, dplyr::left_join(as.data.frame(al_qbc[[1]]),rej))
  
  #AL OPAL set-up
  acc_opal <- acc
  acc_opal$labell <- 1 
  
  #Use OPAL stats dataframe to determine number of labeled instances in cluster, calculate n and pObs.
  app_meta <- as.data.frame(
              dplyr::left_join(app_meta, acc_opal) %>% 
              rowwise %>%
              mutate(label = ifelse(labell == 1 | label == 1, 1, 0))  %>%
              dplyr::select(-labell) %>%
              dplyr::mutate(label = replace_na(label, 0)))  %>%
              group_by(clust) %>%
              mutate(n = sum(label == 1)) %>%
              mutate(pObs = ifelse(n != 0, sum(label == 1 & BAD == "BAD") / sum(label == 1), 0))
  
  #Get n, pObs, density stats for mini reject pool, calculate OPALG for them, take top .10, remove those stats.
  al_opal <- as.data.frame( 
             dplyr::left_join(rej, app_meta) %>% 
             rowwise %>%
             mutate(OPALG = OPAL(n = n, pObs = pObs, tao = .29, m =1) * density)) %>% 
             slice_max(OPALG, prop = 0.10, with_ties = FALSE) %>% 
             dplyr::select(-clust, -density, -label, -n, -pObs, -density, -OPALG)
  
  opal_tr <- rbind(al_opal_tr_meta, al_opal)
  
  #Ceil (only for saving to RDS, not called in training)
  cl_tr <- rbind(rej_meta, rej)

  #RF train set-up
  list_df <- list(acc,                             #1 Base
                  rand   =  rbind(acc, ra_tr),     #2 Random
                  al_us  =  rbind(acc, al_tr),     #3 US AL
                  al_qbc =  rbind(acc, al_qbc_tr), #4 QBC AL
                  al_opal = rbind(acc, opal_tr),   #5 OPAL AL
                  ri_hco =  rbind(acc, hco),       #6 HCO RI
                  ri_par =  rbind(acc, parc),      #7 PAR RI
                  app)                             #8 Ceiling
  
  target <- list() 
  
  for(i in 1:length(list_df)) { 
    target[i] <- list(list_df[[i]]$BAD) 
  } 
  
  list_df <- lapply(list_df, function(x) {
    subset(x[,c(1:17)])
  })
  
  #Do training in parallel:
  cl <- makeCluster(3)
  registerDoParallel(cl)
  environment(ctrl) <- .GlobalEnv
  environNames <- c("ctrl")
  clusterExport(cl, environNames, envir=.GlobalEnv )
  rf <- clusterMap(cl = cl, function(x,y) caret::train(x, y, method = "rf", ntree = 600, trControl = ctrl, metric = "AUC", tuneGrid = data.frame(mtry=4)), x = list_df, y = target)
  
  #Print training data.
  p <- c( "No. Base Train" =   nrow(rf[[1]]$trainingData), 
          "No. Rand Train" =   nrow(rf[[2]]$trainingData), 
          "No. US Train" =     nrow(rf[[3]]$trainingData), 
          "No. QBC Train" =    nrow(rf[[4]]$trainingData),
          "No. OPAL Train" =   nrow(rf[[5]]$trainingData),
          "No. HCO RI Train" = nrow(rf[[6]]$trainingData), 
          "No. PAR RI Train" = nrow(rf[[7]]$trainingData),
          "No. Ceil Train" =   nrow(rf[[8]]$trainingData))
  print(p)
  
  #Predict.
  base_pred <- cbind("actual" = holdout$BAD, "base_pred" = predict(rf[[1]]$finalModel, newdata = holdout, type = "prob")[,2]) #1 Base
  rand_pred <- cbind("actual" = holdout$BAD, "rand_pred" = predict(rf[[2]]$finalModel, newdata = holdout, type = "prob")[,2]) #2 Rand
  al_pred  <-  cbind("actual" = holdout$BAD,   "al_pred" = predict(rf[[3]]$finalModel, newdata = holdout, type = "prob")[,2]) #3 AL US
  qbc_pred <-  cbind("actual" = holdout$BAD, "qbc_pred" =  predict(rf[[4]]$finalModel, newdata = holdout, type = "prob")[,2]) #4 AL QBC
  opal_pred <- cbind("actual" = holdout$BAD, "opal_pred" = predict(rf[[5]]$finalModel, newdata = holdout, type = "prob")[,2]) #5 AL OPAL
  hco_pred  <- cbind("actual" = holdout$BAD, "hco_pred" =  predict(rf[[6]]$finalModel, newdata = holdout, type = "prob")[,2]) #6 HCO
  par_pred  <- cbind("actual" = holdout$BAD, "par_pred" =  predict(rf[[7]]$finalModel, newdata = holdout, type = "prob")[,2]) #7 PAR
  ceil_pred <- cbind("actual" = holdout$BAD, "ceil_pred" = predict(rf[[8]]$finalModel, newdata = holdout, type = "prob")[,2]) #8 Ceil
  
  #List I want to return
  pred = list(base_pred, rand_pred, al_pred, qbc_pred, opal_pred, hco_pred, par_pred, ceil_pred)
  base_model = rf[[1]]$finalModel
  tr_oracle <- list(ra_tr$BAD, al_tr$BAD, al_qbc_tr$BAD, opal_tr$BAD, cl_tr$BAD)
  return_list <- list(pred,       #f1: Return to save RDS of predictions on holdout.
                      base_model, #f2: Return base model to make predictions on next applicant batch.
                      tr_oracle,  #f3: Return to save RDS of training augmentation costs.
                      ra_tr,      #f4: Return for meta accumulating df Random.
                      al_tr,      #f5: Return for meta accumulating df AL US.
                      al_qbc_tr,  #f6: Return for meta accumulating df of QBC US.
                      opal_tr,    #f7: Return for meta accumulating df AL OPAL.
                      cl_tr)      #f8: Return for meta accumulating df Ceiling (only for RDS, not called in training)
 
   on.exit(stopCluster(cl))
  
  return(return_list)
}

###################################
#                                 #
#  3. RUN THE LOOP                #  
#                                 #
###################################

loop <- for(i in 1:length(rapp_list)){
  
  #Train, predict holdout, save RDS files, and return meta dataframes.
  print(i)
  f <- first_func(acc, rej, app, hco, parc)
  
  saveRDS(f[[1]],file = paste(names[i],'.RDS'),compress=TRUE) #Save predictions for each model at each iteration.
  saveRDS(f[[3]],file = paste(names[i],'tr_oracle.RDS'),compress=TRUE) #Save selected instances to calculate cost later.
  
  ra_tr_meta <- f[[4]]
  al_us_tr_meta <- f[[5]]
  al_qbc_tr_meta <- f[[6]]
  al_opal_tr_meta <- f[[7]]
  rej_meta <- f[[8]]
  
  #Use base model to predict on next app batch, accept / reject.
  rapp_list[[i]] <- cbind(rapp_list[[i]], rf = predict(f[[2]], newdata = rapp_list[[i]], type="prob")[,2]) 
  racc <- rapp_list[[i]] %>% slice_min(rf, prop = 0.2,  with_ties = FALSE)
  rej <- dplyr::anti_join(rapp_list[[i]], racc)
  
  #Clean, and return those which accumulate.
  racc$rf <- NULL
  rej$rf <- NULL
  rapp_list[[i]]$rf <- NULL
  acc <- rbind(acc,racc)
  app <- rbind(app,rapp_list[[i]])
  
  #Perform RI HCO using all available accepts to train model, infer outcomes of rejects.
  rej_ri <- rbind(rej_meta[,-c(18)], rej[,-c(18)]) #new
  rej_ri <- cbind(rej_ri, rf = predict(f[[2]], newdata = rej_ri, type="prob")[,2]) 
  hco <- hco_func(acc, rej_ri)
  hco$rf <- NULL
  
  #Perform RI Parcel using all available accepts to train model, infer outcomes of rejects.
  acc_ri <- cbind(acc, rf = predict(f[[2]], newdata = acc, type="prob")[,2]) 
  parc <- parcel(acc_ri, rej_ri)
  parc$rf <- NULL
}

### Confirm parallel is cleaned up.
stopCluster(cl)
registerDoSEQ()
gc()

### Final Iteration is outside of loop: applicant pool exhausted, but can train models one last time and make predictions on holdout. ###
f <- first_func(acc, rej, app, hco, parc)
saveRDS(f[[1]],file = paste("200",'.RDS'),compress=TRUE) 
saveRDS(f[[3]],file = paste("200",'.tr_oracle.RDS'),compress=TRUE) 
