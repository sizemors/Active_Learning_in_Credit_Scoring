#' @title Evaluate Experiment
#' 
#' @description  Calculates evaluation metrics defined in paper.  
#' 
#' @details 
#' After loop is completed, move numbered (1-200).RDS files to a folder "pred" and numbered (1-200 tr_oracle).RDS files to a folder "oracle".
#' Functions are set for size of LC holdout set. To run this script on DGP results, do CTRL+F "^^" and follow instructions. 
#' 

###################################
#                                 #
#  1. LOAD PRED RDS FILES         #  
#                                 #
###################################
library("ggplot2")
library("reshape2")
library("rpart") 
library("caret")
library("hmeasure")
library("dplyr")
library("gtools")
library("pracma")
library("directlabels")
library("scmamp")

### 1. Load RDS and create dataframes of predictions per model.
files <- list.files(path = "/Users/samantha.sizemore/Desktop/out_lc/pred", pattern = ".RDS$", full.names = TRUE)
files <- mixedsort(files)
print(files) #Sanity check order is correct.
pred <- lapply(files, readRDS) #Loop over files to read prediction sets.

# Sanity Check: 
# View(pred[[3]][[8]]) #This is 3rd iteration, ceiling. #First # is the iteration, second # is the model where:
# 1 = Base, 
# 2 = Random, 
# 3 = US AL, 
# 4 = QBC AL,
# 5 = OPAL AL, 
# 6 = HCO RI,
# 7 = PAR RI,
# 8 = Ceiling

### 1 Base
list_base <-  lapply(pred, function(x) x[[1]] [ ,2]) #loop over x which is the iteration, 1 = base, ,2= second column with predictions.
base_pred <-  as.data.frame(cbind("actual" = pred[[1]][[1]][ ,"actual"], do.call(cbind, list_base)))

### 2 Rand
list_rand <-  lapply(pred, function(x) x[[2]] [ ,2]) #loop over x which is the iteration, 2 = rand, ,2= second column with predictions.
rand_pred <-  as.data.frame(cbind("actual" = pred[[1]][[1]][ ,"actual"], do.call(cbind, list_rand)))

### 3 US AL
list_al   <-  lapply(pred, function(x) x[[3]] [ ,2]) #loop over x which is the iteration, 3 = us al, ,2= second column with predictions.
al_pred   <-  as.data.frame(cbind("actual" = pred[[1]][[1]][ ,"actual"], do.call(cbind, list_al)))

### 4 QBC AL
list_qbc   <-  lapply(pred, function(x) x[[4]] [ ,2]) #loop over x which is the iteration, 4 = qbc al, ,2= second column with predictions.
qbc_pred   <-  as.data.frame(cbind("actual" = pred[[1]][[1]][ ,"actual"], do.call(cbind, list_qbc)))

### 5 OPAL AL
list_opal   <-  lapply(pred, function(x) x[[5]] [ ,2]) #loop over x which is the iteration, 5 = opal al, ,2= second column with predictions.
opal_pred   <-  as.data.frame(cbind("actual" = pred[[1]][[1]][ ,"actual"], do.call(cbind, list_opal)))

### 6 HCO RI
list_hco <-  lapply(pred, function(x) x[[6]] [ ,2]) #loop over x which is the iteration, 6 = hco, ,2= second column with predictions.
hco_pred <-  as.data.frame(cbind("actual" = pred[[1]][[1]][ ,"actual"], do.call(cbind, list_hco)))

### 7 PAR RI
list_parc <-  lapply(pred, function(x) x[[7]] [ ,2]) #loop over x which is the iteration, 7 = parc, ,2= second column with predictions.
parc_pred <-  as.data.frame(cbind("actual" = pred[[1]][[1]][ ,"actual"], do.call(cbind, list_parc)))

### 8 Ceil
list_ceil <-  lapply(pred, function(x) x[[8]] [ ,2]) #loop over x which is the iteration, 8 = ceil, ,2= second column with predictions.
ceil_pred <-  as.data.frame(cbind("actual" = pred[[1]][[1]][ ,"actual"], do.call(cbind, list_ceil)))

###################################
#                                 #
#  2. AUC METRICS                 #  
#                                 #
################################### 

###### 2.1 Base Evaluation
h_base <- HMeasure(true.class = base_pred$actual, scores = base_pred) 
h_base$metrics["AUC"]

####
###### 2.2 Random Evaluation
h_rand <- HMeasure(true.class = rand_pred$actual, scores = rand_pred) 
h_rand$metrics["AUC"]

####
###### 2.3 AL US Evaluation
h_al  <- HMeasure(true.class = al_pred$actual, scores = al_pred) 
h_al$metrics["AUC"]

####
###### 2.4 AL QBC Evaluation
h_qbc  <- HMeasure(true.class = qbc_pred$actual, scores = qbc_pred) 
h_qbc$metrics["AUC"]

####
###### 2.5 AL OPAL Evaluation
h_opal  <- HMeasure(true.class = opal_pred$actual, scores = opal_pred) 
h_opal$metrics["AUC"]

####
###### 2.6 HCO Evaluation
h_hco <- HMeasure(true.class = hco_pred$actual, scores = hco_pred) 
h_hco$metrics["AUC"]

####
###### 2.7 PAR Evaluation
h_parc <- HMeasure(true.class = parc_pred$actual, scores = parc_pred) 
h_parc$metrics["AUC"]

####
###### 2.8 Ceiling Evaluation
h_ceil <- HMeasure(true.class = ceil_pred$actual, scores = ceil_pred) 
h_ceil$metrics["AUC"]

##Note: "Class labels have been switched from (1,2) to (0,1)" - This is OK as 1=good, 2=bad and for confusion matrix measure 
# we want a "case" / positive class to be 1 = bad. This happens because cbind converts to matrix which can only have one class so 
# it converts from factor(character) to numeric 1/2..

######  Prepare dataframe for ggplot.
eval <- cbind(h_base$metrics["AUC"], h_rand$metrics["AUC"],h_al$metrics["AUC"], h_qbc$metrics["AUC"], h_opal$metrics["AUC"], 
              h_hco$metrics["AUC"], h_parc$metrics["AUC"],h_ceil$metrics["AUC"])
eval <- eval[-c(1),]
colnames(eval) <- c("Base", "Random", "US_AL", "QBC_AL", "OPAL_AL", "HCO_RI", "PARC_RI", "Ceiling")

#write.csv(eval, "lc_auc.csv")

eval$Iteration <- 1:nrow(eval)
long <- melt(eval, id="Iteration")  # convert to long format
long$Iteration <- as.numeric(long$Iteration)
summary(long)

###### Plot.
dev.off()
myplot_auc <- ggplot(data=long,
                 aes(x=Iteration, y=value, group = variable, colour = variable)) +
                 geom_line() +
                 xlim(1,200) +
                 ylim(0.500,0.773) +
                 ylab("AUC") +
                 labs(colour="Model") + 
                 scale_color_manual(values=c( "red", "orange", "yellow", "blue", "green2", "violet", "gray", "black")) +
                # theme(legend.position = "none") +
                theme(text=element_text(size=18))
myplot_auc

###### Calculate area under iteration curve, overall and in quartiles.
eval$Iteration <- as.numeric(eval$Iteration)

base_areas_auc <- t(as.matrix(c(
                            trapz(eval$Iteration,eval$Base),
                            trapz(eval[1:50,]$Iteration,eval[1:50,]$Base),
                            trapz(eval[50:100,]$Iteration,eval[50:100,]$Base),
                            trapz(eval[100:150,]$Iteration,eval[100:150,]$Base),
                            trapz(eval[150:200,]$Iteration,eval[150:200,]$Base))))
random_areas_auc <- t(as.matrix(c(
                            trapz(eval$Iteration,eval$Random),
                            trapz(eval[1:50,]$Iteration,eval[1:50,]$Random),
                            trapz(eval[50:100,]$Iteration,eval[50:100,]$Random),
                            trapz(eval[100:150,]$Iteration,eval[100:150,]$Random),
                            trapz(eval[150:200,]$Iteration,eval[150:200,]$Random))))
US_AL_areas_auc <- t(as.matrix(c(
                            trapz(eval$Iteration,eval$US_AL),
                            trapz(eval[1:50,]$Iteration,eval[1:50,]$US_AL),
                            trapz(eval[50:100,]$Iteration,eval[50:100,]$US_AL),
                            trapz(eval[100:150,]$Iteration,eval[100:150,]$US_AL),
                            trapz(eval[150:200,]$Iteration,eval[150:200,]$US_AL))))
QBC_AL_areas_auc <- t(as.matrix(c(
                            trapz(eval$Iteration,eval$QBC_AL),
                            trapz(eval[1:50,]$Iteration,eval[1:50,]$QBC_AL),
                            trapz(eval[50:100,]$Iteration,eval[50:100,]$QBC_AL),
                            trapz(eval[100:150,]$Iteration,eval[100:150,]$QBC_AL),
                            trapz(eval[150:200,]$Iteration,eval[150:200,]$QBC_AL))))
OPAL_AL_areas_auc <- t(as.matrix(c(
                            trapz(eval$Iteration,eval$OPAL_AL),
                            trapz(eval[1:50,]$Iteration,eval[1:50,]$OPAL_AL),
                            trapz(eval[50:100,]$Iteration,eval[50:100,]$OPAL_AL),
                            trapz(eval[100:150,]$Iteration,eval[100:150,]$OPAL_AL),
                            trapz(eval[150:200,]$Iteration,eval[150:200,]$OPAL_AL))))
HCO_RI_areas_auc <- t(as.matrix(c(
                            trapz(eval$Iteration,eval$HCO_RI),
                            trapz(eval[1:50,]$Iteration,eval[1:50,]$HCO_RI),
                            trapz(eval[50:100,]$Iteration,eval[50:100,]$HCO_RI),
                            trapz(eval[100:150,]$Iteration,eval[100:150,]$HCO_RI),
                            trapz(eval[150:200,]$Iteration,eval[150:200,]$HCO_RI))))
PARC_RI_areas_auc <- t(as.matrix(c(
                            trapz(eval$Iteration,eval$PARC_RI),
                            trapz(eval[1:50,]$Iteration,eval[1:50,]$PARC_RI),
                            trapz(eval[50:100,]$Iteration,eval[50:100,]$PARC_RI),
                            trapz(eval[100:150,]$Iteration,eval[100:150,]$PARC_RI),
                            trapz(eval[150:200,]$Iteration,eval[150:200,]$PARC_RI))))
ceiling_areas_auc <- t(as.matrix(c(
                            trapz(eval$Iteration,eval$Ceiling),
                            trapz(eval[1:50,]$Iteration,eval[1:50,]$Ceiling),
                            trapz(eval[50:100,]$Iteration,eval[50:100,]$Ceiling),
                            trapz(eval[100:150,]$Iteration,eval[100:150,]$Ceiling),
                            trapz(eval[150:200,]$Iteration,eval[150:200,]$Ceiling))))

areas_auc <- rbind(base_areas_auc, random_areas_auc,  US_AL_areas_auc,  QBC_AL_areas_auc, OPAL_AL_areas_auc,  HCO_RI_areas_auc, 
                   PARC_RI_areas_auc, ceiling_areas_auc)
rownames(areas_auc) = c("Base", "Random", "US AL", "QBC AL", "OPAL AL", "HCO RI", "PARC RI", "Ceiling")
colnames(areas_auc) = c("i 1-200", "i 1-50", "i 50-100","i 100-150","i 150-200")

#write.csv(areas_auc, "lc_areas_auc.csv")

###################################
#                                 #
#  3. BS METRICS                  #  
#                                 #
###################################

###### 3.1 Define:
BrierScore <- function(y, yhat){
  sum((y-yhat)^2)/length(y)
}

#Create duplicates of dataframes.
base_pred_bs <- base_pred
rand_pred_bs <- rand_pred
al_pred_bs   <- al_pred
qbc_pred_bs <- qbc_pred
opal_pred_bs <- opal_pred
hco_pred_bs <- hco_pred
parc_pred_bs <- parc_pred
ceil_pred_bs <- ceil_pred

#Convert from 1/2 to 0/1.
base_pred_bs$actual <- ifelse(base_pred_bs$actual == 2, 1, 0)
rand_pred_bs$actual <- ifelse(rand_pred_bs$actual == 2, 1, 0)
al_pred_bs$actual   <- ifelse(al_pred_bs$actual == 2, 1, 0)
qbc_pred_bs$actual <- ifelse(qbc_pred_bs$actual == 2, 1, 0)
opal_pred_bs$actual <- ifelse(opal_pred_bs$actual == 2, 1, 0)
hco_pred_bs$actual <- ifelse(hco_pred_bs$actual == 2, 1, 0)
parc_pred_bs$actual <- ifelse(parc_pred_bs$actual == 2, 1, 0)
ceil_pred_bs$actual <- ifelse(ceil_pred_bs$actual == 2, 1, 0)

#Create dataframe of BS for each iteration for each model.
base_bs <- as.data.frame(sapply(base_pred_bs, BrierScore, y=base_pred_bs$actual))
rand_bs <- as.data.frame(sapply(rand_pred_bs, BrierScore, y=rand_pred_bs$actual))
al_bs   <- as.data.frame(sapply(al_pred_bs, BrierScore, y=al_pred_bs$actual))
qbc_bs   <- as.data.frame(sapply(qbc_pred_bs, BrierScore, y=qbc_pred_bs$actual))
opal_bs   <- as.data.frame(sapply(opal_pred_bs, BrierScore, y=opal_pred_bs$actual))
hco_bs   <- as.data.frame(sapply(hco_pred_bs, BrierScore, y=hco_pred_bs$actual))
parc_bs   <- as.data.frame(sapply(parc_pred_bs, BrierScore, y=parc_pred_bs$actual))
ceil_bs <- as.data.frame(sapply(ceil_pred_bs, BrierScore, y=ceil_pred_bs$actual))

###### 3.2 Prepare dataframe for ggplot:
eval_bs <- cbind(base_bs, rand_bs, al_bs, qbc_bs, opal_bs, hco_bs, parc_bs, ceil_bs)
eval_bs <- eval_bs[-c(1),]
eval_bs$Iteration <- 1:nrow(eval_bs)
colnames(eval_bs) <- c("Base", "Random", "US_AL", "QBC_AL", "OPAL_AL", "HCO_RI", "PARC_RI", "Ceiling", "Iteration")
#write.csv(eval_bs, "lc_bs.csv")

long_bs <- melt(eval_bs, id="Iteration")  # convert to long format
long_bs$Iteration <- as.numeric(long_bs$Iteration)
summary(long_bs)

###### 3.3 BS Plot:
# myplot_bs <- ggplot(data=long_bs,
#                     aes(x=Iteration, y=value, group = variable, colour = variable)) +
#                     geom_line() +
#                     xlim(1,200) +
#                     ylim(0.0652,.3343) +
#                     ylab("Brier Score") +
#                     labs(colour="Model") + 
#   scale_color_manual(values=c( "red", "orange", "yellow", "blue", "green2", "violet", "gray", "black")) +
#   theme(legend.text=element_text(size=8))
# myplot_bs


###### 3.4 BS Area under Curve:  
eval_bs$Iteration <- as.numeric(eval_bs$Iteration)

base_areas_bs <- t(as.matrix(c(
                    trapz(eval_bs$Iteration,eval_bs$Base),
                    trapz(eval_bs[1:50,]$Iteration,eval_bs[1:50,]$Base),
                    trapz(eval_bs[50:100,]$Iteration,eval_bs[50:100,]$Base),
                    trapz(eval_bs[100:150,]$Iteration,eval_bs[100:150,]$Base),
                    trapz(eval_bs[150:200,]$Iteration,eval_bs[150:200,]$Base))))

random_areas_bs <- t(as.matrix(c(
                    trapz(eval_bs$Iteration,eval_bs$Random),
                    trapz(eval_bs[1:50,]$Iteration,eval_bs[1:50,]$Random),
                    trapz(eval_bs[50:100,]$Iteration,eval_bs[50:100,]$Random),
                    trapz(eval_bs[100:150,]$Iteration,eval_bs[100:150,]$Random),
                    trapz(eval_bs[150:200,]$Iteration,eval_bs[150:200,]$Random))))

US_AL_areas_bs <- t(as.matrix(c(
                    trapz(eval_bs$Iteration,eval_bs$US_AL),
                    trapz(eval_bs[1:50,]$Iteration,eval_bs[1:50,]$US_AL),
                    trapz(eval_bs[50:100,]$Iteration,eval_bs[50:100,]$US_AL),
                    trapz(eval_bs[100:150,]$Iteration,eval_bs[100:150,]$US_AL),
                    trapz(eval_bs[150:200,]$Iteration,eval_bs[150:200,]$US_AL))))

QBC_AL_areas_bs <- t(as.matrix(c(
                    trapz(eval_bs$Iteration,eval_bs$QBC_AL),
                    trapz(eval_bs[1:50,]$Iteration,eval_bs[1:50,]$QBC_AL),
                    trapz(eval_bs[50:100,]$Iteration,eval_bs[50:100,]$QBC_AL),
                    trapz(eval_bs[100:150,]$Iteration,eval_bs[100:150,]$QBC_AL),
                    trapz(eval_bs[150:200,]$Iteration,eval_bs[150:200,]$QBC_AL))))

OPAL_AL_areas_bs <- t(as.matrix(c(
                    trapz(eval_bs$Iteration,eval_bs$OPAL_AL),
                    trapz(eval_bs[1:50,]$Iteration,eval_bs[1:50,]$OPAL_AL),
                    trapz(eval_bs[50:100,]$Iteration,eval_bs[50:100,]$OPAL_AL),
                    trapz(eval_bs[100:150,]$Iteration,eval_bs[100:150,]$OPAL_AL),
                    trapz(eval_bs[150:200,]$Iteration,eval_bs[150:200,]$OPAL_AL))))

HCO_RI_areas_bs <- t(as.matrix(c(
                    trapz(eval_bs$Iteration,eval_bs$HCO_RI),
                    trapz(eval_bs[1:50,]$Iteration,eval_bs[1:50,]$HCO_RI),
                    trapz(eval_bs[50:100,]$Iteration,eval_bs[50:100,]$HCO_RI),
                    trapz(eval_bs[100:150,]$Iteration,eval_bs[100:150,]$HCO_RI),
                    trapz(eval_bs[150:200,]$Iteration,eval_bs[150:200,]$HCO_RI))))

PARC_RI_areas_bs <- t(as.matrix(c(
                    trapz(eval_bs$Iteration,eval_bs$PARC_RI),
                    trapz(eval_bs[1:50,]$Iteration,eval_bs[1:50,]$PARC_RI),
                    trapz(eval_bs[50:100,]$Iteration,eval_bs[50:100,]$PARC_RI),
                    trapz(eval_bs[100:150,]$Iteration,eval_bs[100:150,]$PARC_RI),
                    trapz(eval_bs[150:200,]$Iteration,eval_bs[150:200,]$PARC_RI))))

ceiling_areas_bs <- t(as.matrix(c(
                    trapz(eval_bs$Iteration,eval_bs$Ceiling),
                    trapz(eval_bs[1:50,]$Iteration,eval_bs[1:50,]$Ceiling),
                    trapz(eval_bs[50:100,]$Iteration,eval_bs[50:100,]$Ceiling),
                    trapz(eval_bs[100:150,]$Iteration,eval_bs[100:150,]$Ceiling),
                    trapz(eval_bs[150:200,]$Iteration,eval_bs[150:200,]$Ceiling))))

areas_bs <- rbind(base_areas_bs, random_areas_bs,  US_AL_areas_bs,  QBC_AL_areas_bs, OPAL_AL_areas_bs,  HCO_RI_areas_bs, 
                   PARC_RI_areas_bs, ceiling_areas_bs)
rownames(areas_bs) = c("Base", "Random", "US AL", "QBC AL", "OPAL AL", "HCO RI", "PARC RI", "Ceiling")
colnames(areas_bs) = c("i 1-200", "i 1-50", "i 50-100","i 100-150","i 150-200")

#write.csv(areas_bs, "lc_areas_bs.csv")

###################################
#                                 #
#  4. BRAA METRICS                #  
#                                 #
###################################
######  BRA for Base
bra <- list()
br <- list()
for(i in 2:length(base_pred)) {
      br[i] <- base_pred[,c(1,i)] %>%
                           slice_min(base_pred[,i], prop = 0.20, with_ties = FALSE) %>% 
                           count(actual == "2") %>%
                           summarise(n) / (nrow(base_pred)*.2)
     bra[i] <- br[[i]][2]
}

bra[[1]] <- NULL
bra<- data.frame(matrix(unlist(bra), nrow=length(bra), byrow=T))
colnames(bra) <- c("Base")
bra <- transform(bra, Base=Base * 100)

######  BRA for Rand
rra <- list()
rr <- list()
for(i in 2:length(rand_pred)) {
  rr[i] <- rand_pred[,c(1,i)] %>%
    slice_min(rand_pred[,i], prop = 0.20, with_ties = FALSE) %>% 
    count(actual == "2") %>%
    summarise(n) / (nrow(rand_pred)*.2)
  rra[i] <- rr[[i]][2]
}
rra[[1]] <- NULL
rra<- data.frame(matrix(unlist(rra), nrow=length(rra), byrow=T))
colnames(rra) <- c("Random")
rra <- transform(rra, Random=Random * 100)

######  BRA for US AL
ara <- list()
ar <- list()
for(i in 2:length(al_pred)) {
  ar[i] <- al_pred[,c(1,i)] %>%
    slice_min(al_pred[,i], prop = 0.20, with_ties = FALSE) %>% 
    count(actual == "2") %>%
    summarise(n) / (nrow(al_pred)*.2)
  ara[i] <- ar[[i]][2]
}
ara[[1]] <- NULL
ara<- data.frame(matrix(unlist(ara), nrow=length(ara), byrow=T))
colnames(ara) <- c("US_AL")
ara <- transform(ara, US_AL = US_AL * 100)

######  BRA for US QBC
qra <- list()
qr <- list()
for(i in 2:length(qbc_pred)) {
  qr[i] <- qbc_pred[,c(1,i)] %>%
    slice_min(qbc_pred[,i], prop = 0.20, with_ties = FALSE) %>% 
    count(actual == "2") %>%
    summarise(n) / (nrow(qbc_pred)*.2)
  qra[i] <- qr[[i]][2]
}
qra[[1]] <- NULL
qra<- data.frame(matrix(unlist(qra), nrow=length(qra), byrow=T))
colnames(qra) <- c("QBC_AL")
qra <- transform(qra, QBC_AL = QBC_AL * 100)

######  BRA for US OPAL
ora <- list()
or <- list()
for(i in 2:length(opal_pred)) {
  or[i] <- opal_pred[,c(1,i)] %>%
    slice_min(opal_pred[,i], prop = 0.20, with_ties = FALSE) %>% 
    count(actual == "2") %>%
    summarise(n) / (nrow(opal_pred)*.2)
  ora[i] <- or[[i]][2]
}
ora[[1]] <- NULL
ora<- data.frame(matrix(unlist(ora), nrow=length(ora), byrow=T))
colnames(ora) <- c("OPAL_AL")
ora <- transform(ora, OPAL_AL = OPAL_AL * 100)

######  BRA for HCO RI
hra <- list()
hr <- list()
for(i in 2:length(hco_pred)) {
  hr[i] <- hco_pred[,c(1,i)] %>%
    slice_min(hco_pred[,i], prop = 0.20, with_ties = FALSE) %>% 
    count(actual == "2") %>%
    summarise(n) / (nrow(hco_pred)*.2)
  hra[i] <- hr[[i]][2]
}
hra[[1]] <- NULL
hra<- data.frame(matrix(unlist(hra), nrow=length(hra), byrow=T))
colnames(hra) <- c("HCO_RI")
hra <- transform(hra, HCO_RI = HCO_RI * 100)

######  BRA for PARC RI
pra <- list()
pr <- list()
for(i in 2:length(parc_pred)) {
  pr[i] <- parc_pred[,c(1,i)] %>%
    slice_min(parc_pred[,i], prop = 0.20, with_ties = FALSE) %>%
    count(actual == "2") %>%
    summarise(n) / (nrow(parc_pred)*.2)
  pra[i] <- pr[[i]][2]
}
pra[[1]] <- NULL
pra<- data.frame(matrix(unlist(pra), nrow=length(pra), byrow=T))
colnames(pra) <- c("PARC_RI")
pra <- transform(pra, PARC_RI = PARC_RI * 100)

###### 4.4 BRA for Ceil
cra <- list()
cr <- list()
for(i in 2:length(ceil_pred)) {
  cr[i] <- ceil_pred[,c(1,i)] %>%
    slice_min(ceil_pred[,i], prop = 0.20, with_ties = FALSE) %>% 
    count(actual == "2") %>%
    summarise(n) / (nrow(ceil_pred)*.2)
  cra[i] <- cr[[i]][2]
}
cra[[1]] <- NULL
cra<- data.frame(matrix(unlist(cra), nrow=length(cra), byrow=T))
colnames(cra) <- c("Ceiling")
cra <- transform(cra, Ceiling=Ceiling * 100)

###### 4.5 Prepare dataframe for ggplot:
eval_braa <- cbind(bra, rra, ara, qra, ora, hra, pra, cra)
eval_braa$Iteration <- 1:nrow(eval_braa)
#write.csv(eval_braa, "lc_braa.csv")

###### 4.7 BRAA Plot:
# long_braa <- melt(eval_braa, id="Iteration")  # convert to long format
# long_braa$Iteration <- as.numeric(long_braa$Iteration)
# summary(long_braa)
# 
# myplot_braa <- ggplot(data=long_braa,
#                     aes(x=Iteration, y=value, group = variable, colour = variable)) +
#                     geom_line() +
#                     xlim(1,200) +
#                     ylim(1.6,9.1) +
#                     ylab("Bad Rate Amongst Accepts (%)") +
#                     labs(colour="Model")  + 
#   scale_color_manual(values=c( "red", "orange", "yellow", "blue", "green2", "violet", "gray", "black")) +
#   theme(legend.text=element_text(size=8))
# myplot_braa 

###### 4.8 BRAA Area under Curve:
eval_braa$Iteration <- as.numeric(eval_braa$Iteration)

base_areas_braa <- t(as.matrix(c(
                    trapz(eval_braa$Iteration,eval_braa$Base),
                    trapz(eval_braa[1:50,]$Iteration,eval_braa[1:50,]$Base),
                    trapz(eval_braa[50:100,]$Iteration,eval_braa[50:100,]$Base),
                    trapz(eval_braa[100:150,]$Iteration,eval_braa[100:150,]$Base),
                    trapz(eval_braa[150:200,]$Iteration,eval_braa[150:200,]$Base))))

random_areas_braa <- t(as.matrix(c(
                    trapz(eval_braa$Iteration,eval_braa$Random),
                    trapz(eval_braa[1:50,]$Iteration,eval_braa[1:50,]$Random),
                    trapz(eval_braa[50:100,]$Iteration,eval_braa[50:100,]$Random),
                    trapz(eval_braa[100:150,]$Iteration,eval_braa[100:150,]$Random),
                    trapz(eval_braa[150:200,]$Iteration,eval_braa[150:200,]$Random))))

US_AL_areas_braa <- t(as.matrix(c(
                    trapz(eval_braa$Iteration,eval_braa$US_AL),
                    trapz(eval_braa[1:50,]$Iteration,eval_braa[1:50,]$US_AL),
                    trapz(eval_braa[50:100,]$Iteration,eval_braa[50:100,]$US_AL),
                    trapz(eval_braa[100:150,]$Iteration,eval_braa[100:150,]$US_AL),
                    trapz(eval_braa[150:200,]$Iteration,eval_braa[150:200,]$US_AL))))

QBC_AL_areas_braa <- t(as.matrix(c(
                    trapz(eval_braa$Iteration,eval_braa$QBC_AL),
                    trapz(eval_braa[1:50,]$Iteration,eval_braa[1:50,]$QBC_AL),
                    trapz(eval_braa[50:100,]$Iteration,eval_braa[50:100,]$QBC_AL),
                    trapz(eval_braa[100:150,]$Iteration,eval_braa[100:150,]$QBC_AL),
                    trapz(eval_braa[150:200,]$Iteration,eval_braa[150:200,]$QBC_AL))))

OPAL_AL_areas_braa <- t(as.matrix(c(
                    trapz(eval_braa$Iteration,eval_braa$OPAL_AL),
                    trapz(eval_braa[1:50,]$Iteration,eval_braa[1:50,]$OPAL_AL),
                    trapz(eval_braa[50:100,]$Iteration,eval_braa[50:100,]$OPAL_AL),
                    trapz(eval_braa[100:150,]$Iteration,eval_braa[100:150,]$OPAL_AL),
                    trapz(eval_braa[150:200,]$Iteration,eval_braa[150:200,]$OPAL_AL))))

HCO_RI_areas_braa <- t(as.matrix(c(
                    trapz(eval_braa$Iteration,eval_braa$HCO_RI),
                    trapz(eval_braa[1:50,]$Iteration,eval_braa[1:50,]$HCO_RI),
                    trapz(eval_braa[50:100,]$Iteration,eval_braa[50:100,]$HCO_RI),
                    trapz(eval_braa[100:150,]$Iteration,eval_braa[100:150,]$HCO_RI),
                    trapz(eval_braa[150:200,]$Iteration,eval_braa[150:200,]$HCO_RI))))

PARC_RI_areas_braa <- t(as.matrix(c(
                    trapz(eval_braa$Iteration,eval_braa$PARC_RI),
                    trapz(eval_braa[1:50,]$Iteration,eval_braa[1:50,]$PARC_RI),
                    trapz(eval_braa[50:100,]$Iteration,eval_braa[50:100,]$PARC_RI),
                    trapz(eval_braa[100:150,]$Iteration,eval_braa[100:150,]$PARC_RI),
                    trapz(eval_braa[150:200,]$Iteration,eval_braa[150:200,]$PARC_RI))))

ceiling_areas_braa <- t(as.matrix(c(
                    trapz(eval_braa$Iteration,eval_braa$Ceiling),
                    trapz(eval_braa[1:50,]$Iteration,eval_braa[1:50,]$Ceiling),
                    trapz(eval_braa[50:100,]$Iteration,eval_braa[50:100,]$Ceiling),
                    trapz(eval_braa[100:150,]$Iteration,eval_braa[100:150,]$Ceiling),
                    trapz(eval_braa[150:200,]$Iteration,eval_braa[150:200,]$Ceiling))))

areas_braa <- rbind(base_areas_braa, random_areas_braa,  US_AL_areas_braa,  QBC_AL_areas_braa, OPAL_AL_areas_braa,  HCO_RI_areas_braa, 
                  PARC_RI_areas_braa, ceiling_areas_braa)
rownames(areas_braa) = c("Base", "Random", "US AL", "QBC AL", "OPAL AL", "HCO RI", "PARC RI", "Ceiling")
colnames(areas_braa) = c("i 1-200", "i 1-50", "i 50-100","i 100-150","i 150-200")

#write.csv(areas_braa, "lc_areas_braa.csv")

###################################
#                                 #
#  6. LOAD ORACLE FILES           #  
#                                 #
###################################

###  Load files of Training Augmentation
files_tr <- list.files(path = "/Users/samantha.sizemore/Desktop/out_lc/oracle", pattern = ".RDS$", full.names = TRUE)
files_tr <- mixedsort(files_tr)
print(files_tr) #Sanity check order
tr_aug <- lapply(files_tr, readRDS)

#Sanity Check:
View(tr_aug[[1]][[5]]) #First is the iteration, second is the sample method where 1=rand, 2=al us, 3= al qbc, 4= opal, 5 = ceil)

###################################
#                                 #
#  7. TRAIN AUG COST              #  
#                                 #
###################################
### Calculate cost of training augmentation
##1: Random: Count number of BADs (cumulative) over iterations > convert to profit measure.
rand_aug <- lapply(lapply(tr_aug, function(x) x[[1]]), function(x) rbind(sum(ifelse(x == "BAD", 1, 0)), length(x))) #Creates one row with # Bads, one row with total # train - second row should match aug acc (10% rej).
rand_aug <- as.data.frame(do.call(cbind, rand_aug))
rand_aug[3,] <-  rand_aug[1,]*-2704 #Row 3 = Cost, FN = -2704 defined in other script.
rand_aug[4,] <- (rand_aug[2,] - rand_aug[1,]) * 1128 #Row 4 = Benefit, TN = 1128 defined in other script.
rand_aug[5,] <-  rand_aug[3,] + rand_aug[4,] #Row 5 = Sum

##2: AL US: ""
al_aug <- lapply(lapply(tr_aug, function(x) x[[2]]), function(x) rbind(sum(ifelse(x == "BAD", 1, 0)), length(x)))
al_aug <- as.data.frame(do.call(cbind, al_aug))
al_aug[3,] <-  al_aug[1,]*-2704
al_aug[4,] <- (al_aug[2,] - al_aug[1,]) * 1128
al_aug[5,] <-  al_aug[3,] + al_aug[4,]

#3: AL QBC: ""
qbc_aug <- lapply(lapply(tr_aug, function(x) x[[3]]), function(x) rbind(sum(ifelse(x == "BAD", 1, 0)), length(x)))
qbc_aug <- as.data.frame(do.call(cbind, qbc_aug))
qbc_aug[3,] <-  qbc_aug[1,]*-2704
qbc_aug[4,] <- (qbc_aug[2,] - qbc_aug[1,]) * 1128
qbc_aug[5,] <-  qbc_aug[3,] + qbc_aug[4,]

#4: AL OPAL: ""
opal_aug <- lapply(lapply(tr_aug, function(x) x[[4]]), function(x) rbind(sum(ifelse(x == "BAD", 1, 0)), length(x)))
opal_aug <- as.data.frame(do.call(cbind, opal_aug))
opal_aug[3,] <-  opal_aug[1,]*-2704
opal_aug[4,] <- (opal_aug[2,] - opal_aug[1,]) * 1128
opal_aug[5,] <-  opal_aug[3,] + opal_aug[4,]

#5: CEIL: ""
ceil_aug <- lapply(lapply(tr_aug, function(x) x[[5]]), function(x) rbind(sum(ifelse(x == "BAD", 1, 0)), length(x)))
ceil_aug <- as.data.frame(do.call(cbind, ceil_aug))
ceil_aug[3,] <-  ceil_aug[1,]*-2704
ceil_aug[4,] <- (ceil_aug[2,] - ceil_aug[1,]) * 1128
ceil_aug[5,] <-  ceil_aug[3,] + ceil_aug[4,]

#Cumulative Bads in Training Augmentation
aug <- cbind(transpose(rand_aug[1,]), transpose(al_aug[1,]), transpose(qbc_aug[1,]), transpose(opal_aug[1,]), transpose(ceil_aug[1,]))
colnames(aug) = c("Random", "AL_US", "AL_QBC", "AL_OPAL", "Ceiling")
aug$Iteration <- rownames(aug)

#Plot
# long_aug<- melt(aug, id="Iteration")  
# long_aug$Iteration <- as.numeric(long_aug$Iteration)
# summary(long_aug)
# myplot_aug <- ggplot(data=long_aug,
#                      aes(x=Iteration, y=value, group = variable, colour = variable)) +
#                       geom_line() +
#                       xlim(1,200) +
#                       ylim(0,5690) +
#                       ylab("Cumulative Bads in Training Augmentation") +
#                       labs(colour="Model")  + 
#                       scale_color_manual(values=c( "orange", "yellow", "blue", "green2","black")) +
#   theme(legend.text=element_text(size=8))
# 
# myplot_aug
# write.csv(aug, "aug.csv")

#Above is cumulative. Find # Bads sampled at each iteration
aug_lag <- aug %>%  
  mutate(diff_rand = Random - lag(Random)) %>% 
  mutate(diff_al = AL_US - lag(AL_US)) %>% 
  mutate(diff_qbc = AL_QBC - lag(AL_QBC)) %>% 
  mutate(diff_opal = AL_OPAL - lag(AL_OPAL)) %>% 
  mutate(diff_ceil = Ceiling - lag(Ceiling))

aug_lag[1,7] = aug_lag[1,1]
aug_lag[1,8] = aug_lag[1,2]
aug_lag[1,9] = aug_lag[1,3]
aug_lag[1,10] = aug_lag[1,4]
aug_lag[1,11] = aug_lag[1,5]

aug_lag <- aug_lag[, 6:11]
aug_lag_avg <- apply(aug_lag[,2:6], 2, function(x) sum(x)/200)
#write.csv(aug_lag_avg, "lc.bads.csv")

#Averages in quartiles
# quart_rand <- c(mean(aug_lag[1:50, 2]), mean(aug_lag[51:100, 2]), mean(aug_lag[101:150, 2]), mean(aug_lag[151:200, 2]))
# quart_alus <- c(mean(aug_lag[1:50, 3]), mean(aug_lag[51:100, 3]), mean(aug_lag[101:150, 3]), mean(aug_lag[151:200, 3]))
# quart_qbc <-  c(mean(aug_lag[1:50, 4]), mean(aug_lag[51:100, 4]), mean(aug_lag[101:150, 4]), mean(aug_lag[151:200, 4]))
# quart_opal <- c(mean(aug_lag[1:50, 5]), mean(aug_lag[51:100, 5]), mean(aug_lag[101:150, 5]), mean(aug_lag[151:200, 5]))
# quart_ceil <- c(mean(aug_lag[1:50, 6]), mean(aug_lag[51:100, 6]), mean(aug_lag[101:150, 6]), mean(aug_lag[151:200, 6]))
# quart <- as.data.frame(rbind(quart_rand, quart_alus, quart_qbc, quart_opal, quart_ceil))
# write.csv(quart, "lc.quartiles.bads.csv")

###################################
#                                 #
#  8. MODEL PROFIT                #  
#                                 #
###################################
#Consider only action taken (TN, FN - from PROFIT_VARIABLES.R script)
TN =  1128
FN = -2704

#Calculate the profit based on accept lowest 20% risk profile = bad rate in those accepted * FN cost. No. accepted = 2000 for LC, = 1000 for DGP
profit <- eval_braa[,c(1:8)] 
profit <- as.data.frame(sapply(profit, function(x) x/100)) 
profit <- as.data.frame(sapply(profit, function(x) (2000 * x * FN) + (2000 * (1-x)*TN)))  ##^^For DGP, change from 2000 to 1000 (smaller # accepts as smaller holdout).

profit <- as.data.frame(sapply(profit, function(x) (x - profit[ ,1]))) #Relative to Base.
profit_pdi <- as.data.frame(sapply(profit, function(x) x/ (2000*10000))) #Profit per dollar issued. ^^For DGP, change from 2000 to 1000 (smaller # accepts as smaller holdout). Loan amount issued uses avg. value = 10,000
profit_pdi$Iteration <- 1:nrow(profit)

# long_profit_pdi <- melt(profit_pdi, id="Iteration")  # convert to long format
# long_profit_pdi$Iteration <- as.numeric(profit_pdi$Iteration)
# summary(long_profit_pdi)
# myplot_profit_pdi <- ggplot(data=long_profit_pdi,
#                       aes(x=Iteration, y=value, group = variable, colour = variable)) +
#                       geom_line() +
#                       xlim(1,200) +
#                       ylim(-.014,.0075) +
#                       ylab("Profit per $ Issued (Rel. to Base)") +
#                       labs(colour="Model")  + 
#   scale_color_manual(values=c( "red", "orange", "yellow", "blue", "green2", "violet", "gray", "black")) +
#   theme(legend.text=element_text(size=8))
# 
# myplot_profit_pdi

#Model Profit per Dollar Issued, Overall and in Quartiles
quart_profit <- lapply(profit_pdi, function(x) c((sum(x) / 200),(sum(x[1:50]) / 50), (sum(x[50:100]) / 50), (sum(x[100:150]) / 50), (sum(x[150:200]) / 50)))
quart_profit <- do.call(rbind, quart_profit)
#write.csv(quart_profit, "lc_model_profit.csv")

###################################
#                                 #
#  9. AUG COST PER DOLLAR ISSUED  #  
#                                 #
###################################

#Augementation "cost" in per dollar issued terms:
aug_p <- cbind(transpose(rand_aug[5,]), transpose(al_aug[5,]), transpose(qbc_aug[5,]), transpose(opal_aug[5,]), transpose(ceil_aug[5,]))
colnames(aug_p) = c("Random", "AL_US", "AL_QBC", "AL_OPAL", "Ceiling")
aug_p <- as.data.frame(sapply(aug_p, function(x) x/ (10000*2000))) #Aug cost per dollar issued. ^^For DGP, change from 2000 to 1000 (smaller # accepts as smaller holdout).

quart_ac_profit <- lapply(aug_p, function(x) c((sum(x) / 200),(sum(x[1:50]) / 50), (sum(x[50:100]) / 50), (sum(x[100:150]) / 50), (sum(x[150:200]) / 50)))
quart_ac_profit <- do.call(rbind, quart_ac_profit)
#write.csv(quart_ac_profit, "lc_ac_profit.csv")

###################################
#                                 #
# 10. MODEL+AUG PER DOLLAR ISSUED #  
#                                 #
###################################
#Combined (model profit in PPDI and aug profit in PPDI)
Random_pdi <- as.data.frame(profit_pdi$Random + aug_p$Random)
AL_US_pdi <- as.data.frame(profit_pdi$US_AL + aug_p$AL_US)
QBC_US_pdi <- as.data.frame(profit_pdi$QBC_AL + aug_p$AL_QBC)
OPAL_US_pdi <- as.data.frame(profit_pdi$OPAL_AL + aug_p$AL_OPAL)
Ceil_pdi <- as.data.frame(profit_pdi$Ceiling + aug_p$Ceiling)
Base_pdi <- as.data.frame(profit_pdi$Base)
HCO_pdi <- as.data.frame(profit_pdi$HCO)
PARC_pdi <- as.data.frame(profit_pdi$PARC)
PDI <- cbind(Base_pdi, Random_pdi, AL_US_pdi, QBC_US_pdi, OPAL_US_pdi, HCO_pdi, PARC_pdi, Ceil_pdi)
colnames(PDI) =  c("Base", "Random", "AL_US", "AL_QBC", "AL_OPAL", "RI_HCO", "RI_PARC", "Ceiling")
PDI$Iteration <- 1:nrow(PDI)

#In quartiles
quart_ppdi <- lapply(PDI, function(x) c((sum(x)/200), (sum(x[1:50]) / 50), (sum(x[50:100]) / 50), (sum(x[100:150]) / 50), (sum(x[150:200]) / 50)))
quart_ppdi <- do.call(rbind, quart_ppdi)
#write.csv(quart_ppdi, "lc_ppdi_total.csv")
#write.csv(PDI, "ppdi_full.csv")

#Plot with ceiling
# long_pdi <- melt(PDI, id="Iteration")  
# long_pdi$Iteration <- as.numeric(long_pdi$Iteration)
# summary(long_pdi)
# myplot_pdi <- ggplot(data=long_pdi,
#                    aes(x=Iteration, y=value, group = variable, colour = variable)) +
#                   geom_line() +
#                   xlim(1,200) +
#                   ylim(-.014,.6) +
#                   ylab("Profit per $ Issued (Rel. to Base) + Augmentation Cost") +
#                   labs(colour="Model")  + 
#   scale_color_manual(values=c( "red", "orange", "yellow", "blue", "green2", "violet", "gray", "black")) +
#   theme(legend.text=element_text(size=8))
# 
# myplot_pdi



