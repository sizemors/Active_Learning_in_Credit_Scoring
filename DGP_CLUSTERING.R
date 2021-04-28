#' @title DGP Clustering
#' 
#' @description  Uses validation set to find optimal number of clusters. Then pre-clusters training data to prepare OPALgain calulations in loop.
#' 
#' @details 
#' Outputs dataframe of instances in training set that will be fed into loop. This separate dataframe will be used in loop to record if instance
#' is sampled, then pull it's density / label statistics for the OPAL gain function.
#' 
#' Uses k-means intuitition from following source:
#' BADS Tutorials. (2018). Humboldt Universität Information Systems.

###################################
#                                 #
#  1. SPLIT CLEANED DATA          #  
#                                 #
###################################
library("caret")
library("data.table")
library("ggplot2")

data <- dgp #or load below
data <- readRDS('dgp.rds')

### Split 80/10/10 for loop train/validation/holdout. ###
idx<- createDataPartition(data$BAD, p = 0.8, list = FALSE)
train <- data[idx,]
val_hold  <- data[-idx,]

### Split val_hold 50/50 for validation (hyperparameter tuning) and holdout set. ###
idx<- createDataPartition(val_hold$BAD, p = 0.5, list = FALSE)
holdout <- val_hold[idx,]
val  <- val_hold[-idx,]
rm(val_hold)

### Make loop training set a round number (if split resulted in 40001). ###
train <- train[1:40000,] 

###################################
#                                 #
#  2. FIND OPTIMAL K W/ VAL SET   #  
#                                 #
###################################

### Clean validation set so it is only continuous variables and standardized ###
idx_numeric <- sapply(val, is.numeric)
val_numeric <- val[, idx_numeric]
idx_b <- sapply(val_numeric, function(x) length(unique(na.omit(x))) <= 2L)                                                      
val_continuous <- val_numeric[ ,!idx_b]
val_norm <- lapply(val_continuous, function(x) (x-mean(x))/sd(x)  )
val_norm <- data.frame(val_norm)

### Set up and run k-means ###
set.seed(933330)
k.settings = c(seq(2,100,by=2)) 
obj.values = vector(mode="numeric", length = length(k.settings))  
cluster.models = vector(mode = "list", length = length(k.settings))  

for (i in 1:length(k.settings)) {
  print(paste("Starting iteration:", i))
  clu.sol <- kmeans(val_norm, centers=k.settings[i], iter.max = 50, nstart = 5)  
  obj.values[i] <- clu.sol$tot.withinss  
  cluster.models[[i]] <- clu.sol 
}

### View optimal number of clusters ###
qplot(x= k.settings,y=obj.values,geom=c("line","point"),xlab = "k", ylab="Total within-cluster SS",
      main = "Elbow curve for k selection", color="red") + guides(color=FALSE)

df <- data.frame(k.settings, obj.values)
ggplot(df, aes(k.settings,obj.values))+geom_line(color = "red") + geom_point(color="red")  + xlab("k") + ylab("Total within-cluster SS") + ggtitle("Elbow curve for k selection")

### Result: curve becomes linear at around k=24 ###

###################################
#                                 #
#  2. CLUSTER LOOP TRAIN SET      #  
#                                 #
###################################
### Create new filtered train set that is only continuous variables and standardized ###
idx_numeric <- sapply(train, is.numeric)
train_numeric <- train[, idx_numeric]
idx_b <- sapply(train_numeric, function(x) length(unique(na.omit(x))) <= 2L)                                                      
train_continuous <- train_numeric[ ,!idx_b]
train_norm <- lapply(train_continuous, function(x) (x-mean(x))/sd(x)  )
train_norm <- data.frame(train_norm)

### Cluster training set - assign cluster ID to each data row in loop training set. ###
gc()
cluster.object <- kmeans(train_norm, centers = 24, iter.max = 700, nstart = 20, algorithm = "Lloyd")
clusters <- cluster.object$cluster
train_cluster <- cbind(train, "clust"= factor(clusters))

### Calculate density of each cluster and add column for density for each row in loop training set. ###
train_cluster2 <- train_cluster %>%
  group_by(clust) %>%
  mutate(density = length(clust) / 40000)

sum(unique(train_cluster2$density)) #Sanity check: should equal ~1.

### Result: train_cluster2 is a dataframe of all instances in train (applicant batches), with their cluster ID and density estimate.
