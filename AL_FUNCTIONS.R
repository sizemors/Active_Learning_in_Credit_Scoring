#' @title Active Learning Functions (Ramey 2017)
#' 
#' @description  Defines Uncertainty Sampling and QBC algorithms.
#' 
#' @details 
#' This is modified version of functions from deprecated Active Learning R Package:
#' Ramey, J. (2017). Active learning in R Package. Deprecated from CRAN. Retrieved 04-01-2021 from: https://github.com/ramhiser/activelearning

###################################
#                                 #
#  1. DEFINE HELPER FUNCTIONS     #  
#                                 #
###################################

which_unlabeled <- function(y) {
  which(is.na(y))
}

which_labeled <- function(y, return_logical = FALSE) {
  which(!is.na(y))
}

split_labeled <- function(x, y) {
  
  y <- factor(y)
  
  unlabeled_i <- which_unlabeled(y)
  list(x_labeled=x[-unlabeled_i, ],
       y_labeled=y[-unlabeled_i],
       x_unlabeled=x[unlabeled_i, ],
       y_unlabeled=y[unlabeled_i])
}

###################################
#                                 #
#  2. UNCERTAINTY SAMPLING        #  
#                                 #
###################################

least_confidence <- function(posterior) {
  apply(posterior, 1, max)
}

uncertainty_sampling <- function(x, y, num_query) {
  
  y  <- factor(y)
  split_out <- split_labeled(x, y)
  
  train_out <- caret::train(x=split_out$x_labeled, y=split_out$y_labeled,
                            method="rf", verbose=FALSE, ntree = 600, trControl = ctrl, metric = "AUC", tuneGrid = data.frame(mtry=4))
  
  posterior <- predict(train_out, newdata=split_out$x_unlabeled, type="prob")
  posterior <- as.matrix(posterior)
  
  query <- order(least_confidence=least_confidence(posterior), decreasing=F)[seq_len(num_query)]
  
  most_uncertain <- list(split_out$x_unlabeled[c(query),])
  
  return(most_uncertain)
}

###################################
#                                 #
#  3. QBC                         #  
#                                 #
###################################

fit_f <- function(x,y) { 
  caret::train(x,y,method="rf", verbose=FALSE, ntree = 600, trControl = ctrl, metric = "AUC", tuneGrid = data.frame(mtry=4))}

predict_f <- function(object, x){
  predict(object, x, type = "prob")
}

kullback <- function(x, type="prob") { #x is list of doubles, each member in list is output one classifier G/B.
  avg_post <- Reduce('+', x) / length(x) #outputs G/B average for each observation, length = number of classifiers
  kullback_members <- lapply(x, function(obs) {
    rowSums(obs * log(obs / avg_post)) #calculates in each list for each observation for G/B, KL, sums over G/B (over y)
  })
  Reduce('+', kullback_members) / length(kullback_members) #sums over all classifiers the sum G/B (above) divided by # classifiers
}

query_bagging <- function(x, y, num_query, C) { 
  
  y <- factor(y)
  p <- ncol(x)
  split_out <- split_labeled(x, y)
  
  cl <- makeCluster(3)
  registerDoParallel(cl)
  environment(ctrl) <- .GlobalEnv
  environNames <- c("ctrl")
  clusterExport(cl, environNames, envir=.GlobalEnv )
  
  bag_control <- bagControl(
    fit=fit_f,
    predict=predict_f,
    aggregate=kullback,
    oob=FALSE,
    allowParallel=TRUE)
  
  bag_out <- bag(x=split_out$x_labeled,
                 y=split_out$y_labeled,
                 B=C, vars=p, bagControl=bag_control)
  
  disagreement <- predict(bag_out, split_out$x_unlabeled)
  
  query <- head(order(disagreement, decreasing=TRUE), n=num_query)
  
  kl_max <- list(split_out$x_unlabeled[c(query),])
  
  on.exit(stopCluster(cl))
  
  return(kl_max)
  
  }

