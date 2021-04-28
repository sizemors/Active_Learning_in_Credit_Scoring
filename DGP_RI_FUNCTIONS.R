#' @title Reject Inference Functions for DGP 
#' 
#' @description  Defines Hard Cut Off and Parcelling functions.
#' 
#' @details 
#' These functions use intuition from following sources:
#' 
#'  Anderson, B., Haller, S., & Siddiqi, N. (2009). Reject inference techniques implemented in credit scoring for SAS enterprise miner. 
#'  SAS Global Forum, SAS Institute, Inc. 
#'  
#'  Siddiqi, N. (2012). Credit risk scorecards: developing and implementing intelligent credit scoring. John Wiley & Sons, Inc. 
#' 

###################################
#                                 #
#  1. Define Hard Cut Off         #  
#                                 #
###################################
library("caret")
library("hmeasure")
library("dplyr")

### Define risk factor (expected higher rate of bads in rejects compared to accepts). ###
t <- 6

### Define function - input is all applicants accepted (cumulative over all iterations), and batch of rejects from that iteration. ###
hco_func <- function(df_acc, df_rej) { 
  
  #Calculate bad rate in accepts. If over 1.0, set as 1.0. If accepts bad rate is zero, rejects will be zero. Set floor of .20 to avoid labeling zero as bad.
  hc = ifelse(
    sum(ifelse(df_acc[ ,"BAD"] == "BAD", 1, 0)) / nrow(df_acc) * t < 1.0, 
    sum(ifelse(df_acc[ ,"BAD"] == "BAD", 1, 0)) / nrow(df_acc) * t, 
    1.0)
  
  hc = ifelse(hc == 0, .20, hc)  
  
  #Sort reject batch by RF predictions and apply Bad label to highest % calculated above, Good label to remaining.
  rej_hco <- rbind(df_rej %>% 
                     slice_max(rf, n = round(hc*nrow(df_rej)), with_ties = FALSE) %>% 
                     mutate(BAD = "BAD"),
                   df_rej %>% 
                     slice_min(rf, n = (nrow(df_rej) - round(hc*nrow(df_rej))), with_ties = FALSE) %>% 
                     mutate(BAD = "GOOD"))
  
  rej_hco$BAD <- factor(rej_hco$BAD, levels = c("GOOD", "BAD"))
  
  return(rej_hco)
}

###################################
#                                 #
#  2. Define Parcelling           #  
#                                 #
###################################

### Define risk factor (expected higher rate of bads in rejects compared to accepts in each decile). ###
j <- 6 

### Calculate bad rate in each decile of accepts and multiply by risk factor. If over 1.0, set as 1.0. If zero, set as .20. ###
parcel <- function(df_a, df_r) {
  df_a <- df_a %>% arrange(rf)
  d <- round(nrow(df_a) / 10)
  p_list <- list(
    ifelse(sum(ifelse(df_a[1:d        ,"BAD"] == "BAD", 1, 0)) / d * j < 1.0, sum(ifelse(df_a[1:d        ,"BAD"] == "BAD", 1, 0)) / d * j, 1.0),
    ifelse(sum(ifelse(df_a[d:(2*d)    ,"BAD"] == "BAD", 1, 0)) / (d+1) * j < 1.0, sum(ifelse(df_a[d:(2*d)    ,"BAD"] == "BAD", 1, 0)) / (d+1) * j, 1.0),
    ifelse(sum(ifelse(df_a[(2*d):(3*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j < 1.0, sum(ifelse(df_a[(2*d):(3*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j, 1.0),
    ifelse(sum(ifelse(df_a[(3*d):(4*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j < 1.0, sum(ifelse(df_a[(3*d):(4*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j, 1.0),
    ifelse(sum(ifelse(df_a[(4*d):(5*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j < 1.0, sum(ifelse(df_a[(4*d):(5*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j, 1.0),
    ifelse(sum(ifelse(df_a[(5*d):(6*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j < 1.0, sum(ifelse(df_a[(5*d):(6*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j, 1.0),
    ifelse(sum(ifelse(df_a[(6*d):(7*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j < 1.0, sum(ifelse(df_a[(6*d):(7*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j, 1.0),
    ifelse(sum(ifelse(df_a[(7*d):(8*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j < 1.0, sum(ifelse(df_a[(7*d):(8*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j, 1.0),
    ifelse(sum(ifelse(df_a[(8*d):(9*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j < 1.0, sum(ifelse(df_a[(8*d):(9*d),"BAD"] == "BAD", 1, 0)) / (d+1) * j, 1.0),
    ifelse(sum(ifelse(df_a[(9*d):(nrow(df_a)),"BAD"] == "BAD", 1, 0)) / (d+1) * j < 1.0, sum(ifelse(df_a[(9*d):(nrow(df_a)),"BAD"] == "BAD", 1, 0)) / (d+1) * j, 1.0))
  
  p_list <- lapply(p_list, function(x) ifelse(x == 0, .20, x))
  
  #Rejects partitioned into deciles by predicted probability of default.
  df_r$BAD <- NULL #Remove true label.
  df_r <- df_r %>% arrange(rf)
  q <- round(nrow(df_r) / 10)
  d_list <- list(
    df_r[1:q,         ]  ,
    df_r[(q+1):(2*q),     ]  ,
    df_r[((2*q)+1):(3*q), ]  ,
    df_r[((3*q)+1):(4*q), ]  ,
    df_r[((4*q)+1):(5*q), ]  ,
    df_r[((5*q)+1):(6*q), ]  ,
    df_r[((6*q)+1):(7*q), ]  ,
    df_r[((7*q)+1):(8*q), ]  ,
    df_r[((8*q)+1):(9*q), ]  ,
    df_r[((9*q)+1):nrow(df_r), ])
  
  #Using augmented bad rate calculated from accepts, randomly label as bad that % in each corresponding decile of rejects.
  for(i in 1:length(d_list)){
    d_list[[i]]$BAD[sample(1:nrow(d_list[[i]]), nrow(d_list[[i]]), FALSE)] <- rep(c("BAD","GOOD"), 
                                                                                  c(round(p_list[[i]]*nrow(d_list[[i]])), 
                                                                                    nrow(d_list[[i]]) - round(p_list[[i]]*nrow(d_list[[i]]))))
    
  }
  rej_par <-  do.call("rbind",d_list)
  rej_par$BAD <- factor(rej_par$BAD, levels = c("GOOD", "BAD"))  
  return(rej_par)
  
}
