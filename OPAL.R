#' @title OPALgain Function (Kottke 2015)
#' 
#' @description  Defines OPALgain function.
#' 
#' @details 
#'
#' This is an R adaptation of Kottke and Krempl's Python Script:
#' Kottke, D., & Krempl, G. (2015). OPALgain Python Script. Retrieved 20-04-2021 from https://kmd.cs.ovgu.de/res/opal/OPALgain.py
#' 
#' Copyright (C) 2015 Daniel Kottke, Georg Krempl
#' Author: Daniel Kottke <daniel.kottke@ovgu.de>
#' Related Paper: Krempl, G., Kottke, D., & Lemaire, V. (2015b). Optimised probabilistic active learning (OPAL): 
#' For fast, non-myopic, cost-sensitive active classification, Machine Learning, 100(2–3), 449–476.
#' 
#' This R adaptation matches Python as long as n is smaller than m, and m is not too big.
#' This adaptation adds a log transformation of the gamma function to avoid exploding when n is large.
#' 


###################################
#                                 #
#  1. PREPARE STABLE PARAMETERS   #  
#                                 #
###################################

### Tao defined as relative FP cost: FP / (FP+FN).
tao <- 1128 / (1128+2704) #Uses financial values from PROFIT_VARIABLES.R script.

###################################
#                                 #
#  2. DEFINE FUNCTIONS            #  
#                                 #
###################################



gain <- function(n,k,tao,m,y){
  if(n == 0){
    n <-1
  } else{
    n <- n
  }
  
  if((n+m) == 0) { 
    conditions <- tao
  } else{
    conditions <-  1*(k + y)/(n + m)
  }
  if (abs(conditions - tao) < 1e-15){
    return <- choose(m,y) * (tao-tao**2) *  exp(gammaln(1-y+m+n-k) + gammaln(1+y+k) - gammaln(2+m+n))
  } else{
    if(conditions < tao){
      return <- choose(m,y) * (1-tao)      *  exp(gammaln(1-y+m+n-k) + gammaln(2+y+k) - gammaln(3+m+n))
    } else{
      if (conditions > tao)
        return <- choose(m,y) * (tao)        *  exp(gammaln(2-y+m+n-k) + gammaln(1+y+k) - gammaln(3+m+n))
    }
  }
  return(return)
}

OPAL <- function(n, pObs, tao, m){ 
  k = round(n*pObs)
  cur_val = gain(n,k,tao,0,0)
  
  fut <- data.frame(matrix(ncol = m+1, nrow = 1))
  y_list <- as.list(seq(1,m+1))
  
  fut_loop <- for (y in 1:length(y_list)){
    fut[[y]] <- gain(n,k,tao,m,y)
  }
  
  fut_val = sum(fut[1,])
  
  val = choose(n,k) * (n+1)/m * (cur_val - fut_val)
  
  if(val == Inf){
    val <- 10e307
  } else{
    val = val
  }
  
  return(val)
}


