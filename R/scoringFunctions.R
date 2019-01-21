#'Calculate root mean squared error for a set of predicted values.
#' @param gold A numeric vector of gold standard values.
#' @param pred A numeric vector of prediction values.
#' @export
rmse <-  function(gold, pred){
  sqrt(mean((gold - pred) ** 2))
}

#' Calculate Spearman correlation for a set of predicted values.
#' @param gold A numeric vector of gold standard values.
#' @param pred A numeric vector of prediction values.
#' @export
spearman <- function(gold, pred){
  cor(gold, pred, method = "spearman")
}

#' Calculate Pearson correlation for a set of predicted values.
#' @param gold A numeric vector of gold standard values.
#' @param pred A numeric vector of prediction values.
#' @export
pearson <- function(gold, pred){
  cor(gold, pred, method = "pearson")
}

#' Calculate concordance index (Harrell's C-statistic) for a set of predicted values. .
#' @param gold A numeric vector of gold standard values (e.g. output from survival::Surv).
#' @param pred A numeric vector of prediction values (e.g. output from survival::Surv).
#' @export
c_statistic <- function(gold, pred){
  Hmisc::rcorr.cens(pred,Surv(gold),outx=TRUE)["C Index"]
}

#' Calculate AUC for a set of binary outcomes and a numeric vector of prediction values.
#' @param gold A binary vector of outcome values.
#' @param pred A numeric vector of prediction values.
auc <- function(gold, pred){
  # if(!exists('gold.bin')){
  #   gold.bin <<- binarize(gold, 5, 'lessThan')
  # }
  pROC::roc(gold, pred)$auc %>% as.numeric()
}

#'Binarize a numeric vector above and below a given threshold.
#'@param vec Vector to binarize.
#'@param threshold Threshold at which to binarize.
#'@param direction Side of threshold to set to 1. greaterThan (0>thresh>=1) or lessThan (1=<thresh<0)
#'@export
binarize <- function(vec, threshold, direction){
  print('binarizing')
  if(direction == 'greaterThan'){
    vec[vec>=threshold] <- 1
    vec[vec<threshold] <- 0
    vec
  }
  if(direction == 'lessThan'){
    vec[vec<=threshold] <- 1
    vec[vec>threshold] <- 0
    vec
  }
}
