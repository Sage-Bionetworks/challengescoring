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


