# standard scoring functions ---------------------------------------------------
# These functions must take these parameters:
# gold A numeric vector of gold standard values.
# pred A numeric vector of prediction values.
# gold and pred must be the same length and not have any NA, NaN, Inf values
# All other paremeters must have defaults

#'Calculate root mean squared error for a set of predicted values.
#' @param gold A numeric vector of gold standard values.
#' @param pred A numeric vector of prediction values.
#' @export
score_rmse <-  function(gold, pred){
  sqrt(mean((gold - pred) ** 2))
}

#' Calculate Spearman correlation for a set of predicted values.
#' @param gold A numeric vector of gold standard values.
#' @param pred A numeric vector of prediction values.
#' @export
score_spearman <- function(gold, pred){
  if (stats::var(pred) == 0) return("Variance of predictions is 0")
  stats::cor(gold, pred, method = "spearman")
}

#' Calculate Pearson correlation for a set of predicted values.
#' @param gold A numeric vector of gold standard values.
#' @param pred A numeric vector of prediction values.
#' @export
score_pearson <- function(gold, pred){
  if (stats::var(pred) == 0) return("Variance of predictions is 0")
  stats::cor(gold, pred, method = "pearson")
}

# factor scoring functions -----------------------------------------------------
# These functions must take these parameters:
# gold A factor vector of gold standard values.
# pred A factor vector of prediction values.
# gold and pred must be the same length and not have any NA, NaN, values,
# and have the same levels
# All other paremeters must have defaults

#' Calculate the Area Under the ROC Curve
#' @param gold A factor vector of gold standard values.
#' @param pred A factor vector of prediction values.
#' @export
#' @importFrom pROC auc
score_auc <- function(gold, pred) {
  as.numeric(pROC::auc(response = gold, predictor = pred))
}

#' Calculate the balanced accuracy [ i.e., ( sensitivity + specificity ) / 2 ]
#' @param gold A factor vector of gold standard values.
#' @param pred A factor vector of prediction values.
#' @export
#' @importFrom caret confusionMatrix
score_balanced_accuracy <- function(gold, pred) {
  cm <- caret::confusionMatrix(data = pred, reference = gold, positive = "1")
  as.numeric(cm$byClass["Balanced Accuracy"])
}

#' Calculate Matthew's Correlation Coefficient
#' @param gold A factor vector of gold standard values.
#' @param pred A factor vector of prediction values.
#' @export
#' @importFrom caret confusionMatrix
score_mcc <- function(gold, pred) {
  cm <- as.table(
    caret::confusionMatrix(data = pred, reference = gold), positive = "1"
  )
  num <- ((cm[1,1]*cm[2,2]) - (cm[1,2]*cm[2,1]))
  den <- (sqrt(cm[1,1] + cm[1,2]) * sqrt(cm[1,1] + cm[2,1])
          * sqrt(cm[2,2] + cm[1,2]) * sqrt(cm[2,2] + cm[2,1]))
  num / den
}

#' Calculate F1 score
#' @param gold A factor vector of gold standard values.
#' @param pred A factor vector of prediction values.
#' @export
#' @importFrom caret confusionMatrix
score_f1 <- function(gold, pred) {
  cm <- caret::confusionMatrix(data = pred, reference = gold, positive = "1")
  as.numeric(cm$byClass["F1"])
}



# other scoring functions -----------------------------------------------------

#' Calculate concordance index (Harrell's C-statistic)
#' for a set of predicted values. .
#' @param gold A numeric vector of gold standard values
#' (e.g. output from survival::Surv).
#' @param pred A numeric vector of prediction values
#' (e.g. output from survival::Surv).
#' @export
c_statistic <- function(gold, pred){
  Hmisc::rcorr.cens(pred, survival::Surv(gold), outx = TRUE)["C Index"]
}

#' Calculate AUC for a set of binary outcomes and a numeric vector of
#'prediction values.
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
#'@param direction Side of threshold to set to 1. greaterThan (0>thresh>=1) or
#'lessThan (1=<thresh<0)
#'@export
binarize <- function(vec, threshold, direction){
  print('binarizing')
  if (direction == 'greaterThan') {
    vec[vec >= threshold] <- 1
    vec[vec < threshold] <- 0
    vec
  }
  if (direction == 'lessThan') {
    vec[vec <= threshold] <- 1
    vec[vec > threshold] <- 0
    vec
  }
}
