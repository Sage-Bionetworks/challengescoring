#'@export
rmse <-  function(gold, pred){
  mean((gold - pred) ** 2)
}

#'@export
spearman <- function(gold, pred){
  cor(gold, pred, method = "spearman")
}

#'@export
pearson <- function(gold, pred){
  cor(gold, pred, method = "pearson")
}


