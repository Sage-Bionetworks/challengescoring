rmse <-  function(gold, pred){
  mean((gold - pred) ** 2)
}

spearman <- function(gold, pred){
  cor(gold, pred, method = "spearman")
}

pearson <- function(gold, pred){
  cor(gold, pred, method = "pearson")
}


