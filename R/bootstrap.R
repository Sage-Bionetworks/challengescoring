#' Calculate a bootstrapped score for an initial submission or subsequent submission.
#' @param predictions The relative path to the current prediction csv, or a data frame.
#' @param predictionColname The name of the column in the prediction csv that contains numeric prediction values. If also using a previous prediction file, must be the same name.
#' @param goldStandard The relative path to the gold standard/test data csv, or a data frame.
#' @param goldStandardColname The name of the column in the gold standard csv that contains numeric prediction values.
#' @param prevPredictions If a previous prediction file for this team/participant already exists, pass in the path or data frame here. Prediction colname must match.
#' @param scoreFun A scoring function. Default is Spearman correlation. Any function can be passed as long as it can calculate a score from two vectors (gold standard first, and prediction values second).
#' @param bootstrapN Number of total bootstraps to perform (default 10000).
#' @param reportBootstrapN Number of bootstraps to base returned score off of (default 10). The greater this value, the more accurate of a result is returned (and possibly the more the test data can be overfit).
#' @param bayesThreshold The threshold for reporting a score. If the bayes factor (K) of the new prediction relative to the previous submission is less than this value, no score is returned. Default 3.
#' @param seed Set a seed for bootstrap sampling. Default 98121.
#' @param largerIsBetter Set this to FALSE if a smaller scoring metric indicates better performance (e.g. root mean squared error). Default TRUE.
#' @param verbose Report step. Default FALSE.
#' @param doParallel Bootstrap in parallel. Only works on UNIX based OS. Default TRUE.
#' @return A named list with a bootstrapped score and a boolean stating whether the bayesThreshold was met. If verbose == T, also returns the calculated Bayes factor.
#' @export
bootLadderBoot <- function(predictions,
                           predictionColname,
                           goldStandard,
                           goldStandardColname,
                           prevPredictions = NULL,
                           scoreFun = spearman,
                           bootstrapN = 10000,
                           reportBootstrapN = 10,
                           bayesThreshold = 3,
                           seed = 98121,
                           largerIsBetter = TRUE,
                           verbose = FALSE,
                           doParallel = FALSE){

  if(bootstrapN < reportBootstrapN){
    stop("bootstrapN must be >= reportBootstrapN")
  }

  if(bayesThreshold < 0){
    stop("bayesThreshold must be >0")
  }

  if(is.data.frame(goldStandard)){
    goldStandardDF<-goldStandard
  }else{
    if(verbose == TRUE){print("reading gold standard file")}
    goldStandardDF <- read.csv(goldStandard) ##reads the gold standard file
  }

  if(is.data.frame(predictions)){
    predictionsDF<-predictions
  }else{
    if(verbose == TRUE){print("reading prediction file")}
    predictionsDF <- read.csv(predictions) ## reads the prediction file
  }

  if(is.null(prevPredictions)){ ## tests for previous submission -if none, just joins gold standard and predicition into one dataframe (ensures matched order on id columns)
    joinedData <- dplyr::full_join(goldStandardDF, predictionsDF) %>%
      dplyr::select_(goldStandardColname, predictionColname) %>%
      purrr::set_names('gold', 'pred')

    goldStandardMatrix <- joinedData[,1, drop = FALSE] %>% as.matrix() #make a gold standard matrix (1 column)
    predictionsMatrix <- joinedData[,2, drop = FALSE] %>% as.matrix() #make a prediction data matrix (1 column)

    # INSERT TEST FOR MATCHING COLUMN NAMES

  }else{ ## if there is a previous submission, that gets read in also, and joined to this dataframe (ensures matched order on id columns)
    if(is.data.frame(prevPredictions)){
      prevPredictionsDF<-prevPredictions %>% dplyr::mutate_("prevpred"=predictionColname) %>% dplyr::select(-predictionColname)
    }else{
      if(verbose == TRUE){print("reading previous prediction file")}
      prevPredictionsDF <- read.csv(prevPredictions) %>% dplyr::mutate_("prevpred"=predictionColname) %>% dplyr::select(-predictionColname)
    }

    joinedData <- dplyr::full_join(goldStandardDF, predictionsDF) %>%
      dplyr::full_join(prevPredictionsDF) %>%
      dplyr::select_(goldStandardColname, predictionColname, "prevpred") %>%
      purrr::set_names('gold', 'pred', 'prevpred')

    goldStandardMatrix <- joinedData[,1, drop = FALSE] %>% as.matrix() #make a gold standard matrix (1 column)
    predictionsMatrix <- joinedData[,2:3, drop = FALSE] %>% as.matrix() #make a prediction matrix (2 columns - current prediction, previous prediction)
    }

  ## bootstrap gold standard and predictions
  bootstrapMetricMatrix <- bootstrappingMetric(goldStandardMatrix = goldStandardMatrix,
                                               predictionsMatrix = predictionsMatrix,
                                               scoreFun = scoreFun,
                                               bootstrapN = bootstrapN,
                                               seed = seed,
                                               doParallel = doParallel)

  if(verbose == TRUE){print("joining bootstrapped data frames")}

  if(!is.null(prevPredictions)){
    meanBS_new <- mean(bootstrapMetricMatrix[1:bootstrapN,1])
    meanBS_prev <- mean(bootstrapMetricMatrix[1:bootstrapN,2])
    if(largerIsBetter == T & meanBS_new<=meanBS_prev){invBayes = T}
    if(largerIsBetter == T & meanBS_new>meanBS_prev){invBayes = F}
    if(largerIsBetter == F & meanBS_new<=meanBS_prev){invBayes = T}
    if(largerIsBetter == F & meanBS_new>meanBS_prev){invBayes = F}
  }

  if(!is.null(prevPredictions) & largerIsBetter == TRUE){ #test for previous prediction data and whether larger scores are better
    K <- computeBayesFactor(bootstrapMetricMatrix, 2, invertBayes = invBayes) #compute bayes factor where a larger score is better
    metBayesCutoff <- c(K['pred']>bayesThreshold)
    if(K['pred'] > bayesThreshold & meanBS_new > meanBS_prev){ ##if bayes score is greater than threshold set by user, AND score is better, report bootstrapped score

      if(verbose == TRUE){print("Larger is better : current prediction is better")}
      returnedScore <- mean(bootstrapMetricMatrix[1:reportBootstrapN,1])
    }else{

      if(verbose == TRUE){print("Larger is better : previous prediction is better or bayes threshold not met")}
      returnedScore <- mean(bootstrapMetricMatrix[1:reportBootstrapN,2]) ##if within K threshold, return previous bootstrap score
    }
  }else if(!is.null(prevPredictions) & largerIsBetter == FALSE){ #compute bayes factor where a smaller score is better
     K <- computeBayesFactor(bootstrapMetricMatrix, 2, invertBayes = invBayes)
     metBayesCutoff <- c(K['pred']>bayesThreshold)
    if(K['pred'] > bayesThreshold & meanBS_new < meanBS_prev){ ##if bayes score is greater than threshold set by user, AND score is better, report bootstrapped score
      if(verbose == TRUE){print("Smaller is better : current prediction is better")}
      returnedScore <- mean(bootstrapMetricMatrix[1:reportBootstrapN,1])
    }else{
      if(verbose == TRUE){print("Smaller is better : previous prediction is better or bayes threshold not met")}
      returnedScore <- mean(bootstrapMetricMatrix[1:reportBootstrapN,2]) ##if within K threshold, return NA for score
    }
  }else if(is.null(prevPredictions)){ ## if there is no previous file, simply return bootstrapped score
    if(verbose == TRUE){print("no previous submission")}
    returnedScore <- mean(bootstrapMetricMatrix[1:reportBootstrapN,1])
  }


  if(verbose == TRUE & !is.null(prevPredictions)){
    return(list("score" = returnedScore, "metBayesCutoff" = as.vector(metBayesCutoff), "bayes" = as.vector(K['pred'])))
  }else if(verbose == FALSE & !is.null(prevPredictions)){
    return(list("score" = returnedScore, "metBayesCutoff" = as.vector(metBayesCutoff['pred'])))
  }else{
    return(list("score" = returnedScore, "metBayesCutoff" = NA))
  }
}

#' Create an matrix of bootstrapped predictions.
#' @param goldStandardMatrix A single column matrix with the gold standard predictions.
#' @param predictionsMatrix Columns of prediction sets, in the same order as the goldStandardMatrix.
#' @param bootstrapN Number of total bootstraps to perform.
#' @param seed Set a seed for bootstrap sampling.
#' @param doParallel Bootstrap in parallel. Only works on UNIX based OS. Default FALSE.
#' @return An MxN matrix of bootstrapped predictions where M is the number of bootstraps performed and N is the number of prediction sets.
#' @export
bootstrappingMetric <- function(goldStandardMatrix, predictionsMatrix, scoreFun = scoreFun, bootstrapN = bootstrapN, seed = seed, doParallel = F, ...){

   # matrix, columns are boostraps, rows are samples
  bsIndexMatrix <- matrix(1:nrow(goldStandardMatrix), nrow(goldStandardMatrix), bootstrapN)
  bsIndexMatrix <- t(aaply(bsIndexMatrix, 2, sample, replace = T))# create bootstrap indices

  numCores <- parallel::detectCores()
  if(numCores == 1){doParallel = FALSE}

  if(numCores > 1 & doParallel == TRUE){
  doMC::registerDoMC(cores = numCores-1)
  gc()
  }

  bsMetric  <- alply(.data = bsIndexMatrix, ##score bootstrapped indices
                      .margins = 2,
                      .fun = indexedScore,
                      .parallel = doParallel,
                      goldStandardMatrix,
                      predictionsMatrix,
                      scoreFun,
                      ...)

  # matrix, columns are prediction sets, rows are bootstraps
  bsMetric <- do.call(rbind, bsMetric)
  return(bsMetric)
}

#' Calculate one or more Bayes factors using a bootstrapMetricMatrix.
#' @param bootstrapMetricMatrix An NxM matrix where M is the number of bootstraps performed and N is the number of prediction sets (output of bootstrappingMetric function).
#' @param refPredIndex Column index of the reference prediction to calculate Bayes factor (e.g. best prediction).
#' @param invertBayes Boolean to invert Bayes factor.
#' if(largerIsBetter == T & currentPred<=refPred){invertBayes = T},
#' if(largerIsBetter == T & currentPred>refPred){invertBayes = F},
#' if(largerIsBetter == F & currentPred<=refPred){invertBayes = T},
#' if(largerIsBetter == F & currentPred>refPred){invertBayes = F}
#' @return A matrix of Bayes factors.
#' @export
computeBayesFactor <- function(bootstrapMetricMatrix, refPredIndex, invertBayes){

    M <- as.data.frame(bootstrapMetricMatrix - bootstrapMetricMatrix[,refPredIndex])
    K <- apply(M ,2, function(x) {
      k <- sum(x >= 0)/sum(x < 0)
      return(k)
    })
    K[refPredIndex] <- 0
    if(invertBayes == T){K <- 1/K}
    return(K)
}

#wrapper function to pass bootstrapped data to scoring function provided by user
#this allows user to provide a simple scoring function of the form function(gold, pred)
#where gold and pred are vectors with the gold standard data and the prediction data
indexedScore <- function(dataIndices, goldStandardMatrix, predictionsMatrix, scoreFun){
  gold <- goldStandardMatrix[dataIndices,]
  if(ncol(predictionsMatrix)>1){
    plyr::aaply(predictionsMatrix[dataIndices,], 2, scoreFun, gold = gold)
  }else{
    scoreFun(gold = goldStandardMatrix[dataIndices,], pred = predictionsMatrix[dataIndices,])
  }
}
