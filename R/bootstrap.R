#' Calculate a bootstrapped score for an initial submission or subsequent submission.
#' @param predictionsPath The relative path to the current prediction csv.
#' @param predictionColname The name of the column in the prediction csv that contains numeric prediction values. If also using a previous prediction file, must be the same name.
#' @param goldStandardPath The relative path to the gold standard/test data csv.
#' @param goldStandardColname The name of the column in the gold standard csv that contains numeric prediction values.
#' @param prevPredictionsPath If a previous prediction file for this team/participant already exists, pass in the path here. Prediction colname must match.
#' @param scoreFun A scoring function. Default is Spearman correlation. Any function can be passed as long as it can calculate a score from two vectors (gold standard first, and prediction values second).
#' @param bootstrapN Number of total bootstraps to perform (default 10000).
#' @param reportBootstrapN Number of bootstraps to base returned score off of (default 10). The greater this value, the more accurate of a result is returned (and possibly the more the test data can be overfit).
#' @param bayesThreshold The threshold for reporting a score. If the bayes factor (K) of the new prediction relative to the previous submission is less than this value, no score is returned. Default 3.
#' @param seed Set a seed for bootstrap sampling. Default 98121.
#' @param largerIsBetter Set this to FALSE if a smaller scoring metric indicates better performance (e.g. root mean squared error). Default TRUE.
#' @param verbose Report step. Default FALSE.
#' @param doParallel Bootstrap in parallel. Only works on UNIX based OS. Default TRUE.
#' @export
bootLadderBoot <- function(predictionsPath,
                           predictionColname,
                           goldStandardPath,
                           goldStandardColname,
                           prevPredictionsPath = NULL,
                           scoreFun = spearman,
                           bootstrapN = 10000,
                           reportBootstrapN = 10,
                           bayesThreshold = 3,
                           seed = 98121,
                           largerIsBetter = TRUE,
                           verbose = FALSE,
                           doParallel = TRUE){

  if(verbose == TRUE){print("reading gold standard file")}
  goldStandardDF <- read.csv(goldStandardPath) ##reads the gold standard file

  if(verbose == TRUE){print("reading prediction file")}
  predictionsDF <- read.csv(predictionsPath) ## reads the prediction file

  if(is.null(prevPredictionsPath)){ ## tests for previous submission -if none, just joins gold standard and predicition into one dataframe (ensures matched order on id columns)
    joinedData <- dplyr::full_join(goldStandardDF, predictionsDF) %>%
      dplyr::select_(goldStandardColname, predictionColname) %>%
      purrr::set_names('gold', 'pred')

    goldStandardMatrix <- joinedData[,1, drop = FALSE] %>% as.matrix() #make a gold standard matrix (1 column)
    predictionsMatrix <- joinedData[,2, drop = FALSE] %>% as.matrix() #make a prediction data matrix (1 column)

  }else{ ## if there is a previous submission, that gets read in also, and joined to this dataframe (ensures matched order on id columns)
    if(verbose == TRUE){print("reading previous prediction file")}
    prevPredictionsDF <- read.csv(predictionsPath) %>% dplyr::mutate_("prevpred"=predictionColname) %>% dplyr::select(-predictionColname)
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

  if(!is.null(prevPredictionsPath) & largerIsBetter == TRUE){ #test for previous prediction data and whether larger scores are better

    K <- computeBayesFactor(bootstrapMetricMatrix, 1, largerIsBetter = TRUE) #compute bayes factor where a larger score is better
    meanBS_new <- mean(bootstrapMetricMatrix[1:reportBootstrapN,1])
    meanBS_prev <- mean(bootstrapMetricMatrix[1:reportBootstrapN,2])
    if(K['pred'] > bayesThreshold & meanBS_new > meanBS_prev){ ##if bayes score is greater than threshold set by user, AND score is better, report bootstrapped score
      returnedScore <- mean(bootstrapMetricMatrix[1:reportBootstrapN,1])
    }else{
      returnedScore <- NA ##if within K threshold, return NA for score
    }
  }else if(!is.null(prevPredictionsPath) & largerIsBetter == FALSE){ #compute bayes factor where a smaller score is better
    K <- computeBayesFactor(bootstrapMetricMatrix, 1, largerIsBetter = FALSE)
    meanBS_new <- mean(bootstrapMetricMatrix[1:reportBootstrapN,1])
    meanBS_prev <- mean(bootstrapMetricMatrix[1:reportBootstrapN,2])
    if(K['pred'] > bayesThreshold & meanBS_new < meanBS_prev){ ##if bayes score is greater than threshold set by user, AND score is better, report bootstrapped score
      returnedScore <- mean(bootstrapMetricMatrix[1:reportBootstrapN,1])
    }else{
      returnedScore <- NA ##if within K threshold, return NA for score
    }
  }else if(is.null(prevPredictionsPath)){ ## if there is no previous file, simply return bootstrapped score
    if(verbose == TRUE){print("no previous file")}
    returnedScore <- mean(bootstrapMetricMatrix[1:reportBootstrapN,1])
  }
  return(returnedScore)
}

# this function creates paired bootstrap indices and then scores the bootstrapped data using the indexedScore function
bootstrappingMetric <- function(goldStandardMatrix, predictionsMatrix, scoreFun = scoreFun, bootstrapN = bootstrapN, seed = seed, doParallel = T, ...){

   # matrix, columns are boostraps, rows are samples
  bsIndexMatrix <- matrix(1:nrow(goldStandardMatrix), nrow(goldStandardMatrix), bootstrapN)
  bsIndexMatrix <- t(aaply(bsIndexMatrix, 2, sample, replace = T))# create bootstrap indices
  doMC::registerDoMC(cores = detectCores()-1)

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

#calculate bayes factor from a set of bootstrapped scores
computeBayesFactor <- function(bootstrapMetricMatrix, bestTeamIndex, largerIsBetter = TRUE){
  if(largerIsBetter==TRUE){
    M <- as.data.frame(bootstrapMetricMatrix - bootstrapMetricMatrix[,bestTeamIndex])
    K <- apply(M ,2, function(x) {sum(x <= 0)/sum(x > 0)})
    K[bestTeamIndex] <- 0
    return(K)
  }else{
    M <- as.data.frame(bootstrapMetricMatrix - (bootstrapMetricMatrix[,bestTeamIndex]))
    K <- apply(M ,2, function(x) {sum(-x <= 0)/sum(-x > 0)})
    K[bestTeamIndex] <- 0
    return(K)
  }
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
