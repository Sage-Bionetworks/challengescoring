library(challengescoring)

bootLadderBoot(predictions = goodSim, ##data frame or path
               predictionColname = 'prediction',
               goldStandard = truth, ##data frame or path
               goldStandardColname = 'value',
               prevPredictions = badSim, ##data frame or path or NULL
               scoreFun = spearman,
               bootstrapN = 1000,
               reportBootstrapN = 10,
               bayesThreshold = 3,
               largerIsBetter = T,
               verbose = T,
               doParallel = T)
