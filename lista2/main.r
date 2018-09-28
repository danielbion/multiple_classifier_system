library('rpart')
library('ROSE')
library('caret')
library('dbscan')

path = 'C:/Projects/lista1_multiple_classifier_system/lista2'
setwd(path)
source('utils.r')

experiments = function(dataset, prefix, skipTraining) {
    numOfFolds = 10
    sizeOfPool = 5

    if (skipTraining) {
        x = readRDS(paste(prefix, "_x.rds", sep = ""))
        perceptronModels = readRDS(paste(prefix, "_perceptronModels.rds", sep = ""))
    }else {
        #Split in folds with train/test
        x = splitInFolds(dataset, numOfFolds)
        saveRDS(x, paste(prefix, "_x.rds", sep = ""))

        perceptronModels = list()

        for (i in 1:numOfFolds) {
            print(i)
            trainSet = x[[i]]$train
            perceptronModels[[i]] = list()

            for (j in 1:sizeOfPool) {
                cat("", j)

                # Apply bagging
                trainSet = bagging(trainSet)
                
                # Train the perceptron
                perceptronModels[[i]][[j]] = perceptron.train(trainSet, 1, 10)
            }
        }        
        # Saving the models
        saveRDS(perceptronModels, paste(prefix, "_perceptronModels.rds", sep = ""))
    }

    if(skipTraining){
        # Pruning
        perceptronModelsPruning = list()
        perceptronMetrics = list()
        diversity = list()

        for(i in 1:numOfFolds){            
            print(paste("Fold> ", i))
            kdnValues = kdn(x[[i]]$train, 10)
            perceptronModelsPruning[[i]] = list()
            perceptronMetrics[[i]] = list()
            diversity[[i]] = list()

            kdnThreshold = 0.2
            validationSets = list()
            validationSets[[length(validationSets) + 1]] = x[[i]]$train
            validationSets[[length(validationSets) + 1]] = x[[i]]$train[which(kdnValues > kdnThreshold), ]
            validationSets[[length(validationSets) + 1]] = x[[i]]$train[which(kdnValues <= kdnThreshold), ]

            for(j in 1:length(validationSets)){
                if(nrow(validationSets[[j]]) < 1){
                    print("Empty Validation Set")
                }
                perceptronModelsPruning[[i]][[j]] = list()
                perceptronModelsPruning[[i]][[j]]$bestFirst = bestFirstPruning(perceptronModels[[i]], validationSets[[j]])
                perceptronModelsPruning[[i]][[j]]$reduceError = reduceErrorPruning(perceptronModels[[i]], validationSets[[j]])

                perceptronMetrics[[i]][[j]] = list()
                perceptronMetrics[[i]][[j]]$full = predictPerceptron(perceptronModels[[i]], x[[i]]$test)
                perceptronMetrics[[i]][[j]]$bestFirst = predictPerceptron(perceptronModelsPruning[[i]][[j]]$bestFirst, x[[i]]$test)
                perceptronMetrics[[i]][[j]]$reduceError = predictPerceptron(perceptronModelsPruning[[i]][[j]]$reduceError, x[[i]]$test)    

                diversity[[i]][[j]] = list()
                diversity[[i]][[j]]$full = diversityMean(perceptronModels[[i]])
                diversity[[i]][[j]]$bestFirst = diversityMean(perceptronModelsPruning[[i]][[j]]$bestFirst)
                diversity[[i]][[j]]$reduceError = diversityMean(perceptronModelsPruning[[i]][[j]]$reduceError)
            }       
        }
        saveRDS(perceptronMetrics, paste(prefix, "_perceptronMetrics.rds", sep = ""))
        saveRDS(perceptronModelsPruning, paste(prefix, "_perceptronModelsPruning.rds", sep = ""))
        saveRDS(diversity, paste(prefix, "_diversity.rds", sep = ""))
    }
}

# data.csv: http://promise.site.uottawa.ca/SERepository/datasets/cm1.arff
# data2.csv: http://promise.site.uottawa.ca/SERepository/datasets/kc1-class-level-defectiveornot.arff
data1 = read.table('data.csv', sep=',', header=TRUE)
data2 = read.table('data2.csv', sep=',', header=TRUE)

## Generate the pool
experiments(data1, "data1", FALSE)
experiments(data2, "data2", FALSE)

## Prunning and metrics
experiments(data1, "data1", TRUE)
experiments(data2, "data2", TRUE)

perceptronModelsPruning = readRDS("data1_perceptronModelsPruning.rds")
numOfClassifiersBestFirst = c()
numOfClassifiersReduceError = c()
for(i in 1:10){
    for(j in 1:3){
        numOfClassifiersBestFirst = c(numOfClassifiersBestFirst, length(perceptronModelsPruning[[i]][[j]]$bestFirst))
        numOfClassifiersReduceError = c(numOfClassifiersReduceError, length(perceptronModelsPruning[[i]][[j]]$reduceError))
    }
}
plot(numOfClassifiersBestFirst, type='l')
plot(numOfClassifiersReduceError, type='l')