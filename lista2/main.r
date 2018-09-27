library('rpart')
library('ROSE')
library('caret')
library('dbscan')

path = 'C:/Projects/lista1_multiple_classifier_system/lista2'
setwd(path)
source('utils.r')

experiments = function(dataset, prefix, skipTraining) {
    numOfFolds = 10
    sizeOfPool = 10    

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
                perceptronModels[[i]][[j]] = perceptron.train(trainSet, 1, 5)
            }
        }        
        # Saving the models
        saveRDS(perceptronModels, paste(prefix, "_perceptronModels.rds", sep = ""))
    }

    if(skipTraining){
        # Pruning
        perceptronModelsPrunning = list()
        perceptronMetrics = list()

        for(i in 1:numOfFolds){            
            print(i)
            kdnValues = kdn(x[[i]]$train, 5)
            perceptronModelsPrunning[[i]] = list()
            perceptronMetrics[[i]] = list()

            validationSets = list()
            validationSets[[length(validationSets) + 1]] = x[[i]]$train
            validationSets[[length(validationSets) + 1]] = x[[i]]$train[which(kdnValues > 0.5), ]
            validationSets[[length(validationSets) + 1]] = x[[i]]$train[which(kdnValues <= 0.5), ]

            for(j in 1:length(validationSets)){
                if(nrow(validationSets[[j]]) < 1){
                    print("Empty Validation Set")
                }
                perceptronModelsPrunning[[i]]$bestFirst = bestFirstPruning(perceptronModels[[i]], validationSets[[j]])
                # perceptronModelsPrunning[[i]]$reduceError = reduceErrorPruning(perceptronModels[[i]], validationSets[[j]])
            }
            
            perceptronMetrics[[i]][[1]] = predictPerceptron(perceptronModels[[i]], x[[i]]$test)
            perceptronMetrics[[i]][[2]] = predictPerceptron(perceptronModelsPrunning[[i]]$bestFirst, x[[i]]$test)
            # perceptronMetrics[[i]][[3]] = predictPerceptron(perceptronModelsPrunning[[i]]$reduceError, x[[i]]$test)            
        }
        saveRDS(perceptronMetrics, paste(prefix, "_perceptronMetrics.rds", sep = ""))
    }
}

# data.csv: http://promise.site.uottawa.ca/SERepository/datasets/cm1.arff
# data2.csv: http://promise.site.uottawa.ca/SERepository/datasets/kc1-class-level-defectiveornot.arff
data1 = read.table('data.csv', sep=',', header=TRUE)
data2 = read.table('data2.csv', sep=',', header=TRUE)

## Generate the pool
experiments(data1, "data1", FALSE)
experiments(data2, "data2", FALSE)