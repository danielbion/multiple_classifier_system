library('rpart')
library('ROSE')
library('caret')

path = 'C:/Projects/lista1_multiple_classifier_system/lista1'
setwd(path)
source('utils.r')

experiments = function(dataset, prefix){
    numOfFolds = 10
    sizeOfPool = 100

    #Split in folds with train/test
    x = splitInFolds(dataset, numOfFolds)

    dtMetrics = list()
    perceptronMetrics = list()

    percents = c(0.5, 0.6, 0.7, 0.8, 0.9, 1)

    for(i in 1:numOfFolds){
        print(i)
        
        dtModels = list()
        perceptronModels = list()
        trainSets = list()

        # Subset in percents
        for(j in 1:length(percents)){
            trainSets[[length(trainSets) + 1]] = subsetStratified(x[[i]]$train, percents[j])
        }

        for(j in 1:sizeOfPool){    
            cat("", j)
            dtModels[[j]] = list()
            perceptronModels[[j]] = list()

            # Apply bagging and random subspace on the subsets
            trainSets2 = list()
            for(k in 1:length(percents)){
                trainSets2[[length(trainSets2) + 1]] = bagging(trainSets[[k]])
                trainSets2[[length(trainSets2) + 1]] = randomSubspace(trainSets[[k]], 0.5)
            }

            # Train the decision tree and perceptron
            for(k in 1:length(trainSets2)){
                dtModels[[j]][[k]] = decisionTree(trainSets2[[k]])    
                perceptronModels[[j]][[k]] = perceptron.train(trainSets2[[k]], 1, 10)
            }
        }

        # Predict on test data and get the metrics
        dtMetrics[[i]] = predictDecisionTree(dtModels, x[[i]]$test, sizeOfPool)
        perceptronMetrics[[i]] = predictPerceptron(perceptronModels, x[[i]]$test, sizeOfPool)
    }

    # Saving the metrics for analysis
    saveRDS(dtModels, paste(prefix, "_dtModels.rds", sep = ""))
    saveRDS(perceptronModels, paste(prefix, "_perceptronModels.rds", sep = ""))
    saveRDS(dtMetrics, paste(prefix, "_dtMetrics.rds", sep = ""))
    saveRDS(perceptronMetrics, paste(prefix, "_perceptronMetrics.rds", sep = ""))
}

# data.csv: http://promise.site.uottawa.ca/SERepository/datasets/cm1.arff
# data2.csv: http://promise.site.uottawa.ca/SERepository/datasets/kc1-class-level-defectiveornot.arff
data1 = read.table('data.csv', sep=',', header=TRUE)
data2 = read.table('data2.csv', sep=',', header=TRUE)

experiments(data1, "data1")
experiments(data2, "data2")

