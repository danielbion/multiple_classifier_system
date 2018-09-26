library('rpart')
library('ROSE')
library('caret')

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

        for (i in 1:numOfFolds) {
            print(i)
            
            perceptronModels = list()
            trainSet = x[[i]]$train

            for (j in 1:sizeOfPool) {    
                cat("", j)
                perceptronModels[[j]] = list()

                # Apply bagging
                trainSet = bagging(trainSet)
                
                # Train the perceptron
                perceptronModels[[j]] = perceptron.train(trainSet, 1, 10)
            }
        }        
        # Saving the models
        saveRDS(perceptronModels, paste(prefix, "_perceptronModels.rds", sep = ""))
    }

    perceptronMetrics = list()
    
    # Prunning
    for(i in 1:numOfFolds){
        print(i)
        # Predict on test data and get the metrics
        perceptronMetrics[[i]] = predictPerceptron(perceptronModels, x[[i]]$test, sizeOfPool)
        saveRDS(perceptronMetrics, paste(prefix, "_perceptronMetrics.rds", sep = ""))
    }
}

# data.csv: http://promise.site.uottawa.ca/SERepository/datasets/cm1.arff
# data2.csv: http://promise.site.uottawa.ca/SERepository/datasets/kc1-class-level-defectiveornot.arff
data1 = read.table('data.csv', sep=',', header=TRUE)
data2 = read.table('data2.csv', sep=',', header=TRUE)

## Generate the pool
experiments(data1, "data1", TRUE)
experiments(data2, "data2", TRUE)