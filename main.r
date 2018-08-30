library('rpart')
library('neuralnet')
library('ROSE')

path = 'C:/Projects/lista1_multiple_classifier_system'
setwd(path)
source('utils.r')

# data.csv: http://promise.site.uottawa.ca/SERepository/datasets/jm1.arff
# data2.csv: http://promise.site.uottawa.ca/SERepository/datasets/kc1.arff
dataset = read.table('data.csv', sep=',', header=TRUE)

numOfFolds = 10
sizeOfPool = 100
x = splitInFolds(dataset, numOfFolds)

dtMetrics = list()
perceptronMetrics = list()

percents = c(0.5, 0.6, 0.7, 0.8, 0.9, 1)

for(i in 1:numOfFolds){
    print(i)
    
    dtModels = list()
    perceptronModels = list()
    trainSets = list()

    for(j in 1:length(percents)){
        trainSets[[length(trainSets) + 1]] = subset(x[[i]]$train, percents[j])
    }

    for(j in 1:sizeOfPool){    
        cat("", j)
        dtModels[[j]] = list()
        perceptronModels[[j]] = list()

        trainSets2 = list()
        for(k in 1:length(percents)){
            trainSets2[[length(trainSets2) + 1]] = bagging(trainSets[[k]])
            trainSets2[[length(trainSets2) + 1]] = randomSubspace(trainSets[[k]], 0.5)
        }

        for(k in 1:length(trainSets2)){
            dtModels[[j]][[k]] = decisionTree(trainSets2[[k]])    
            perceptronModels[[j]][[k]] = perceptron.train(trainSets2[[k]], 1, 10)
        }
    }

    dtMetrics[[i]] = predictDecisionTree(dtModels, x[[i]]$test, sizeOfPool)
    perceptronMetrics[[i]] = predictPerceptron(perceptronModels, x[[i]]$test, sizeOfPool)
}

saveRDS(dtMetrics, "dtMetrics.rds")
saveRDS(perceptronMetrics, "perceptronMetrics.rds")

#dtMetrics = readRDS("dtMetrics.rds")
#perceptronMetrics = readRDS("perceptronMetrics.rds")

