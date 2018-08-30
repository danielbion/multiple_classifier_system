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

# auc = c()

# for (i in 1:numOfFolds){
#     train = x[[i]]$train
#     test = x[[i]]$test
#     fit = decisionTree(train)
#     pred = predict(fit, newdata = test)[,2]
#     pred = ifelse(pred < 0.50, 'false', 'true')
#     pred = as.factor(pred)
#     auc = c(auc, roc.curve(test[,'target'], pred, plotit = F)$auc)
# }


hitRate = list()
auc = list()
gmean = list()
fmeasure = list()


for(i in 1:numOfFolds){
    print(i)
    hitRate[[i]] = list()
    auc[[i]] = list()
    gmean[[i]] = list()
    fmeasure[[i]] = list()

    dtModels = list()
    perceptronModels = list()
    xTrain = x[[i]]$train
    xTest = x[[i]]$test

    for(j in 1:sizeOfPool){    
        cat("", j)    
        datasets = list()
        dtModels[[j]] = list()
        perceptronModels[[j]] = list()

        xBagging = bagging(xTrain)
        
        datasets[[length(datasets) + 1]] = subset(xBagging, 0.5)
        datasets[[length(datasets) + 1]] = subset(xBagging, 0.6)
        datasets[[length(datasets) + 1]] = subset(xBagging, 0.7)
        datasets[[length(datasets) + 1]] = subset(xBagging, 0.8)
        datasets[[length(datasets) + 1]] = subset(xBagging, 0.9)
        datasets[[length(datasets) + 1]] = xBagging
        
        xRandomSubspace = randomSubspace(xTrain, 0.5)

        datasets[[length(datasets) + 1]] = subset(xRandomSubspace, 0.5)
        datasets[[length(datasets) + 1]] = subset(xRandomSubspace, 0.6)
        datasets[[length(datasets) + 1]] = subset(xRandomSubspace, 0.7)
        datasets[[length(datasets) + 1]] = subset(xRandomSubspace, 0.8)
        datasets[[length(datasets) + 1]] = subset(xRandomSubspace, 0.9)
        datasets[[length(datasets) + 1]] = xRandomSubspace
       
        for(k in 1:length(datasets)){
            dtModels[[j]][[k]] = decisionTree(datasets[[k]])    
            # perceptronModels[[j]][[k]] = perceptron(datasets[[k]])
        }
    }

    # Predict on test dataset
    pred = list()
    for(k in 1:length(dtModels[[j]])){
        pred[[k]] = matrix(0, sizeOfPool, nrow(xTest))
        for(j in 1:sizeOfPool){            
            p = predict(dtModels[[j]][[k]], newdata = xTest)
            pred[[k]][j, ] = ifelse(p[,2] < 0.50, 'false', 'true')
        }
    }

    # Get majority vote
    predFinal = list()
    for(k in 1:length(dtModels[[j]])){   
        predFinal[[k]] = list()
        for(j in 1:nrow(xTest)){
            predFinal[[k]][[j]] = majorityVote(pred[[k]][,j])
        }
    }

    # Calculate metrics
    for(k in 1:length(dtModels[[j]])){
        predF = as.factor(array(unlist(predFinal[[k]])))
        auc[[i]][[k]] = roc.curve(xTest[,'target'], predF, plotit = F)$auc
    }
}

