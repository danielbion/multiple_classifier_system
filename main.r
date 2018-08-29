library('rpart')
library('neuralnet')
library('ROSE')

path = 'C:/Projects/lista1_multiple_classifier_system'
setwd(path)
source('utils.r')

dataset = as.matrix(read.table('data.csv', sep=',', header=FALSE))
numOfFolds = 10
sizeOfPool = 100
x = splitInFolds(dataset, numOfFolds)

hitRate = list()
auc = list()
gmean = list()
fmeasure = list()

decisionTree = function(x){
    x = as.data.frame(x)
    return (rpart(x[, ncol(x)] ~ . -x[, ncol(x)], data = x, maxdepth = 1))
}

perceptron = function(x){
    return(neuralnet(x[,22] ~ x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6] + x[,7] + x[,8] + x[,9] 
    + x[,10] + x[,11] + x[,12] + x[,13] + x[,14] + x[,15] + x[,16] + x[,17] + x[,18] + x[,19] + x[,20] + x[,21] , data=x, hidden=0))
}

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
            pred[[k]][j, ] = predict(dtModels[[j]][[k]], newdata = as.data.frame(xTest))
        }
    }

    # Get majority vote
    predFinal = list()
    for(k in 1:length(dtModels[[j]])){   
        predFinal[[k]] = list()
        for(j in 1:nrow(xTest)){
            predFinal[[k]][[j]] = as.numeric(names(which.max(table(pred[[k]][,j]))))
        }
    }

    # Calculate metrics
    for(k in 1:length(dtModels[[j]])){
        predF = array(as.numeric(unlist(predFinal[[k]])))
        auc[[i]][[k]] = roc.curve(xTest[,22], predF, plotit = F)$auc
    }
}

