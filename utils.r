splitInFolds = function(x, numOfFolds){
    # Shuffle the dataset instances
    x = x[sample(nrow(x)),]

    # Cut dataset in folds
    folds = cut(seq(1,nrow(x)), breaks=numOfFolds, labels=FALSE)

    splited = list()

    # Split each fold in train and test
    for(i in 1:numOfFolds){        
        testIndexes = which(folds==i)
        subset = list()
        subset$test = x[testIndexes, ]
        subset$train = x[-testIndexes, ]
        splited[[i]] = subset
    }

    return (splited)
}

bagging = function(x){
    numOfRows = nrow(x)
    return (x[sample(numOfRows, replace=T), ])
}

randomSubspace = function(x, percent){
    target = x[,ncol(x)]
    x = x[, -ncol(x)]
    numOfFeatures = floor(ncol(x) * percent)
    selectedFeatures = sample(numOfFeatures)
    x = cbind(x[, selectedFeatures], target)
    return (x)
}

subset = function(x, percent){
    numOfRows = floor(nrow(x) * percent)
    x = x[sample(numOfRows),]
}

decisionTree = function(x){
    return (rpart(target ~ ., data = x, method="class"))
}

perceptron = function(x){
    return(neuralnet(x[,22] ~ x[,1] + x[,2] + x[,3] + x[,4] + x[,5] + x[,6] + x[,7] + x[,8] + x[,9] 
    + x[,10] + x[,11] + x[,12] + x[,13] + x[,14] + x[,15] + x[,16] + x[,17] + x[,18] + x[,19] + x[,20] + x[,21] , data=x, hidden=0))
}

majorityVote = function(predicts){
    return (names(which.max(table(predicts))))
}

predictAndGetMetrics = function(models, test, sizeOfPool){
    # Predict on test dataset
    pred = list()
    for(i in 1:length(models[[1]])){
        pred[[i]] = matrix(0, sizeOfPool, nrow(test))
        for(j in 1:sizeOfPool){            
            p = predict(models[[j]][[i]], newdata = test)
            pred[[i]][j, ] = ifelse(p[,2] < 0.50, 'false', 'true')
        }
    }

    # Get majority vote
    predFinal = list()
    for(i in 1:length(models[[1]])){   
        predFinal[[i]] = list()
        for(j in 1:nrow(test)){
            predFinal[[i]][[j]] = majorityVote(pred[[i]][,j])
        }
    }

    # Calculate metrics
    metrics = list()

    for(i in 1:length(models[[1]])){
        predF = array(unlist(predFinal[[i]]))
        predF = factor(predF, levels = c('false','true'))

        metrics$auc = c(metrics$auc, roc.curve(test[,'target'], predF, plotit = F)$auc)
        metrics$accuracy = c(metrics$accuracy, confusionMatrix(predF, test[,'target'])$overall['Accuracy'])
        metrics$fmeasure = c(metrics$fmeasure, confusionMatrix(predF, test[,'target'])$byClass['F1'])
        metrics$gmean = c(metrics$gmean, sqrt(confusionMatrix(predF, test[,'target'])$byClass['Precision'] * confusionMatrix(test[,'target'], predF)$byClass['Recall']))
    }

    return (metrics)
}