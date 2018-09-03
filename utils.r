splitInFolds = function(x, numOfFolds){
    # Cut dataset in folds
    folds = createFolds(x[,'target'], list=FALSE)

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
    x[sample(numOfRows, replace=T), ]
    rownames(x) = NULL
    return (x)
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
    return (x)
}

subsetStratified = function(x, percent){
    idx = which(x[,'target'] == 'true')
    class1 = x[idx, ]
    class2 = x[-idx, ]
    x = rbind(subset(class1, percent), subset(class2, percent))
    x = x[sample(nrow(x)),]
}

majorityVote = function(predicts){
    return (names(which.max(table(predicts))))
}

getMetrics = function(numOfModels, predMatrix, target){
     # Get majority vote
    predFinal = list()
    for(i in 1:numOfModels){   
        predFinal[[i]] = list()
        for(j in 1:length(target)){
            predFinal[[i]][[j]] = majorityVote(predMatrix[[i]][,j])
        }
    }

    # Calculate metrics
    metrics = list()

    for(i in 1:numOfModels){
        predF = array(unlist(predFinal[[i]]))
        predF = factor(predF, levels = c('false','true'))

        metrics$auc = c(metrics$auc, roc.curve(target, predF, plotit = F)$auc)
        metrics$accuracy = c(metrics$accuracy, confusionMatrix(predF, target)$overall['Accuracy'])
        metrics$fmeasure = c(metrics$fmeasure, confusionMatrix(predF, target)$byClass['F1'])
        metrics$gmean = c(metrics$gmean, sqrt(confusionMatrix(predF, target)$byClass['Precision'] * confusionMatrix(target, predF)$byClass['Recall']))
    }

    return (metrics)
}

predictDecisionTree = function(models, test, sizeOfPool){
    # Predict on test dataset
    numOfModels = length(models[[1]])
    pred = list()
    for(i in 1:numOfModels){
        pred[[i]] = matrix(0, sizeOfPool, nrow(test))
        for(j in 1:sizeOfPool){            
            p = predict(models[[j]][[i]], newdata = test)
            pred[[i]][j, ] = ifelse(p[,2] < 0.50, 'false', 'true')
        }
    }
    metrics = getMetrics(numOfModels, pred, test[,'target'])
    return (metrics)
}

predictPerceptron = function(models, test, sizeOfPool){
    # Predict on test dataset
    target = test[, 'target']
    test = test[, -ncol(test)]

    numOfModels = length(models[[1]])
    pred = list()
    for(i in 1:numOfModels){
        pred[[i]] = matrix(0, sizeOfPool, nrow(test))
        for(j in 1:sizeOfPool){
            t = test[,models[[j]][[i]][[1]]]
            pred[[i]][j, ] = perceptron.test(t, models[[j]][[i]][[2]])
        }
    }
    metrics = getMetrics(numOfModels, pred, target)
    return (metrics)
}

decisionTree = function(x){
    dtModel = rpart(target ~ ., data = x, method="class")
    return (dtModel)
}

perceptron.test = function(x, weights) {
    target = c()
    for (i in 1:nrow(x)){
        label = 'true'
        if (as.numeric(x[i,]) %*% weights[2:length(weights)] > 0){
            label = 'false'
        }
        target = c(target, label)
    }
    return (target)
}

# https://rpubs.com/FaiHas/197581
perceptron.train = function(x, eta, niter) {
    target = x[,ncol(x)]
    x = x[, -ncol(x)]    

    y = rep (1, length(target))
    y[target == "true"] = -1    
    
    # initialize weight vector
    weight <- rep(0, dim(x)[2] + 1)
    names = names(x)
    errors <- rep(0, niter)    
    
    # loop over number of epochs niter
    for (jj in 1:niter) {
            
        # loop through training data set
        for (ii in 1:length(y)) {                
            # Predict binary label using Heaviside activation 
            # function
            z <- sum(weight[2:length(weight)] * 
                                as.numeric(x[ii, ])) + weight[1]
            if(z < 0) {
                    ypred <- -1
            } else {
                    ypred <- 1
            }
            
            # Change weight - the formula doesn't do anything 
            # if the predicted value is correct
            weightdiff <- eta * (y[ii] - ypred) * 
                    c(1, as.numeric(x[ii, ]))
            weight <- weight + weightdiff
            
            # Update error function
            if ((y[ii] - ypred) != 0.0) {
                    errors[jj] <- errors[jj] + 1
            }                
        }
    }    
    # weight to decide between the two species 
    return( list(names, weight))
}
