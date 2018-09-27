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

bestFirstPruning = function(pool, validationSet){
    accuracy = c()
    for(i in 1:length(pool)){
        accuracy = c(accuracy, getEnsembleAccuracy(list(pool[[i]]), validationSet))
    }
    
    # Insert best classifier first
    accuracyIdx = sort(accuracy, decreasing = TRUE,index.return=TRUE)$ix
    ensemble = list()
    ensemble[[length(ensemble) + 1]] = pool[[accuracyIdx[1]]]
    accuracyIdx = accuracyIdx[-1]

    while(length(accuracyIdx) > 0){
        testEnsemble = ensemble
        testEnsemble[[length(testEnsemble) + 1]] = pool[[accuracyIdx[1]]]
        currentAccuracy = getEnsembleAccuracy(ensemble, validationSet)
        newAccuracy = getEnsembleAccuracy(testEnsemble, validationSet)
        if(newAccuracy >= currentAccuracy){
            ensemble = testEnsemble
            accuracyIdx = accuracyIdx[-1]
        }else{
            break
        }    
    }
    return (ensemble)
}

# http://www.cin.ufpe.br/~tg/2017-1/fnw_tg.pdf
kdn = function(x, k){
    target = x[,ncol(x)]
    x = x[, -ncol(x)]

    neighboorsIdx = kNN(x, k)$id
    kdnValues = c()
    for (i in 1:nrow(x)){
        ## Calculate KDN 
        disagreeingNeighboors = length(which(target[neighboorsIdx[i,]] != target[i]))
        kdnValues = c(kdnValues, disagreeingNeighboors/k)
    }
    return (kdnValues)
}

bagging = function(x){
    numOfRows = nrow(x)
    x = x[sample(numOfRows, replace=T), ]
    rownames(x) = NULL
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

getMetrics = function(predMatrix, target){
     # Get majority vote
    predFinal = list()
    for(j in 1:length(target)){
        predFinal[[j]] = majorityVote(predMatrix[,j])
    }

    # Calculate metrics
    metrics = list()

    predF = array(unlist(predFinal))
    predF = factor(predF, levels = c('false','true'))

    metrics$auc = roc.curve(target, predF, plotit = F)$auc
    metrics$accuracy = confusionMatrix(predF, target)$overall['Accuracy']
    metrics$fmeasure = confusionMatrix(predF, target)$byClass['F1']
    metrics$gmean = sqrt(confusionMatrix(predF, target)$byClass['Precision'] * confusionMatrix(target, predF)$byClass['Recall'])

    return (metrics)
}

getEnsembleAccuracy = function(ensemble, data){
    # Predict on data dataset
    target = data[, 'target']
    data = data[, -ncol(data)]

    pred = matrix(0, length(ensemble), nrow(data))
    for(j in 1:length(ensemble)){
        t = data[,ensemble[[j]][[1]]]
        pred[j, ] = perceptron.test(t, ensemble[[j]][[2]])
    }
    
    predFinal = list()
    for(j in 1:length(target)){
        predFinal[[j]] = majorityVote(pred[,j])
    }

    predF = array(unlist(predFinal))
    predF = factor(predF, levels = c('false','true'))

    accuracy = confusionMatrix(predF, target)$overall['Accuracy']
    return (accuracy)
}

predictPerceptron = function(models, test){
    # Predict on test dataset
    target = test[, 'target']
    test = test[, -ncol(test)]

    pred = matrix(0, length(models), nrow(test))
    for(j in 1:length(models)){
        t = test[,models[[j]][[1]]]
        pred[j, ] = perceptron.test(t, models[[j]][[2]])        
    }
    metrics = getMetrics(pred, target)
    return (metrics)
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