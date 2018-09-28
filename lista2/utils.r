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

testPair = function(classifier1, classifier2, x){
    target = x[,ncol(x)]
    x = x[, -ncol(x)]
    n11 = 0
    n00 = 0
    n10 = 0
    n01 = 0

    for(i in 1:nrow(x)){
        x1 = x[i,classifier1[[1]]]
        pred1 = perceptron.test(x1, classifier1[[2]])

        x2 = x[i,classifier2[[1]]]
        pred2 = perceptron.test(x2, classifier2[[2]])

        if(pred1 == target[i] && pred2 == target[i]){
            n11 = n11 + 1
        }else if(pred1 == target[i] && pred2 != target[i]){
            n10 = n10 + 1
        }else if(pred1 != target[i] && pred2 == target[i]){
            n01 = n01 + 1
        }else{
            n00 = n00 + 1
        }
    }
    return (list(n11, n10, n01, n00))
}

qStatistic = function(n11, n10, n01, n00, l){
    if(n11 == l) {
        return (1)
    }
    q = ((n11 * n00) - (n01 * n10)) / ((n11 * n00) + (n01 * n10))
    return (q)
}

kappaStatistic = function(n11, n10, n01, n00, l){
    if(n11 == l) {
        return (1)
    }
    theta1 = (n11 + n00) / l
    theta2 = (((n11 + n01) * (n11 + n10)) + ((n10 + n00) * (n01 + n00))) / (l ^2)
    kappa = (theta1 - theta2) / (1 - theta2)
    return (kappa)
}

diversityMean = function(ensemble, x){
    if(length(ensemble) < 2){
        return (list(-1, -1))
    }
    q = c()
    kappa = c()
    n = nrow(x)
    l = length(ensemble)
    for(i in 1:(length(ensemble)-1)){
        print(paste("Diversity pair:", i))
        for(j in (i+1):length(ensemble)){            
            test = testPair(ensemble[[i]], ensemble[[j]], x)
            q = c(q, qStatistic(test[[1]], test[[2]], test[[3]], test[[4]], n))
            kappa = c(kappa, kappaStatistic(test[[1]], test[[2]], test[[3]], test[[4]], n))
        }
    }

    qSum = sum(q)
    qSum = 2 * qSum / (l * (l-1))
    kappaSum = sum(kappa)
    kappaSum = 2 * kappaSum / (l * (l-1))

    return (list(qSum, kappaSum))
}

reduceErrorPruning = function(pool, validationSet){
   # Sort classifiers by accuracy
    accuracy = c()
    for(i in 1:length(pool)){
        accuracy = c(accuracy, getEnsembleAccuracy(list(pool[[i]]), validationSet))
    }
    bestClassifierIdx = which.max(accuracy)
    
    ensemble = list()
    ensemble[[length(ensemble) + 1]] = pool[[bestClassifierIdx]]
    remainingIdx = 1:length(pool)
    remainingIdx = remainingIdx[-bestClassifierIdx]

    while(length(remainingIdx) > 0){
        print(paste("Reduce Error: Emsemble Size: ", length(ensemble)))
        currentAccuracy = getEnsembleAccuracy(ensemble, validationSet)

        newAccuracy = c()
        for(i in 1:length(remainingIdx)){
            testEnsemble = ensemble
            testEnsemble[[length(testEnsemble) + 1]] = pool[[remainingIdx[i]]]
            newAccuracy = c(newAccuracy, getEnsembleAccuracy(testEnsemble, validationSet))
        }
        
        bestClassifierIdx = which.max(newAccuracy)
        newAccuracy = max(newAccuracy)
        if((newAccuracy > currentAccuracy) || (newAccuracy >= currentAccuracy && length(ensemble) < 3)){
            ensemble[[length(ensemble) + 1]] = pool[[remainingIdx[bestClassifierIdx]]]
            remainingIdx = remainingIdx[-bestClassifierIdx]
        }else{
            break
        }
    }
    return (ensemble)
}

reduceErrorPruningOrdered = function(pool, validationSet){
   # Sort classifiers by accuracy
    accuracy = c()
    for(i in 1:length(pool)){
        accuracy = c(accuracy, getEnsembleAccuracy(list(pool[[i]]), validationSet))
    }
    accuracyIdx = sort(accuracy, decreasing = TRUE,index.return=TRUE)$ix

    ensemble = list()
    ensemble[[length(ensemble) + 1]] = pool[[accuracyIdx[1]]]
    accuracyIdx = accuracyIdx[-1]

    while(length(accuracyIdx) > 0){
        print(paste("Reduce Error: Emsemble Size: ", length(ensemble)))
        currentAccuracy = getEnsembleAccuracy(ensemble, validationSet)

        newAccuracy = c()
        for(i in 1:length(accuracyIdx)){
            testEnsemble = ensemble
            testEnsemble[[length(testEnsemble) + 1]] = pool[[accuracyIdx[i]]]
            newAccuracy = c(newAccuracy, getEnsembleAccuracy(testEnsemble, validationSet))
        }
        
        bestClassifierIdx = which.max(newAccuracy)
        newAccuracy = max(newAccuracy)
        if(newAccuracy > currentAccuracy){
            ensemble[[length(ensemble) + 1]] = pool[[accuracyIdx[bestClassifierIdx]]]
            accuracyIdx = accuracyIdx[-bestClassifierIdx]
        }else{
            break
        }
    }
    return (ensemble)
}

bestFirstPruning = function(pool, validationSet){
    # Sort classifiers by accuracy
    accuracy = c()
    for(i in 1:length(pool)){
        accuracy = c(accuracy, getEnsembleAccuracy(list(pool[[i]]), validationSet))
    }
    accuracyIdx = sort(accuracy, decreasing = TRUE,index.return=TRUE)$ix

    # Mount the ensembles
    ensembles = list()
    accuracy = c()    
    for(i in 1:5){#length(pool)){
        print(paste("Best First: Emsemble ", i))
        if(i == 1){
            ensembles[[i]] = list(pool[[accuracyIdx[i]]])
        }else{
            ensembles[[i]] = append(ensembles[[i-1]], list(pool[[accuracyIdx[i]]]))
        }
        accuracy = c(accuracy, getEnsembleAccuracy(ensembles[[i]], validationSet))
    }
    # plot(accuracy, ylim=c(0,1), type='l', main='Best First Pruning', xlab='Number of Classifiers on Ensamble', ylab='Accuracy')

    bestEnsemble = ensembles[[max(which(accuracy == max(accuracy)))]]
    # bestEnsemble = ensembles[[which.max(accuracy)]]

    return (bestEnsemble)
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