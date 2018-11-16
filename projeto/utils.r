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

standardize = function(x){
    return((x - min(x)) / (max(x) - min(x)))
}

bootstrap = function(x){
    numOfRows = nrow(x)
    x = x[sample(numOfRows, replace=T), ]
    rownames(x) = NULL
    return (x)
}

predictPool = function(pool, test, rule){
    # Predict on test dataset
    target = test[, 'target']
    test = test[, -ncol(test)]

    predClassifier = data.frame(matrix(0, length(pool), nrow(test)))
    for(j in 1:length(pool)){
        predClassifier[j, ] = predict(pool[[j]], test)
    }
    
    pred = list()
    for(j in 1:length(target)){
        pred[[j]] = rule(predClassifier[,j])
    }
    pred = array(unlist(pred))
    pred = factor(pred, levels = c('negative','positive'))
    return(pred)
}

majorityVote = function(predicts){
    return (names(which.max(table(predicts))))
}