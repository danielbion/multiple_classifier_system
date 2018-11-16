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

    hard = data.frame(matrix(0, length(pool), nrow(test)))
    prob1 = hard
    prob2 = hard
    for(j in 1:length(pool)){
        p = predict(pool[[j]], test, type = 'probability')
        prob1[j, ] = p[, 1]
        prob2[j, ] = p[, 2]
        hard[j, ] = predict(pool[[j]], test)
    }
    
    pred = list()
    for(j in 1:length(target)){
        pred[[j]] = rule(hard[, j], prob1[, j], prob2[, j])
    }
    pred = array(unlist(pred))
    pred = factor(pred, levels = c('negative','positive'))
    return(pred)
}

majorityVote = function(hard, prob1, prob2){
    return (names(which.max(table(hard))))
}

maxRule = function(hard, prob1, prob2){
    return (ifelse(max(prob1) > max(prob2), 'negative', 'positive'))
}

minRule = function(hard, prob1, prob2){
    return (ifelse(min(prob1) > min(prob2), 'negative', 'positive'))
}

prodRule = function(hard, prob1, prob2){
    return (ifelse(prod(prob1) > prod(prob2), 'negative', 'positive'))
}

sumRule = function(hard, prob1, prob2){
    return (ifelse(sum(prob1) > sum(prob2), 'negative', 'positive'))
}