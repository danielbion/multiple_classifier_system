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
    V22 = x[,ncol(x)]
    x = x[, -ncol(x)]
    numOfFeatures = floor(ncol(x) * percent)
    selectedFeatures = sample(numOfFeatures)
    x = cbind(x[, selectedFeatures], V22)
    return (x)
}

subset = function(x, percent){
    numOfRows = floor(nrow(x) * percent)
    x = x[sample(numOfRows),]
}