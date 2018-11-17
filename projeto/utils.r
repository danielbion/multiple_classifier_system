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
    # Predict no test dataset
    target = test[, 'target']
    test = test[, -ncol(test)]

    hard = data.frame(matrix(0, length(pool), nrow(test)))
    prob1 = hard
    prob2 = hard
    for(j in 1:length(pool)){
        # Predict probability
        p = tryCatch({
                predict(pool[[j]], test, type = 'probability')
            }, 
            error = function(e) {
                predict(pool[[j]], test, type = 'prob')
            }
        )

        prob1[j, ] = p[, 1]
        prob2[j, ] = p[, 2]
        # Predict hard
        hard[j, ] = predict(pool[[j]], test)
    }
    
    pred = list()
    for(j in 1:length(target)){
        # Aplicar a regra escolhida
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

splitBal = function(majorityClass, numOfBins){
    binsIdx = createFolds(majorityClass$target, k = numOfBins)
    return (binsIdx)
}

clusterBal = function(majorityClass, numOfBins){
    kIdx = kmeans(majorityClass[, -ncol(majorityClass)], numOfBins)$cluster
    binsIdx = list()
    for(j in 1:numOfBins){
        binsIdx[[j]] = which(kIdx == j)
    }
    return (binsIdx)
}

experiments = function(data, classifier, rules, splitMethod){
    numOfFolds = 10
    
    # Padronizar variáveis explicativas entre 0 e 1
    x = as.data.frame(apply(data[, -ncol(data)], 2, standardize))
    x$target = data[, ncol(data)]
    
    # Quebrar dataset em 10 folds divididos em treino e teste
    x = splitInFolds(x, numOfFolds)
    auc = list()

    for (i in 1:numOfFolds) {
        trainSet = x[[i]]$train
        testSet = x[[i]]$test

        # Separar classe minoritária da majoritária
        majorityClass = trainSet[which(trainSet$target == 'negative'), ]
        minorityClass = trainSet[which(trainSet$target == 'positive'), ]

        # Calcular número de bins
        numOfBins = floor(nrow(majorityClass) / nrow(minorityClass))

        # Criar bins juntando a classe minoritária
        binsIdx = splitMethod(majorityClass, numOfBins)
    
        bins = list()
        for(j in 1:length(binsIdx)){
            bins[[j]] = rbind(majorityClass[binsIdx[[j]], ], minorityClass)
            bins[[j]] = bootstrap(bins[[j]])
        }

        # Treinar o classificador base escolhido para cada bin
        pool = list()
        for(j in 1:length(bins)){
            pool[[j]] = classifier(target ~ ., bins[[j]])
        }

        # Aplicar modelo no conjunto de teste para cada regra de combinação
        auc[[i]] = list()
        for(j in 1:length(rules)){
            p = predictPool(pool, testSet, rules[[j]])
            auc[[i]][[j]] = roc.curve(testSet$target, p, plotit = F)$auc
        }
    }
    return (auc)
}