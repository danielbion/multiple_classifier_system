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

predictPool = function(pool, bins, test, d1, d2, rule){
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
        pred[[j]] = rule(list(hard = hard[, j], prob1 = prob1[, j], prob2 = prob2[, j], d1 = d1, d2 = d2))
    }
    pred = array(unlist(pred))
    pred = factor(pred, levels = c('negative','positive'))
    return(pred)
}

calcDistance = function(testInstance, bins){
    d1 = c()
    d2 = c()
    for(i in 1:length(bins)){    
        bin = bins[[i]]
        negativeIdx = which(bin$target == 'negative')
        negative = bin[negativeIdx, ]
        positive = bin[-negativeIdx, ]

        positive = data.matrix(positive[, -ncol(positive)])
        negative = data.matrix(negative[, -ncol(negative)])

        testInstance = as.numeric(testInstance)

        positiveDistance = mean(apply(positive, 1, EuclideanDistance, y = testInstance))
        negativeDistance = mean(apply(negative, 1, EuclideanDistance, y = testInstance))
        d1[i] = positiveDistance
        d2[i] = negativeDistance
    }
    return (list(d1, d2))
}

majorityVote = function(info){
    tb = table(factor(info$hard, levels=c('negative', 'positive')))
    return (names(which.max(tb)))
}

majorityVoteDistance = function(info){
    tb = table(factor(info$hard, levels=c('negative', 'positive')))
    return (names(which.max(tb/c(sum(info$d1+1), sum(info$d2+2)) )))
}

maxRule = function(info){
    return (ifelse(max(info$prob1) > max(info$prob2), 'negative', 'positive'))
}

maxDistanceRule = function(info){
    return (ifelse(max(info$prob1/(info$d1+1)) > max(info$prob2/(info$d2+1)), 'negative', 'positive'))
}

minRule = function(info){
    return (ifelse(min(info$prob1) > min(info$prob2), 'negative', 'positive'))
}

minDistanceRule = function(info){
    return (ifelse(min(info$prob1/(info$d1+1)) > min(info$prob2/(info$d2+1)), 'negative', 'positive'))
}

prodRule = function(info){
    return (ifelse(prod(info$prob1) > prod(info$prob2), 'negative', 'positive'))
}

prodDistanceRule = function(info){
    return (ifelse(prod(info$prob1/(info$d1+1)) > prod(info$prob2/(info$d2+1)), 'negative', 'positive'))
}

sumRule = function(info){
    return (ifelse(sum(info$prob1) > sum(info$prob2), 'negative', 'positive'))
}

sumDistanceRule = function(info){
    return (ifelse(sum(info$prob1/(info$d1+1)) > sum(info$prob2/(info$d2+1)), 'negative', 'positive'))
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
        print(paste("Fold ", i))
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
            #bins[[j]] = bootstrap(bins[[j]])
        }

        # Treinar o classificador base escolhido para cada bin
        pool = list()
        for(j in 1:length(bins)){
            pool[[j]] = classifier(target ~ ., bins[[j]])
        }

        d1 = matrix(0, nrow(testSet), length(bins))
        d2 = d1
        print(paste("Calc Distance ", nrow(testSet)))
        for(j in 1:nrow(testSet)){
            #print(paste(" > Calc Distance ", j, "of", nrow(testSet)))
            d = calcDistance(testSet[j, -ncol(testSet)], bins)
            d1[j, ] = d[[1]]
            d2[j, ] = d[[2]]
        }

        # Aplicar modelo no conjunto de teste para cada regra de combinação
        print("Rules")
        auc[[i]] = list()
        for(j in 1:length(rules)){
            #print(paste(" > Rules ", j, "of", length(rules)))
            p = predictPool(pool, bins, testSet, d1, d2, rules[[j]])
            auc[[i]][[j]] = roc.curve(testSet$target, p, plotit = F)$auc
        }
    }
    return (auc)
}

original = function(data, classifier, sampling){
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
      
        # Treinar o classificador base escolhido para cada bin
        c = classifier(target ~ ., trainSet)

        p = predict(c, testSet)
        auc = roc.curve(testSet$target, p, plotit = F)$auc
    }
    return (auc)
}

orig = function(data){
    return (data)
}

overSampling = function(data){
    class1 = table(data$target)[1]
    class2 = table(data$target)[2]
    
    idxs = sample(which(data$target == 'positive'), class1 - class2, replace = TRUE)
    data = rbind(data, data[idxs,])
    return(data)
}

underSampling = function(data){
    class1 = table(data$target)[1]
    class2 = table(data$target)[2]

    idxsClass1 = sample(which(data$target == 'negative'), class2, replace = TRUE)
    idxsClass2 = sample(which(data$target == 'positive'), class2)
    data = rbind(data[idxsClass1,], data[idxsClass2,])
    data = data[sample(nrow(data)),]
    return (data)
}
