library('ROSE')
library('caret')
library('randomForest')
library('RWeka')

path = 'C:/Projects/lista1_multiple_classifier_system/projeto'
setwd(path)
source('utils.r')

NB = make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

experiments = function(data, classifier, rule){
    numOfFolds = 10
    
    x = as.data.frame(apply(data[, -ncol(data)], 2, standardize))
    x$target = data$target
    
    x = splitInFolds(x, numOfFolds)
    auc = list()

    for (i in 1:numOfFolds) {
        trainSet = x[[i]]$train
        testSet = x[[i]]$test

        majorityClass = trainSet[which(trainSet$target == 'false'), ]
        minorityClass = trainSet[which(trainSet$target == 'true'), ]

        numOfBins = floor(nrow(majorityClass) / nrow(minorityClass))

        binsIdx = createFolds(majorityClass$target, k = numOfBins)

        bins = list()
        for(j in 1:length(binsIdx)){
            bins[[j]] = rbind(majorityClass[binsIdx[[j]], ], minorityClass)
            bins[[j]] = bootstrap(bins[[j]])
        }

        pool = list()
        for(j in 1:length(bins)){
            pool[[j]] = classifier(target ~ ., bins[[j]]) #IBK
        }

        p = predictPool(pool, testSet, rule)    
        auc[[i]] = roc.curve(testSet$target, p, plotit = F)$auc
    }

    return (auc)
}

data = read.table('data3.csv', sep=',', header=TRUE)

naiveBayes_MajorityVote = experiments(data, NB, majorityVote)
J48_MajorityVote = experiments(data, J48, majorityVote)
JRip_MajorityVote = experiments(data, JRip, majorityVote)
SMO_MajorityVote = experiments(data, SMO, majorityVote)
IBK_MajorityVote = experiments(data, IBk, majorityVote)
randomForest_MajorityVote = experiments(data, randomForest, majorityVote)