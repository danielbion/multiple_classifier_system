library('rpart')
library('ROSE')
library('caret')
library('dbscan')
library('naivebayes')
library('randomForest')
library('RWeka')

path = 'C:/Projects/lista1_multiple_classifier_system/projeto'
setwd(path)
source('utils.r')

NB = make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

data = read.table('data3.csv', sep=',', header=TRUE)
numOfFolds = 10

x = splitInFolds(data, numOfFolds)

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

    for(j in 1:length(bins)){
        #model = naive_bayes(bins[[j]], bins[[j]]$target)
        #model = NB(target ~ ., bins[[j]]) # Naive Bayes
        #model = J48(target ~ ., bins[[j]]) # C4.5
        #model = JRip(target ~ ., bins[[j]]) # Ripper
        #model = SMO(target ~ ., bins[[j]]) # SMO
        #model = IBk(target ~ ., bins[[j]]) #IBK
        model = randomForest(target ~ ., bins[[j]]) #IBK
        model = randomForest(bins[[j]], bins[[j]]$target) #IBK

        p = predict(model, testSet)
        print(table(p, testSet$target))
    }
}