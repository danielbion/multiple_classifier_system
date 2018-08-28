library('rpart')

path = 'C:/Projects/lista1_multiple_classifier_system'
setwd(path)
source('utils.r')

dataset = read.table('data.csv', sep=',')
numOfFolds = 10
sizeOfPool = 100
x = splitInFolds(dataset, numOfFolds)

hitRate = list()
auc = list()
gmean = list()
fmeasure = list()

decisionTree = function(x){
    return (rpart(x[, ncol(x)] ~ . -x[, ncol(x)], data = x))
}

for(i in 1:numOfFolds){
    for(j in 1:sizeOfPool){
        xTrain = x[[i]]$train
        xBagging = bagging(xTrain)
        xRandomSubspace = randomSubspace(xTrain, 0.5)
        
        xBagging50 = subset(xBagging, 0.5)
        xBagging60 = subset(xBagging, 0.6)
        xBagging70 = subset(xBagging, 0.7)
        xBagging80 = subset(xBagging, 0.8)
        xBagging90 = subset(xBagging, 0.9)

        xRandomSubspace50 = subset(xRandomSubspace, 0.5)
        xRandomSubspace60 = subset(xRandomSubspace, 0.6)
        xRandomSubspace70 = subset(xRandomSubspace, 0.7)
        xRandomSubspace80 = subset(xRandomSubspace, 0.8)
        xRandomSubspace90 = subset(xRandomSubspace, 0.9)

        decisionTree(xBagging)
        # rodar perceptron para cada dataset
        # rodar a decision tree para cada dataset
    }
    # combinar os classificadores
    # calcular as métricas
}

