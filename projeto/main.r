library('ROSE')
library('caret')
library('randomForest')
library('RWeka')

path = 'C:/Projects/lista1_multiple_classifier_system/projeto'
setwd(path)
source('utils.r')

NB = make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

experiments = function(data, classifier, rules){
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
        binsIdx = createFolds(majorityClass$target, k = numOfBins)
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

data = read.table('data3.csv' , sep=',', header=T)

files = list.files('data/')
result = list()

rules = list(majorityVote, maxRule, minRule, prodRule, sumRule)

for(i in 1:length(files)){
    print(i)
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)

    result[[i]] = list()
    result[[i]]$naiveBayes = experiments(data, NB, rules)
    result[[i]]$J48 = experiments(data, J48, rules)
    result[[i]]$JRip = experiments(data, JRip, rules)
    result[[i]]$SMO = experiments(data, SMO, rules)
    result[[i]]$IBK = experiments(data, IBk, rules)
    result[[i]]$randomForest = experiments(data, randomForest, rules)
}

