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

        # Aplicar modelo no conjunto de teste com a regra de combinação escolhida
        p = predictPool(pool, testSet, rule)    

        # Calcular a métrica de desempenho
        auc[[i]] = roc.curve(testSet$target, p, plotit = F)$auc
    }

    return (auc)
}

files = list.files('data/')
result = list()
for(i in 1:length(files)){
    print(i)
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)

    result[[i]] = list()
    result[[i]]$naiveBayes_MajorityVote = experiments(data, NB, majorityVote)
    result[[i]]$J48_MajorityVote = experiments(data, J48, majorityVote)
    result[[i]]$JRip_MajorityVote = experiments(data, JRip, majorityVote)
    result[[i]]$SMO_MajorityVote = experiments(data, SMO, majorityVote)
    result[[i]]$IBK_MajorityVote = experiments(data, IBk, majorityVote)
    result[[i]]$randomForest_MajorityVote = experiments(data, randomForest, majorityVote)
}

