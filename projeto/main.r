library('ROSE')
library('caret')
library('randomForest')
library('RWeka')
library('dbscan')
library('TSdist')

path = 'C:/Projects/lista1_multiple_classifier_system/projeto'
setwd(path)
source('utils.r')

NB = make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
rules = list(majorityVote, maxRule, minRule, prodRule, sumRule, 
    majorityVoteDistance, maxDistanceRule, minDistanceRule, prodDistanceRule, sumDistanceRule)

startData = 3

files = list.files('data/')
result = list()
result = readRDS(paste("results/results", startData - 1))

# SplitBal
for(i in startData:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)

    result[[i]] = list()
    print("1 - Naive Bayes")
    result[[i]]$naiveBayes = experiments(data, NB, rules, splitBal)
    print("2 - J48")
    result[[i]]$J48 = experiments(data, J48, rules, splitBal)
    print("3 - JRip")
    result[[i]]$JRip = experiments(data, JRip, rules, splitBal)
    print("4 - SMO")
    result[[i]]$SMO = experiments(data, SMO, rules, splitBal)
    print("5 - IBK")
    result[[i]]$IBK = experiments(data, IBk, rules, splitBal)
    print("6 - RandomForest")
    result[[i]]$randomForest = experiments(data, randomForest, rules, splitBal)
    saveRDS(result, paste("results/results", i, ".rds"))
}

