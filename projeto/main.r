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
rules = list(majorityVote, maxRule, minRule, prodRule, sumRule, majorityVoteDistance, maxDistanceRule, minDistanceRule, prodDistanceRule, sumDistanceRule)

files = list.files('data/')
result = list()
# SplitBal
for(i in 1:length(files)){
    print(i)
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)

    result[[i]] = list()
    result[[i]]$naiveBayes = experiments(data, NB, rules, splitBal)
    result[[i]]$J48 = experiments(data, J48, rules, splitBal)
    result[[i]]$JRip = experiments(data, JRip, rules, splitBal)
    result[[i]]$SMO = experiments(data, SMO, rules, splitBal)
    result[[i]]$IBK = experiments(data, IBk, rules, splitBal)
    result[[i]]$randomForest = experiments(data, randomForest, rules, splitBal)
}
