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

startData = 1

files = list.files('data/')
result = list()
result = readRDS(paste("results/results", startData - 1, ".rds"))

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

startData = 1
# ClusterBal
result = list()
for(i in startData:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)

    result[[i]] = list()
    print("1 - Naive Bayes")
    result[[i]]$naiveBayes = experiments(data, NB, rules, clusterBal)
    print("2 - J48")
    result[[i]]$J48 = experiments(data, J48, rules, clusterBal)
    print("3 - JRip")
    result[[i]]$JRip = experiments(data, JRip, rules, clusterBal)
    print("4 - SMO")
    result[[i]]$SMO = experiments(data, SMO, rules, clusterBal)
    print("5 - IBK")
    result[[i]]$IBK = experiments(data, IBk, rules, clusterBal)
    print("6 - RandomForest")
    result[[i]]$randomForest = experiments(data, randomForest, rules, clusterBal)
    saveRDS(result, paste("results/results", i, ".rds"))
}

# Média de AUC de cada fold de cada dataset
naiveBayes = rep(0, length(rules))
J48 = rep(0, length(rules))
JRip = rep(0, length(rules))
SMO = rep(0, length(rules))
IBK = rep(0, length(rules))
randomForest = rep(0, length(rules))

for(i in 1:length(result)){
    for(j in 1:length(rules)){
        naiveBayes[j] = naiveBayes[j] + mean(unlist(result[[i]]$naiveBayes[[j]]))
        J48[j] = J48[j] + mean(unlist(result[[i]]$J48[[j]]))
        JRip[j] = JRip[j] + mean(unlist(result[[i]]$JRip[[j]]))
        SMO[j] = SMO[j] + mean(unlist(result[[i]]$SMO[[j]]))
        IBK[j] = IBK[j] + mean(unlist(result[[i]]$IBK[[j]]))
        randomForest[j] = randomForest[j] + mean(unlist(result[[i]]$randomForest[[j]]))
    }
}
naiveBayes = naiveBayes / length(result)
J48 = J48 / length(result)
JRip = JRip / length(result)
randomForest = randomForest / length(result)
SMO = SMO / length(result)
IBK = IBK / length(result)

# Ranking
calcRank = function(result){
    aux = sort(result, decreasing = T, index.return=TRUE)$ix
    rank = c()
    for(i in 1:length(aux)){
        rank[aux[i]] = i
    }
    return (rank)
}
naiveBayesRank = calcRank(naiveBayes)
J48Rank = calcRank(J48)
JRipRank = calcRank(JRip)
randomForestRank = calcRank(randomForest)
SMORank = calcRank(SMO)
IBKRank = calcRank(IBK)


# External Methods
# Original Dataset
startData = 1
result = list()
for(i in startData:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)
    
    result[[i]] = list()
    print("1 - Naive Bayes")
    result[[i]]$naiveBayes = original(data, NB, orig)
    print("2 - J48")
    result[[i]]$J48 = original(data, J48, orig)
    print("3 - JRip")
    result[[i]]$JRip = original(data, JRip, orig)
    print("4 - SMO")
    result[[i]]$SMO = original(data, SMO, orig)
    print("5 - IBK")
    result[[i]]$IBK = original(data, IBk, orig)
    print("6 - RandomForest")
    result[[i]]$randomForest = original(data, randomForest, orig)
}
saveRDS(result, paste("results/orig_results.rds"))
naiveBayes = 0
J48 = 0
JRip = 0
SMO = 0
IBK = 0
randomForest = 0

for(i in 1:length(result)){
    naiveBayes = naiveBayes + result[[i]]$naiveBayes
    J48 = J48 + result[[i]]$J48
    JRip = JRip + result[[i]]$JRip
    SMO = SMO + result[[i]]$SMO
    IBK = IBK + result[[i]]$IBK
    randomForest = randomForest + result[[i]]$randomForest
}
naiveBayes = naiveBayes / length(result)
J48 = J48 / length(result)
JRip = JRip / length(result)
randomForest = randomForest / length(result)
SMO = SMO / length(result)
IBK = IBK / length(result)

# Under Sampling
startData = 1
result = list()
for(i in startData:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)
    
    result[[i]] = list()
    print("1 - Naive Bayes")
    result[[i]]$naiveBayes = original(data, NB, underSampling)
    print("2 - J48")
    result[[i]]$J48 = original(data, J48, underSampling)
    print("3 - JRip")
    result[[i]]$JRip = original(data, JRip, underSampling)
    print("4 - SMO")
    result[[i]]$SMO = original(data, SMO, underSampling)
    print("5 - IBK")
    result[[i]]$IBK = original(data, IBk, underSampling)
    print("6 - RandomForest")
    result[[i]]$randomForest = original(data, randomForest, underSampling)
}
saveRDS(result, paste("results/under_results.rds"))
naiveBayes = 0
J48 = 0
JRip = 0
SMO = 0
IBK = 0
randomForest = 0
for(i in 1:length(result)){
    naiveBayes = naiveBayes + result[[i]]$naiveBayes
    J48 = J48 + result[[i]]$J48
    JRip = JRip + result[[i]]$JRip
    SMO = SMO + result[[i]]$SMO
    IBK = IBK + result[[i]]$IBK
    randomForest = randomForest + result[[i]]$randomForest
}
naiveBayes = naiveBayes / length(result)
J48 = J48 / length(result)
JRip = JRip / length(result)
randomForest = randomForest / length(result)
SMO = SMO / length(result)
IBK = IBK / length(result)

# Over Sampling
startData = 1
result = list()
for(i in startData:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)
    
    result[[i]] = list()
    print("1 - Naive Bayes")
    result[[i]]$naiveBayes = original(data, NB, overSampling)
    print("2 - J48")
    result[[i]]$J48 = original(data, J48, overSampling)
    print("3 - JRip")
    result[[i]]$JRip = original(data, JRip, overSampling)
    print("4 - SMO")
    result[[i]]$SMO = original(data, SMO, overSampling)
    print("5 - IBK")
    result[[i]]$IBK = original(data, IBk, overSampling)
    print("6 - RandomForest")
    result[[i]]$randomForest = original(data, randomForest, overSampling)
}
saveRDS(result, paste("results/over_results.rds"))

naiveBayes = 0
J48 = 0
JRip = 0
SMO = 0
IBK = 0
randomForest = 0

for(i in 1:length(result)){
    naiveBayes = naiveBayes + result[[i]]$naiveBayes
    J48 = J48 + result[[i]]$J48
    JRip = JRip + result[[i]]$JRip
    SMO = SMO + result[[i]]$SMO
    IBK = IBK + result[[i]]$IBK
    randomForest = randomForest + result[[i]]$randomForest
}
naiveBayes = naiveBayes / length(result)
J48 = J48 / length(result)
JRip = JRip / length(result)
randomForest = randomForest / length(result)
SMO = SMO / length(result)
IBK = IBK / length(result)