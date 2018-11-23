library('ROSE')
library('caret')
library('randomForest')
library('RWeka')
library('DMwR')
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
result = list()
result$naiveBayes = c()
result$J48 = c()
result$JRip = c()
result$SMO = c()
result$IBK = c()
result$randomForest = c()
for(i in 1:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)
    
    result$naiveBayes = c(result$naiveBayes, mean(original(data, NB, orig)))
    result$J48 = c(result$J48, mean(original(data, J48, orig)))
    result$JRip = c(result$JRip, mean(original(data, JRip, orig)))
    result$randomForest = c(result$randomForest, mean(original(data, randomForest, orig)))
    result$SMO = c(result$SMO, mean(original(data, SMO, orig)))
    result$IBK = c(result$IBK, mean(original(data, IBk, orig)))
}
saveRDS(result, paste("results/orig_results.rds"))

mean(result$naiveBayes)
mean(result$J48)
mean(result$JRip)
mean(result$randomForest)
mean(result$SMO)
mean(result$IBK)

# Under Sampling
result = list()
result$naiveBayes = c()
result$J48 = c()
result$JRip = c()
result$SMO = c()
result$IBK = c()
result$randomForest = c()
for(i in 1:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)
    
    result$naiveBayes = c(result$naiveBayes, mean(original(data, NB, underSampling)))
    result$J48 = c(result$J48, mean(original(data, J48, underSampling)))
    result$JRip = c(result$JRip, mean(original(data, JRip, underSampling)))
    result$randomForest = c(result$randomForest, mean(original(data, randomForest, underSampling)))
    result$SMO = c(result$SMO, mean(original(data, SMO, underSampling)))
    result$IBK = c(result$IBK, mean(original(data, IBk, underSampling)))
}
saveRDS(result, paste("results/under_results.rds"))

mean(result$naiveBayes)
mean(result$J48)
mean(result$JRip)
mean(result$randomForest)
mean(result$SMO)
mean(result$IBK)

# Over Sampling
result = list()
result$naiveBayes = c()
result$J48 = c()
result$JRip = c()
result$SMO = c()
result$IBK = c()
result$randomForest = c()
for(i in 1:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)
    
    result$naiveBayes = c(result$naiveBayes, mean(original(data, NB, overSampling)))
    result$J48 = c(result$J48, mean(original(data, J48, overSampling)))
    result$JRip = c(result$JRip, mean(original(data, JRip, overSampling)))
    result$randomForest = c(result$randomForest, mean(original(data, randomForest, overSampling)))
    result$SMO = c(result$SMO, mean(original(data, SMO, overSampling)))
    result$IBK = c(result$IBK, mean(original(data, IBk, overSampling)))
}
saveRDS(result, paste("results/over_results.rds"))

mean(result$naiveBayes)
mean(result$J48)
mean(result$JRip)
mean(result$randomForest)
mean(result$SMO)
mean(result$IBK)

# SMOTE
result = list()
result$naiveBayes = c()
result$J48 = c()
result$JRip = c()
result$SMO = c()
result$IBK = c()
result$randomForest = c()
for(i in 1:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)
    
    result$naiveBayes = c(result$naiveBayes, mean(original(data, NB, smoted)))
    result$J48 = c(result$J48, mean(original(data, J48, smoted)))
    result$JRip = c(result$JRip, mean(original(data, JRip, smoted)))
    result$randomForest = c(result$randomForest, mean(original(data, randomForest, smoted)))
    result$SMO = c(result$SMO, mean(original(data, SMO, smoted)))
    result$IBK = c(result$IBK, mean(original(data, IBk, smoted)))
}
saveRDS(result, paste("results/smote_results.rds"))

mean(result$naiveBayes)
mean(result$J48)
mean(result$JRip)
mean(result$randomForest)
mean(result$SMO)
mean(result$IBK)


# Split + OLA
result = list()
result$naiveBayes = c()
result$J48 = c()
result$JRip = c()
result$SMO = c()
result$IBK = c()
result$randomForest = c()
for(i in 1:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)
    
    result$naiveBayes[i] = mean(experimentsProposed(data, NB, splitBal, ola))
    result$J48[i] = mean(experimentsProposed(data, J48, splitBal, ola))
    result$JRip[i] = mean(experimentsProposed(data, JRip, splitBal, ola))
    result$randomForest[i] = mean(experimentsProposed(data, randomForest, splitBal, ola))
    result$SMO[i] = mean(experimentsProposed(data, SMO, splitBal, ola))
    result$IBK[i] = mean(experimentsProposed(data, IBk, splitBal, ola))
}
saveRDS(result, paste("results/splitbal_ola_results.rds"))

mean(result$naiveBayes)
mean(result$J48)
mean(result$JRip)
mean(result$randomForest)
mean(result$SMO)
mean(result$IBK)

# Split + LCA
result = list()
result$naiveBayes = c()
result$J48 = c()
result$JRip = c()
result$SMO = c()
result$IBK = c()
result$randomForest = c()
for(i in 1:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)
    
    result$naiveBayes[i] = mean(experimentsProposed(data, NB, splitBal, lca))
    result$J48[i] = mean(experimentsProposed(data, J48, splitBal, lca))
    result$JRip[i] = mean(experimentsProposed(data, JRip, splitBal, lca))
    result$randomForest[i] = mean(experimentsProposed(data, randomForest, splitBal, lca))
    result$SMO[i] = mean(experimentsProposed(data, SMO, splitBal, lca))
    result$IBK[i] = mean(experimentsProposed(data, IBk, splitBal, lca))
}
saveRDS(result, paste("results/splitbal_lca_results.rds"))

mean(result$naiveBayes)
mean(result$J48)
mean(result$JRip)
mean(result$randomForest)
mean(result$SMO)
mean(result$IBK)

# Cluster + OLA
result = list()
result$naiveBayes = c()
result$J48 = c()
result$JRip = c()
result$SMO = c()
result$IBK = c()
result$randomForest = c()
for(i in 1:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)
    
    result$naiveBayes[i] = mean(experimentsProposed(data, NB, clusterBal, ola))
    result$J48[i] = mean(experimentsProposed(data, J48, clusterBal, ola))
    result$JRip[i] = mean(experimentsProposed(data, JRip, clusterBal, ola))
    result$randomForest[i] = mean(experimentsProposed(data, randomForest, clusterBal, ola))
    result$SMO[i] = mean(experimentsProposed(data, SMO, clusterBal, ola))
    result$IBK[i] = mean(experimentsProposed(data, IBk, clusterBal, ola))
}
saveRDS(result, paste("results/clusterBal_ola_results.rds"))

mean(result$naiveBayes)
mean(result$J48)
mean(result$JRip)
mean(result$randomForest)
mean(result$SMO)
mean(result$IBK)

# Cluster + LCA
result = list()
result$naiveBayes = c()
result$J48 = c()
result$JRip = c()
result$SMO = c()
result$IBK = c()
result$randomForest = c()
for(i in 1:length(files)){
    print(paste("Dataset", i))
    data = read.table(paste('data/', files[i], sep = ''), sep=',', header=F)
    
    result$naiveBayes[i] = mean(experimentsProposed(data, NB, clusterBal, lca))
    result$J48[i] = mean(experimentsProposed(data, J48, clusterBal, lca))
    result$JRip[i] = mean(experimentsProposed(data, JRip, clusterBal, lca))
    result$randomForest[i] = mean(experimentsProposed(data, randomForest, clusterBal, lca))
    result$SMO[i] = mean(experimentsProposed(data, SMO, clusterBal, lca))
    result$IBK[i] = mean(experimentsProposed(data, IBk, clusterBal, lca))
}
saveRDS(result, paste("results/clusterBal_lca_results.rds"))

mean(result$naiveBayes)
mean(result$J48)
mean(result$JRip)
mean(result$randomForest)
mean(result$SMO)
mean(result$IBK)