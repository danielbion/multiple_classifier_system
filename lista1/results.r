data1_dtMetrics = readRDS("data1_dtMetrics.rds")
data1_perceptronMetrics = readRDS("data1_perceptronMetrics.rds")
data2_dtMetrics = readRDS("data2_dtMetrics.rds")
data2_perceptronMetrics = readRDS("data2_perceptronMetrics.rds")

results.metric = function(metrics, name, index){
    metric = c()
    for(i in 1:length(metrics)){
        value = get(name, metrics[[i]])[index]
        if(is.na(value)){
            value = 0.01
        }
        metric = c(metric, value)
    }    
    return (metric)
}

# Dataset 1:

# Bagging + Decision Tree metrics
data1_bagging50_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 1)
data1_bagging60_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 3)
data1_bagging70_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 5)
data1_bagging80_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 7)
data1_bagging90_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 9)
data1_bagging100_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 11)

data1_bagging50_dt_auc = results.metric(data1_dtMetrics, 'auc', 1)
data1_bagging60_dt_auc = results.metric(data1_dtMetrics, 'auc', 3)
data1_bagging70_dt_auc = results.metric(data1_dtMetrics, 'auc', 5)
data1_bagging80_dt_auc = results.metric(data1_dtMetrics, 'auc', 7)
data1_bagging90_dt_auc = results.metric(data1_dtMetrics, 'auc', 9)
data1_bagging100_dt_auc = results.metric(data1_dtMetrics, 'auc', 11)

data1_bagging50_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 1)
data1_bagging60_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 3)
data1_bagging70_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 5)
data1_bagging80_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 7)
data1_bagging90_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 9)
data1_bagging100_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 11)

data1_bagging50_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 1)
data1_bagging60_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 3)
data1_bagging70_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 5)
data1_bagging80_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 7)
data1_bagging90_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 9)
data1_bagging100_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 11)


# Random subspace + Decision Tree metrics
data1_randomSubspace50_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 2)
data1_randomSubspace60_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 4)
data1_randomSubspace70_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 6)
data1_randomSubspace80_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 8)
data1_randomSubspace90_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 10)
data1_randomSubspace100_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 12)

data1_randomSubspace50_dt_auc = results.metric(data1_dtMetrics, 'auc', 2)
data1_randomSubspace60_dt_auc = results.metric(data1_dtMetrics, 'auc', 4)
data1_randomSubspace70_dt_auc = results.metric(data1_dtMetrics, 'auc', 6)
data1_randomSubspace80_dt_auc = results.metric(data1_dtMetrics, 'auc', 8)
data1_randomSubspace90_dt_auc = results.metric(data1_dtMetrics, 'auc', 10)
data1_randomSubspace100_dt_auc = results.metric(data1_dtMetrics, 'auc', 12)

data1_randomSubspace50_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 2)
data1_randomSubspace60_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 4)
data1_randomSubspace70_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 6)
data1_randomSubspace80_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 8)
data1_randomSubspace90_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 10)
data1_randomSubspace100_dt_gmean = results.metric(data1_dtMetrics, 'gmean', 12)

data1_randomSubspace50_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 2)
data1_randomSubspace60_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 4)
data1_randomSubspace70_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 6)
data1_randomSubspace80_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 8)
data1_randomSubspace90_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 10)
data1_randomSubspace100_dt_fmeasure = results.metric(data1_dtMetrics, 'fmeasure', 12)


# Bagging + Perceptron metrics
data1_bagging50_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 1)
data1_bagging60_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 3)
data1_bagging70_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 5)
data1_bagging80_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 7)
data1_bagging90_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 9)
data1_bagging100_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 11)

data1_bagging50_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 1)
data1_bagging60_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 3)
data1_bagging70_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 5)
data1_bagging80_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 7)
data1_bagging90_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 9)
data1_bagging100_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 11)

data1_bagging50_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 1)
data1_bagging60_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 3)
data1_bagging70_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 5)
data1_bagging80_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 7)
data1_bagging90_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 9)
data1_bagging100_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 11)

data1_bagging50_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 1)
data1_bagging60_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 3)
data1_bagging70_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 5)
data1_bagging80_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 7)
data1_bagging90_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 9)
data1_bagging100_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 11)


# Random subspace + Perceptron Tree metrics
data1_randomSubspace50_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 2)
data1_randomSubspace60_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 4)
data1_randomSubspace70_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 6)
data1_randomSubspace80_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 8)
data1_randomSubspace90_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 10)
data1_randomSubspace100_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 12)

data1_randomSubspace50_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 2)
data1_randomSubspace60_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 4)
data1_randomSubspace70_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 6)
data1_randomSubspace80_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 8)
data1_randomSubspace90_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 10)
data1_randomSubspace100_perceptron_auc = results.metric(data1_perceptronMetrics, 'auc', 12)

data1_randomSubspace50_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 2)
data1_randomSubspace60_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 4)
data1_randomSubspace70_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 6)
data1_randomSubspace80_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 8)
data1_randomSubspace90_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 10)
data1_randomSubspace100_perceptron_gmean = results.metric(data1_perceptronMetrics, 'gmean', 12)

data1_randomSubspace50_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 2)
data1_randomSubspace60_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 4)
data1_randomSubspace70_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 6)
data1_randomSubspace80_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 8)
data1_randomSubspace90_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 10)
data1_randomSubspace100_perceptron_fmeasure = results.metric(data1_perceptronMetrics, 'fmeasure', 12)


# Dataset 2:

# Bagging + Decision Tree metrics
data2_bagging50_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 1)
data2_bagging60_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 3)
data2_bagging70_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 5)
data2_bagging80_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 7)
data2_bagging90_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 9)
data2_bagging100_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 11)

data2_bagging50_dt_auc = results.metric(data2_dtMetrics, 'auc', 1)
data2_bagging60_dt_auc = results.metric(data2_dtMetrics, 'auc', 3)
data2_bagging70_dt_auc = results.metric(data2_dtMetrics, 'auc', 5)
data2_bagging80_dt_auc = results.metric(data2_dtMetrics, 'auc', 7)
data2_bagging90_dt_auc = results.metric(data2_dtMetrics, 'auc', 9)
data2_bagging100_dt_auc = results.metric(data2_dtMetrics, 'auc', 11)

data2_bagging50_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 1)
data2_bagging60_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 3)
data2_bagging70_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 5)
data2_bagging80_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 7)
data2_bagging90_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 9)
data2_bagging100_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 11)

data2_bagging50_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 1)
data2_bagging60_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 3)
data2_bagging70_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 5)
data2_bagging80_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 7)
data2_bagging90_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 9)
data2_bagging100_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 11)

# Random subspace + Decision Tree metrics
data2_randomSubspace50_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 2)
data2_randomSubspace60_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 4)
data2_randomSubspace70_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 6)
data2_randomSubspace80_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 8)
data2_randomSubspace90_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 10)
data2_randomSubspace100_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 12)

data2_randomSubspace50_dt_auc = results.metric(data2_dtMetrics, 'auc', 2)
data2_randomSubspace60_dt_auc = results.metric(data2_dtMetrics, 'auc', 4)
data2_randomSubspace70_dt_auc = results.metric(data2_dtMetrics, 'auc', 6)
data2_randomSubspace80_dt_auc = results.metric(data2_dtMetrics, 'auc', 8)
data2_randomSubspace90_dt_auc = results.metric(data2_dtMetrics, 'auc', 10)
data2_randomSubspace100_dt_auc = results.metric(data2_dtMetrics, 'auc', 12)

data2_randomSubspace50_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 2)
data2_randomSubspace60_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 4)
data2_randomSubspace70_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 6)
data2_randomSubspace80_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 8)
data2_randomSubspace90_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 10)
data2_randomSubspace100_dt_gmean = results.metric(data2_dtMetrics, 'gmean', 12)

data2_randomSubspace50_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 2)
data2_randomSubspace60_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 4)
data2_randomSubspace70_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 6)
data2_randomSubspace80_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 8)
data2_randomSubspace90_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 10)
data2_randomSubspace100_dt_fmeasure = results.metric(data2_dtMetrics, 'fmeasure', 12)

# Bagging + Perceptron metrics
data2_bagging50_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 1)
data2_bagging60_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 3)
data2_bagging70_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 5)
data2_bagging80_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 7)
data2_bagging90_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 9)
data2_bagging100_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 11)

data2_bagging50_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 1)
data2_bagging60_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 3)
data2_bagging70_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 5)
data2_bagging80_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 7)
data2_bagging90_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 9)
data2_bagging100_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 11)

data2_bagging50_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 1)
data2_bagging60_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 3)
data2_bagging70_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 5)
data2_bagging80_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 7)
data2_bagging90_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 9)
data2_bagging100_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 11)

data2_bagging50_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 1)
data2_bagging60_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 3)
data2_bagging70_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 5)
data2_bagging80_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 7)
data2_bagging90_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 9)
data2_bagging100_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 11)

# Random subspace + Perceptron Tree metrics
data2_randomSubspace50_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 2)
data2_randomSubspace60_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 4)
data2_randomSubspace70_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 6)
data2_randomSubspace80_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 8)
data2_randomSubspace90_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 10)
data2_randomSubspace100_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 12)

data2_randomSubspace50_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 2)
data2_randomSubspace60_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 4)
data2_randomSubspace70_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 6)
data2_randomSubspace80_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 8)
data2_randomSubspace90_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 10)
data2_randomSubspace100_perceptron_auc = results.metric(data2_perceptronMetrics, 'auc', 12)

data2_randomSubspace50_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 2)
data2_randomSubspace60_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 4)
data2_randomSubspace70_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 6)
data2_randomSubspace80_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 8)
data2_randomSubspace90_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 10)
data2_randomSubspace100_perceptron_gmean = results.metric(data2_perceptronMetrics, 'gmean', 12)

data2_randomSubspace50_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 2)
data2_randomSubspace60_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 4)
data2_randomSubspace70_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 6)
data2_randomSubspace80_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 8)
data2_randomSubspace90_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 10)
data2_randomSubspace100_perceptron_fmeasure = results.metric(data2_perceptronMetrics, 'fmeasure', 12)



# PORCENTAGEM vs PORCENTAGEM
# H0: O desempenho dos métodos no conjunto de teste se mantém igual ao mudar a porcentagem do conjunto de treinamento
# H1: O desempenho dos métodos são diferentes

# P > 0.05 não rejeita
# P < 0.05 rejeita

# Bagging + decision tree
# p-value = 0.6269 - Não rejeita, não existe diferença
friedman_data1_bagging_dt_accuracy = friedman.test(cbind(
    data1_bagging50_dt_accuracy, 
    data1_bagging60_dt_accuracy, 
    data1_bagging70_dt_accuracy, 
    data1_bagging80_dt_accuracy,
    data1_bagging90_dt_accuracy,
    data1_bagging100_dt_accuracy
))

# p-value = 0.1026 - Não rejeita, não existe diferença
friedman_data1_bagging_dt_gmean = friedman.test(cbind(
    data1_bagging50_dt_gmean, 
    data1_bagging60_dt_gmean, 
    data1_bagging70_dt_gmean, 
    data1_bagging80_dt_gmean,
    data1_bagging90_dt_gmean,
    data1_bagging100_dt_gmean
))

# p-value = 0.3208 - Não rejeita, não existe diferença
friedman_data1_bagging_dt_auc = friedman.test(cbind(
    data1_bagging50_dt_auc, 
    data1_bagging60_dt_auc, 
    data1_bagging70_dt_auc, 
    data1_bagging80_dt_auc,
    data1_bagging90_dt_auc,
    data1_bagging100_dt_auc
))

# p-value = 0.3365 - Não rejeita, não existe diferença
friedman_data1_bagging_dt_fmeasure = friedman.test(cbind(
    data1_bagging50_dt_fmeasure, 
    data1_bagging60_dt_fmeasure, 
    data1_bagging70_dt_fmeasure, 
    data1_bagging80_dt_fmeasure,
    data1_bagging90_dt_fmeasure,
    data1_bagging100_dt_fmeasure
))


# Random Subspace + decision tree
# p-value = 0.6269 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_dt_accuracy = friedman.test(cbind(
    data1_randomSubspace50_dt_accuracy, 
    data1_randomSubspace60_dt_accuracy, 
    data1_randomSubspace70_dt_accuracy, 
    data1_randomSubspace80_dt_accuracy,
    data1_randomSubspace90_dt_accuracy,
    data1_randomSubspace100_dt_accuracy
))

# p-value = 0.3293 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_dt_gmean = friedman.test(cbind(
    data1_randomSubspace50_dt_gmean, 
    data1_randomSubspace60_dt_gmean, 
    data1_randomSubspace70_dt_gmean, 
    data1_randomSubspace80_dt_gmean,
    data1_randomSubspace90_dt_gmean,
    data1_randomSubspace100_dt_gmean
))

# p-value = 0.8809 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_dt_auc = friedman.test(cbind(
    data1_randomSubspace50_dt_auc, 
    data1_randomSubspace60_dt_auc, 
    data1_randomSubspace70_dt_auc, 
    data1_randomSubspace80_dt_auc,
    data1_randomSubspace90_dt_auc,
    data1_randomSubspace100_dt_auc
))

# p-value = 0.5308 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_dt_fmeasure = friedman.test(cbind(
    data1_randomSubspace50_dt_fmeasure, 
    data1_randomSubspace60_dt_fmeasure, 
    data1_randomSubspace70_dt_fmeasure, 
    data1_randomSubspace80_dt_fmeasure,
    data1_randomSubspace90_dt_fmeasure,
    data1_randomSubspace100_dt_fmeasure
))


# Bagging + Perceptron
# p-value = 0.2149 - Não rejeita, não existe diferença
friedman_data1_bagging_perceptron_accuracy = friedman.test(cbind(
    data1_bagging50_perceptron_accuracy, 
    data1_bagging60_perceptron_accuracy, 
    data1_bagging70_perceptron_accuracy, 
    data1_bagging80_perceptron_accuracy,
    data1_bagging90_perceptron_accuracy,
    data1_bagging100_perceptron_accuracy
))

# p-value = 0.03035 - Rejeita
friedman_data1_bagging_perceptron_gmean = friedman.test(cbind(
    data1_bagging50_perceptron_gmean, 
    data1_bagging60_perceptron_gmean, 
    data1_bagging70_perceptron_gmean, 
    data1_bagging80_perceptron_gmean,
    data1_bagging90_perceptron_gmean,
    data1_bagging100_perceptron_gmean
))

# p-value = 0.154 - Não rejeita, não existe diferença
friedman_data1_bagging_perceptron_auc = friedman.test(cbind(
    data1_bagging50_perceptron_auc, 
    data1_bagging60_perceptron_auc, 
    data1_bagging70_perceptron_auc, 
    data1_bagging80_perceptron_auc,
    data1_bagging90_perceptron_auc,
    data1_bagging100_perceptron_auc
))

# p-value = 0.2195 - Não rejeita, não existe diferença
friedman_data1_bagging_perceptron_fmeasure = friedman.test(cbind(
    data1_bagging50_perceptron_fmeasure, 
    data1_bagging60_perceptron_fmeasure, 
    data1_bagging70_perceptron_fmeasure, 
    data1_bagging80_perceptron_fmeasure,
    data1_bagging90_perceptron_fmeasure,
    data1_bagging100_perceptron_fmeasure
))


# Random Subspace + Perceptron
# p-value = 0.1603 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_perceptron_accuracy = friedman.test(cbind(
    data1_randomSubspace50_perceptron_accuracy, 
    data1_randomSubspace60_perceptron_accuracy, 
    data1_randomSubspace70_perceptron_accuracy, 
    data1_randomSubspace80_perceptron_accuracy,
    data1_randomSubspace90_perceptron_accuracy,
    data1_randomSubspace100_perceptron_accuracy
))

# p-value = 0.2626 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_perceptron_gmean = friedman.test(cbind(
    data1_randomSubspace50_perceptron_gmean, 
    data1_randomSubspace60_perceptron_gmean, 
    data1_randomSubspace70_perceptron_gmean, 
    data1_randomSubspace80_perceptron_gmean,
    data1_randomSubspace90_perceptron_gmean,
    data1_randomSubspace100_perceptron_gmean
))

# p-value = 0.03024 - Rejeita
friedman_data1_randomSubspace_perceptron_auc = friedman.test(cbind(
    data1_randomSubspace50_perceptron_auc, 
    data1_randomSubspace60_perceptron_auc, 
    data1_randomSubspace70_perceptron_auc, 
    data1_randomSubspace80_perceptron_auc,
    data1_randomSubspace90_perceptron_auc,
    data1_randomSubspace100_perceptron_auc
))

# p-value = 0.5308 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_perceptron_fmeasure = friedman.test(cbind(
    data1_randomSubspace50_perceptron_fmeasure, 
    data1_randomSubspace60_perceptron_fmeasure, 
    data1_randomSubspace70_perceptron_fmeasure, 
    data1_randomSubspace80_perceptron_fmeasure,
    data1_randomSubspace90_perceptron_fmeasure,
    data1_randomSubspace100_perceptron_fmeasure
))

# Bagging + decision tree
# p-value = 0.6487 - Não rejeita, não existe diferença
friedman_data2_bagging_dt_accuracy = friedman.test(cbind(
    data2_bagging50_dt_accuracy, 
    data2_bagging60_dt_accuracy, 
    data2_bagging70_dt_accuracy, 
    data2_bagging80_dt_accuracy,
    data2_bagging90_dt_accuracy,
    data2_bagging100_dt_accuracy
))

# p-value = 0.8238 - Não rejeita, não existe diferença
friedman_data2_bagging_dt_gmean = friedman.test(cbind(
    data2_bagging50_dt_gmean, 
    data2_bagging60_dt_gmean, 
    data2_bagging70_dt_gmean, 
    data2_bagging80_dt_gmean,
    data2_bagging90_dt_gmean,
    data2_bagging100_dt_gmean
))

# p-value = 0.03033 - Rejeita
friedman_data2_bagging_dt_auc = friedman.test(cbind(
    data2_bagging50_dt_auc, 
    data2_bagging60_dt_auc, 
    data2_bagging70_dt_auc, 
    data2_bagging80_dt_auc,
    data2_bagging90_dt_auc,
    data2_bagging100_dt_auc
))

# p-value = 0.5822 - Não rejeita, não existe diferença
friedman_data2_bagging_dt_fmeasure = friedman.test(cbind(
    data2_bagging50_dt_fmeasure, 
    data2_bagging60_dt_fmeasure, 
    data2_bagging70_dt_fmeasure, 
    data2_bagging80_dt_fmeasure,
    data2_bagging90_dt_fmeasure,
    data2_bagging100_dt_fmeasure
))


# Random Subspace + decision tree
# p-value = 0.1606 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_dt_accuracy = friedman.test(cbind(
    data2_randomSubspace50_dt_accuracy, 
    data2_randomSubspace60_dt_accuracy, 
    data2_randomSubspace70_dt_accuracy, 
    data2_randomSubspace80_dt_accuracy,
    data2_randomSubspace90_dt_accuracy,
    data2_randomSubspace100_dt_accuracy
))

# p-value = 0.008043 - Rejeita
friedman_data2_randomSubspace_dt_gmean = friedman.test(cbind(
    data2_randomSubspace50_dt_gmean, 
    data2_randomSubspace60_dt_gmean, 
    data2_randomSubspace70_dt_gmean, 
    data2_randomSubspace80_dt_gmean,
    data2_randomSubspace90_dt_gmean,
    data2_randomSubspace100_dt_gmean
))

# p-value = 0.9515 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_dt_auc = friedman.test(cbind(
    data2_randomSubspace50_dt_auc, 
    data2_randomSubspace60_dt_auc, 
    data2_randomSubspace70_dt_auc, 
    data2_randomSubspace80_dt_auc,
    data2_randomSubspace90_dt_auc,
    data2_randomSubspace100_dt_auc
))

# p-value = 0.299 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_dt_fmeasure = friedman.test(cbind(
    data2_randomSubspace50_dt_fmeasure, 
    data2_randomSubspace60_dt_fmeasure, 
    data2_randomSubspace70_dt_fmeasure, 
    data2_randomSubspace80_dt_fmeasure,
    data2_randomSubspace90_dt_fmeasure,
    data2_randomSubspace100_dt_fmeasure
))


# Bagging + Perceptron
# p-value = 0.3495 - Não rejeita, não existe diferença
friedman_data2_bagging_perceptron_accuracy = friedman.test(cbind(
    data2_bagging50_perceptron_accuracy, 
    data2_bagging60_perceptron_accuracy, 
    data2_bagging70_perceptron_accuracy, 
    data2_bagging80_perceptron_accuracy,
    data2_bagging90_perceptron_accuracy,
    data2_bagging100_perceptron_accuracy
))

# p-value = 0.6584 - Não rejeita, não existe diferença
friedman_data2_bagging_perceptron_gmean = friedman.test(cbind(
    data2_bagging50_perceptron_gmean, 
    data2_bagging60_perceptron_gmean, 
    data2_bagging70_perceptron_gmean, 
    data2_bagging80_perceptron_gmean,
    data2_bagging90_perceptron_gmean,
    data2_bagging100_perceptron_gmean
))

# p-value = 0.176 - Não rejeita, não existe diferença
friedman_data2_bagging_perceptron_auc = friedman.test(cbind(
    data2_bagging50_perceptron_auc, 
    data2_bagging60_perceptron_auc, 
    data2_bagging70_perceptron_auc, 
    data2_bagging80_perceptron_auc,
    data2_bagging90_perceptron_auc,
    data2_bagging100_perceptron_auc
))

# p-value = 0.1978 - Não rejeita, não existe diferença
friedman_data2_bagging_perceptron_fmeasure = friedman.test(cbind(
    data2_bagging50_perceptron_fmeasure, 
    data2_bagging60_perceptron_fmeasure, 
    data2_bagging70_perceptron_fmeasure, 
    data2_bagging80_perceptron_fmeasure,
    data2_bagging90_perceptron_fmeasure,
    data2_bagging100_perceptron_fmeasure
))


# Random Subspace + Perceptron
# p-value = 0.5205 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_perceptron_accuracy = friedman.test(cbind(
    data2_randomSubspace50_perceptron_accuracy, 
    data2_randomSubspace60_perceptron_accuracy, 
    data2_randomSubspace70_perceptron_accuracy, 
    data2_randomSubspace80_perceptron_accuracy,
    data2_randomSubspace90_perceptron_accuracy,
    data2_randomSubspace100_perceptron_accuracy
))

# p-value = 0.7971 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_perceptron_gmean = friedman.test(cbind(
    data2_randomSubspace50_perceptron_gmean, 
    data2_randomSubspace60_perceptron_gmean, 
    data2_randomSubspace70_perceptron_gmean, 
    data2_randomSubspace80_perceptron_gmean,
    data2_randomSubspace90_perceptron_gmean,
    data2_randomSubspace100_perceptron_gmean
))

# p-value = 0.6147 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_perceptron_auc = friedman.test(cbind(
    data2_randomSubspace50_perceptron_auc, 
    data2_randomSubspace60_perceptron_auc, 
    data2_randomSubspace70_perceptron_auc, 
    data2_randomSubspace80_perceptron_auc,
    data2_randomSubspace90_perceptron_auc,
    data2_randomSubspace100_perceptron_auc
))

# p-value = 0.6651 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_perceptron_fmeasure = friedman.test(cbind(
    data2_randomSubspace50_perceptron_fmeasure, 
    data2_randomSubspace60_perceptron_fmeasure, 
    data2_randomSubspace70_perceptron_fmeasure, 
    data2_randomSubspace80_perceptron_fmeasure,
    data2_randomSubspace90_perceptron_fmeasure,
    data2_randomSubspace100_perceptron_fmeasure
))
# Resultado: Não existiu diferença significativa em nenhuma métrica do perceptron ou arvore de decisão, dado o corte no conjunto de treinamento da base 1



# RANDOM SUBSPACE VS BAGGING

# H0: O desempenho dos métodos no conjunto de teste se mantém igual ao serem treinados com bagging ou random subspace
# H1: O desempenho dos métodos são diferentes

# Decision Tree
# p-value = 0.8489 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_vs_bagging_dt_accuracy = friedman.test(cbind(
    data1_randomSubspace50_dt_accuracy,
    data1_bagging50_dt_accuracy,
    data1_randomSubspace100_dt_accuracy,
    data1_bagging100_dt_accuracy
))

# p-value = 0.1004 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_vs_bagging_dt_gmean = friedman.test(cbind(
    data1_randomSubspace50_dt_gmean,
    data1_bagging50_dt_gmean,
    data1_randomSubspace100_dt_gmean,
    data1_bagging100_dt_gmean
))
# p-value = 0.1359 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_vs_bagging_dt_auc = friedman.test(cbind(
    data1_randomSubspace50_dt_auc,
    data1_bagging50_dt_auc,
    data1_randomSubspace100_dt_auc,
    data1_bagging100_dt_auc
))
# p-value = 0.7679 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_vs_bagging_dt_fmeasure = friedman.test(cbind(
    data1_randomSubspace50_dt_fmeasure,
    data1_bagging50_dt_fmeasure,
    data1_randomSubspace100_dt_fmeasure,
    data1_bagging100_dt_fmeasure
))

# p-value = 0.3344 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_vs_bagging_dt_accuracy = friedman.test(cbind(
    data2_randomSubspace50_dt_accuracy,
    data2_bagging50_dt_accuracy,
    data2_randomSubspace100_dt_accuracy,
    data2_bagging100_dt_accuracy
))

# p-value = 0.1807 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_vs_bagging_dt_gmean = friedman.test(cbind(
    data2_randomSubspace50_dt_gmean,
    data2_bagging50_dt_gmean,
    data2_randomSubspace100_dt_gmean,
    data2_bagging100_dt_gmean
))

# p-value = 0.1877 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_vs_bagging_dt_auc = friedman.test(cbind(
    data2_randomSubspace50_dt_auc,
    data2_bagging50_dt_auc,
    data2_randomSubspace100_dt_auc,
    data2_bagging100_dt_auc
))

# p-value = 0.3154 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_vs_bagging_dt_fmeasure = friedman.test(cbind(
    data2_randomSubspace50_dt_fmeasure,
    data2_bagging50_dt_fmeasure,
    data2_randomSubspace100_dt_fmeasure,
    data2_bagging100_dt_fmeasure
))


# Perceptron

# p-value = 0.09707 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_vs_bagging_perceptron_accuracy = friedman.test(cbind(
    data1_randomSubspace50_perceptron_accuracy,
    data1_bagging50_perceptron_accuracy,
    data1_randomSubspace100_perceptron_accuracy,
    data1_bagging100_perceptron_accuracy
))

# p-value = 1 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_vs_bagging_perceptron_gmean = friedman.test(cbind(
    data1_randomSubspace50_perceptron_gmean,
    data1_bagging50_perceptron_gmean
))

# p-value = 0.7819 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_vs_bagging_perceptron_auc = friedman.test(cbind(
    data1_randomSubspace50_perceptron_auc,
    data1_bagging50_perceptron_auc,
    data1_randomSubspace100_perceptron_auc,
    data1_bagging100_perceptron_auc
))

# p-value = 0.09707 - Não rejeita, não existe diferença
friedman_data1_randomSubspace_vs_bagging_perceptron_fmeasure = friedman.test(cbind(
    data1_randomSubspace50_perceptron_fmeasure,
    data1_bagging50_perceptron_fmeasure,
    data1_randomSubspace100_perceptron_fmeasure,
    data1_bagging100_perceptron_fmeasure
))

# p-value = 0.5923 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_vs_bagging_perceptron_accuracy = friedman.test(cbind(
    data2_randomSubspace50_perceptron_accuracy,
    data2_bagging50_perceptron_accuracy,
    data2_randomSubspace100_perceptron_accuracy,
    data2_bagging100_perceptron_accuracy
))

# p-value = 0.3027 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_vs_bagging_perceptron_gmean = friedman.test(cbind(
    data2_randomSubspace50_perceptron_gmean,
    data2_bagging50_perceptron_gmean,
    data2_randomSubspace100_perceptron_gmean,
    data2_bagging100_perceptron_gmean
))

# p-value = 0.0002207 - Rejeita
friedman_data2_randomSubspace_vs_bagging_perceptron_auc = friedman.test(cbind(
    data2_randomSubspace50_perceptron_auc,
    data2_bagging50_perceptron_auc,
    data2_randomSubspace100_perceptron_auc,
    data2_bagging100_perceptron_auc
))

# p-value = 0.16 - Não rejeita, não existe diferença
friedman_data2_randomSubspace_vs_bagging_perceptron_fmeasure = friedman.test(cbind(
    data2_randomSubspace50_perceptron_fmeasure,
    data2_bagging50_perceptron_fmeasure,
    data2_randomSubspace100_perceptron_fmeasure,
    data2_bagging100_perceptron_fmeasure
))

# Resultado: De todas as métricas analisadas, o bagging teve uma performance superior ao random subspace na AUC apenas no treinamento do perceptron na base 2.
# Nos demais casos não existiu diferença estatística, entre o uso do bagging e random subspace.




# PERCEPTRON VS DECISION TREE

# H0: O desempenho dos métodos são iguais
# H1: O desempenho dos métodos são diferentes

# Perceptron vs Decision Tree (data1)

# p-value = 0.1323
friedman_data1_perceptron_vs_dt_accuracy = friedman.test(cbind(
    data1_bagging50_dt_accuracy,
    data1_bagging50_perceptron_accuracy,
    data1_randomSubspace50_dt_accuracy,
    data1_randomSubspace50_perceptron_accuracy
))

# p-value = 0.1783
friedman_data1_perceptron_vs_dt_gmean = friedman.test(cbind(
    data1_bagging50_dt_gmean,
    data1_bagging50_perceptron_gmean,
    data1_randomSubspace50_dt_gmean,
    data1_randomSubspace50_perceptron_gmean
))

# p-value = 0.1323
friedman_data1_perceptron_vs_dt_fmeasure = friedman.test(cbind(
    data1_bagging50_dt_fmeasure,
    data1_bagging50_perceptron_fmeasure,
    data1_randomSubspace50_dt_fmeasure,
    data1_randomSubspace50_perceptron_fmeasure
))

# p-value = 4.772e-05
friedman_data1_perceptron_vs_dt_auc = friedman.test(cbind(
    data1_bagging50_dt_auc,
    data1_bagging50_perceptron_auc,
    data1_randomSubspace50_dt_auc,
    data1_randomSubspace50_perceptron_auc
))


# p-value = 0.006897 - Decision Tree is Greater
wilcox_data1_bagging_perceptron_vs_dt_auc = wilcox.test(paired = TRUE, alternative = "greater", conf.level = 0.95,
    x = data1_bagging50_dt_auc, 
    y = data1_bagging50_perceptron_auc
)

# p-value = 0.007133 - Decision Tree is Greater
wilcox_data1_randomSubspace_perceptron_vs_dt_auc = wilcox.test(paired = TRUE, alternative = "greater", conf.level = 0.95,
    x = data1_randomSubspace50_dt_auc, 
    y = data1_randomSubspace50_perceptron_auc
)

# p-value = 0.06811 - Random Subspace and Bagging Equals
wilcox_data1_randomSubspace_vs_bagging_dt_auc = wilcox.test(paired = TRUE, alternative = "greater", conf.level = 0.95,
    x = data1_randomSubspace50_dt_auc,
    y = data1_bagging50_dt_auc
)


# Perceptron vs Decision Tree (data2)

# p-value = 0.08803
friedman_data2_perceptron_vs_dt_accuracy = friedman.test(cbind(
    data2_bagging50_dt_accuracy,
    data2_bagging50_perceptron_accuracy,
    data2_randomSubspace50_dt_accuracy,
    data2_randomSubspace50_perceptron_accuracy
))

# p-value = 0.4741
friedman_data2_perceptron_vs_dt_gmean = friedman.test(cbind(
    data2_bagging50_dt_gmean,
    data2_bagging50_perceptron_gmean,
    data2_randomSubspace50_dt_gmean,
    data2_randomSubspace50_perceptron_gmean
))

# p-value = 0.05938
friedman_data2_perceptron_vs_dt_fmeasure = friedman.test(cbind(
    data2_bagging50_dt_fmeasure,
    data2_bagging50_perceptron_fmeasure,
    data2_randomSubspace50_dt_fmeasure,
    data2_randomSubspace50_perceptron_fmeasure
))

# p-value = 0.008268
friedman_data2_perceptron_vs_dt_auc = friedman.test(cbind(
    data2_bagging50_dt_auc,
    data2_bagging50_perceptron_auc,
    data2_randomSubspace50_dt_auc,
    data2_randomSubspace50_perceptron_auc
))
# Indicações que os métodos diferem


# p-value = 0.9603 - Perceptron is Greater than Decision Tree with Bagging
wilcox_data2_bagging_perceptron_vs_dt_auc = wilcox.test(paired = TRUE, alternative = "greater", conf.level = 0.95,
    x = data2_bagging50_dt_auc, 
    y = data2_bagging50_perceptron_auc
)

# p-value = 0.006386 - Decision Tree is Greater than Perceptron with Random Subspace
wilcox_data2_randomSubspace_perceptron_vs_dt_auc = wilcox.test(paired = TRUE, alternative = "greater", conf.level = 0.95,
    x = data2_randomSubspace50_dt_auc, 
    y = data2_randomSubspace50_perceptron_auc
)

# p-value = 0.02858 - Because random Subspace was better on Decision Tree
wilcox_data2_randomSubspace_vs_bagging_dt_auc = wilcox.test(paired = TRUE, alternative = "greater", conf.level = 0.95,
    x = data2_randomSubspace50_dt_auc,
    y = data2_bagging50_dt_auc
)

# p-value = 0.9971 - And Bagging was better on Perceptron
wilcox_data2_randomSubspace_vs_bagging_perceptron_auc = wilcox.test(paired = TRUE, alternative = "greater", conf.level = 0.95,
    x = data2_randomSubspace50_perceptron_auc,
    y = data2_bagging50_perceptron_auc
)

# p-value = 0.8368 - But, Decision Tree with Random Subspace had the same performance as Perceptron with Bagging
wilcox_data2_randomSubspace_dt_vs_bagging_perceptron_auc = wilcox.test(paired = TRUE, alternative = "greater", conf.level = 0.95,
    x = data2_randomSubspace50_dt_auc,
    y = data2_bagging50_perceptron_auc
)

# p-value = 0.2 - But, Decision Tree with Random Subspace had the same performance as Perceptron with Bagging
wilcox_data2_bagging_perceptron_vs_randomSubspace_dt_auc = wilcox.test(paired = TRUE, alternative = "less", conf.level = 0.95,
    x = data2_randomSubspace50_dt_auc,
    y = data2_bagging50_perceptron_auc
)