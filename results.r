data1_dtMetrics = readRDS("data1_dtMetrics.rds")
data1_perceptronMetrics = readRDS("data1_perceptronMetrics.rds")
data2_dtMetrics = readRDS("data2_dtMetrics.rds")
data2_perceptronMetrics = readRDS("data2_perceptronMetrics.rds")

results.metric = function(metrics, name, index){
    metric = c()
    for(i in 1:length(metrics)){
        value = get(name, metrics[[i]])[index]
        if(is.na(value)){
            value = 0
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
data1_bagging50_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 2)
data1_bagging60_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 4)
data1_bagging70_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 6)
data1_bagging80_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 8)
data1_bagging90_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 10)
data1_bagging100_dt_accuracy = results.metric(data1_dtMetrics, 'accuracy', 12)

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
data1_bagging50_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 2)
data1_bagging60_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 4)
data1_bagging70_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 6)
data1_bagging80_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 8)
data1_bagging90_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 10)
data1_bagging100_perceptron_accuracy = results.metric(data1_perceptronMetrics, 'accuracy', 12)

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
data2_bagging50_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 2)
data2_bagging60_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 4)
data2_bagging70_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 6)
data2_bagging80_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 8)
data2_bagging90_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 10)
data2_bagging100_dt_accuracy = results.metric(data2_dtMetrics, 'accuracy', 12)

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
data2_bagging50_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 2)
data2_bagging60_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 4)
data2_bagging70_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 6)
data2_bagging80_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 8)
data2_bagging90_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 10)
data2_bagging100_perceptron_accuracy = results.metric(data2_perceptronMetrics, 'accuracy', 12)

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

