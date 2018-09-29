# Fold > Validation Set > Pruning > Metrics
data1_metrics = readRDS("data1_perceptronMetrics.rds")
data2_metrics = readRDS("data2_perceptronMetrics.rds")

# Fold > Validation Set > Pruning > Diversity Measures
data1_diversity = readRDS("data1_diversity.rds")
data2_diversity = readRDS("data2_diversity.rds")

# Fold: 1-10
# Validation Set: 1-3 (Complete, Hard, Easy)
# Pruning: No Pruning, Best First, Reduce Error
# Metrics: Accuracy, auc, fmeasure, gmean
# Diversity: q, kappa

results.diversity = function(metrics, set, pruning, index){
    metric = c()
    for(i in 1:length(metrics)){
        value = get(pruning, metrics[[i]][[set]])[[index]]
        if(is.na(value)){
            value = 0
        }
        metric = c(metric, value)
    }    
    return (metric)
}

results.metric = function(metrics, name, set, pruning){
    metric = c()
    for(i in 1:length(metrics)){
        value = get(name, get(pruning, metrics[[i]][[set]]))
        if(is.na(value)){
            value = 0
        }
        metric = c(metric, value)
    }    
    return (metric)
}

# Mean of metrics for table
data1_accuracy_validation1_noPruning_mean = mean(results.metric(data1_metrics, 'accuracy', 1, 'full'), na.rm=TRUE)
data1_accuracy_validation1_bestFirst_mean = mean(results.metric(data1_metrics, 'accuracy', 1, 'bestFirst'), na.rm=TRUE)
data1_accuracy_validation1_reduceError_mean = mean(results.metric(data1_metrics, 'accuracy', 1, 'reduceError'), na.rm=TRUE)

data1_accuracy_validation2_noPruning_mean = mean(results.metric(data1_metrics, 'accuracy', 2, 'full'), na.rm=TRUE)
data1_accuracy_validation2_bestFirst_mean = mean(results.metric(data1_metrics, 'accuracy', 2, 'bestFirst'), na.rm=TRUE)
data1_accuracy_validation2_reduceError_mean = mean(results.metric(data1_metrics, 'accuracy', 2, 'reduceError'), na.rm=TRUE)

data1_accuracy_validation3_noPruning_mean = mean(results.metric(data1_metrics, 'accuracy', 3, 'full'), na.rm=TRUE)
data1_accuracy_validation3_bestFirst_mean = mean(results.metric(data1_metrics, 'accuracy', 3, 'bestFirst'), na.rm=TRUE)
data1_accuracy_validation3_reduceError_mean = mean(results.metric(data1_metrics, 'accuracy', 3, 'reduceError'), na.rm=TRUE)


data1_auc_validation1_noPruning_mean = mean(results.metric(data1_metrics, 'auc', 1, 'full'), na.rm=TRUE)
data1_auc_validation1_bestFirst_mean = mean(results.metric(data1_metrics, 'auc', 1, 'bestFirst'), na.rm=TRUE)
data1_auc_validation1_reduceError_mean = mean(results.metric(data1_metrics, 'auc', 1, 'reduceError'), na.rm=TRUE)

data1_auc_validation2_noPruning_mean = mean(results.metric(data1_metrics, 'auc', 2, 'full'), na.rm=TRUE)
data1_auc_validation2_bestFirst_mean = mean(results.metric(data1_metrics, 'auc', 2, 'bestFirst'), na.rm=TRUE)
data1_auc_validation2_reduceError_mean = mean(results.metric(data1_metrics, 'auc', 2, 'reduceError'), na.rm=TRUE)

data1_auc_validation3_noPruning_mean = mean(results.metric(data1_metrics, 'auc', 3, 'full'), na.rm=TRUE)
data1_auc_validation3_bestFirst_mean = mean(results.metric(data1_metrics, 'auc', 3, 'bestFirst'), na.rm=TRUE)
data1_auc_validation3_reduceError_mean = mean(results.metric(data1_metrics, 'auc', 3, 'reduceError'), na.rm=TRUE)


data1_gmean_validation1_noPruning_mean = mean(results.metric(data1_metrics, 'gmean', 1, 'full'), na.rm=TRUE)
data1_gmean_validation1_bestFirst_mean = mean(results.metric(data1_metrics, 'gmean', 1, 'bestFirst'), na.rm=TRUE)
data1_gmean_validation1_reduceError_mean = mean(results.metric(data1_metrics, 'gmean', 1, 'reduceError'), na.rm=TRUE)

data1_gmean_validation2_noPruning_mean = mean(results.metric(data1_metrics, 'gmean', 2, 'full'), na.rm=TRUE)
data1_gmean_validation2_bestFirst_mean = mean(results.metric(data1_metrics, 'gmean', 2, 'bestFirst'), na.rm=TRUE)
data1_gmean_validation2_reduceError_mean = mean(results.metric(data1_metrics, 'gmean', 2, 'reduceError'), na.rm=TRUE)

data1_gmean_validation3_noPruning_mean = mean(results.metric(data1_metrics, 'gmean', 3, 'full'), na.rm=TRUE)
data1_gmean_validation3_bestFirst_mean = mean(results.metric(data1_metrics, 'gmean', 3, 'bestFirst'), na.rm=TRUE)
data1_gmean_validation3_reduceError_mean = mean(results.metric(data1_metrics, 'gmean', 3, 'reduceError'), na.rm=TRUE)


data1_fmeasure_validation1_noPruning_mean = mean(results.metric(data1_metrics, 'fmeasure', 1, 'full'), na.rm=TRUE)
data1_fmeasure_validation1_bestFirst_mean = mean(results.metric(data1_metrics, 'fmeasure', 1, 'bestFirst'), na.rm=TRUE)
data1_fmeasure_validation1_reduceError_mean = mean(results.metric(data1_metrics, 'fmeasure', 1, 'reduceError'), na.rm=TRUE)

data1_fmeasure_validation2_noPruning_mean = mean(results.metric(data1_metrics, 'fmeasure', 2, 'full'), na.rm=TRUE)
data1_fmeasure_validation2_bestFirst_mean = mean(results.metric(data1_metrics, 'fmeasure', 2, 'bestFirst'), na.rm=TRUE)
data1_fmeasure_validation2_reduceError_mean = mean(results.metric(data1_metrics, 'fmeasure', 2, 'reduceError'), na.rm=TRUE)

data1_fmeasure_validation3_noPruning_mean = mean(results.metric(data1_metrics, 'fmeasure', 3, 'full'), na.rm=TRUE)
data1_fmeasure_validation3_bestFirst_mean = mean(results.metric(data1_metrics, 'fmeasure', 3, 'bestFirst'), na.rm=TRUE)
data1_fmeasure_validation3_reduceError_mean = mean(results.metric(data1_metrics, 'fmeasure', 3, 'reduceError'), na.rm=TRUE)


# Dataset 2
data2_accuracy_validation1_noPruning_mean = mean(results.metric(data2_metrics, 'accuracy', 1, 'full'), na.rm=TRUE)
data2_accuracy_validation1_bestFirst_mean = mean(results.metric(data2_metrics, 'accuracy', 1, 'bestFirst'), na.rm=TRUE)
data2_accuracy_validation1_reduceError_mean = mean(results.metric(data2_metrics, 'accuracy', 1, 'reduceError'), na.rm=TRUE)

data2_accuracy_validation2_noPruning_mean = mean(results.metric(data2_metrics, 'accuracy', 2, 'full'), na.rm=TRUE)
data2_accuracy_validation2_bestFirst_mean = mean(results.metric(data2_metrics, 'accuracy', 2, 'bestFirst'), na.rm=TRUE)
data2_accuracy_validation2_reduceError_mean = mean(results.metric(data2_metrics, 'accuracy', 2, 'reduceError'), na.rm=TRUE)

data2_accuracy_validation3_noPruning_mean = mean(results.metric(data2_metrics, 'accuracy', 3, 'full'), na.rm=TRUE)
data2_accuracy_validation3_bestFirst_mean = mean(results.metric(data2_metrics, 'accuracy', 3, 'bestFirst'), na.rm=TRUE)
data2_accuracy_validation3_reduceError_mean = mean(results.metric(data2_metrics, 'accuracy', 3, 'reduceError'), na.rm=TRUE)


data2_auc_validation1_noPruning_mean = mean(results.metric(data2_metrics, 'auc', 1, 'full'), na.rm=TRUE)
data2_auc_validation1_bestFirst_mean = mean(results.metric(data2_metrics, 'auc', 1, 'bestFirst'), na.rm=TRUE)
data2_auc_validation1_reduceError_mean = mean(results.metric(data2_metrics, 'auc', 1, 'reduceError'), na.rm=TRUE)

data2_auc_validation2_noPruning_mean = mean(results.metric(data2_metrics, 'auc', 2, 'full'), na.rm=TRUE)
data2_auc_validation2_bestFirst_mean = mean(results.metric(data2_metrics, 'auc', 2, 'bestFirst'), na.rm=TRUE)
data2_auc_validation2_reduceError_mean = mean(results.metric(data2_metrics, 'auc', 2, 'reduceError'), na.rm=TRUE)

data2_auc_validation3_noPruning_mean = mean(results.metric(data2_metrics, 'auc', 3, 'full'), na.rm=TRUE)
data2_auc_validation3_bestFirst_mean = mean(results.metric(data2_metrics, 'auc', 3, 'bestFirst'), na.rm=TRUE)
data2_auc_validation3_reduceError_mean = mean(results.metric(data2_metrics, 'auc', 3, 'reduceError'), na.rm=TRUE)


data2_gmean_validation1_noPruning_mean = mean(results.metric(data2_metrics, 'gmean', 1, 'full'), na.rm=TRUE)
data2_gmean_validation1_bestFirst_mean = mean(results.metric(data2_metrics, 'gmean', 1, 'bestFirst'), na.rm=TRUE)
data2_gmean_validation1_reduceError_mean = mean(results.metric(data2_metrics, 'gmean', 1, 'reduceError'), na.rm=TRUE)

data2_gmean_validation2_noPruning_mean = mean(results.metric(data2_metrics, 'gmean', 2, 'full'), na.rm=TRUE)
data2_gmean_validation2_bestFirst_mean = mean(results.metric(data2_metrics, 'gmean', 2, 'bestFirst'), na.rm=TRUE)
data2_gmean_validation2_reduceError_mean = mean(results.metric(data2_metrics, 'gmean', 2, 'reduceError'), na.rm=TRUE)

data2_gmean_validation3_noPruning_mean = mean(results.metric(data2_metrics, 'gmean', 3, 'full'), na.rm=TRUE)
data2_gmean_validation3_bestFirst_mean = mean(results.metric(data2_metrics, 'gmean', 3, 'bestFirst'), na.rm=TRUE)
data2_gmean_validation3_reduceError_mean = mean(results.metric(data2_metrics, 'gmean', 3, 'reduceError'), na.rm=TRUE)


data2_fmeasure_validation1_noPruning_mean = mean(results.metric(data2_metrics, 'fmeasure', 1, 'full'), na.rm=TRUE)
data2_fmeasure_validation1_bestFirst_mean = mean(results.metric(data2_metrics, 'fmeasure', 1, 'bestFirst'), na.rm=TRUE)
data2_fmeasure_validation1_reduceError_mean = mean(results.metric(data2_metrics, 'fmeasure', 1, 'reduceError'), na.rm=TRUE)

data2_fmeasure_validation2_noPruning_mean = mean(results.metric(data2_metrics, 'fmeasure', 2, 'full'), na.rm=TRUE)
data2_fmeasure_validation2_bestFirst_mean = mean(results.metric(data2_metrics, 'fmeasure', 2, 'bestFirst'), na.rm=TRUE)
data2_fmeasure_validation2_reduceError_mean = mean(results.metric(data2_metrics, 'fmeasure', 2, 'reduceError'), na.rm=TRUE)

data2_fmeasure_validation3_noPruning_mean = mean(results.metric(data2_metrics, 'fmeasure', 3, 'full'), na.rm=TRUE)
data2_fmeasure_validation3_bestFirst_mean = mean(results.metric(data2_metrics, 'fmeasure', 3, 'bestFirst'), na.rm=TRUE)
data2_fmeasure_validation3_reduceError_mean = mean(results.metric(data2_metrics, 'fmeasure', 3, 'reduceError'), na.rm=TRUE)

# Diversity Data 1
data1_q_validation1_noPruning = mean(results.diversity(data1_diversity, 1, 'full', 1))
data1_q_validation1_bestFirst = mean(results.diversity(data1_diversity, 1, 'bestFirst', 1))
data1_q_validation1_reduceError = mean(results.diversity(data1_diversity, 1, 'reduceError', 1))

data1_q_validation2_noPruning = mean(results.diversity(data1_diversity, 2, 'full', 1))
data1_q_validation2_bestFirst = mean(results.diversity(data1_diversity, 2, 'bestFirst', 1))
data1_q_validation2_reduceError = mean(results.diversity(data1_diversity, 2, 'reduceError', 1))

data1_q_validation3_noPruning = mean(results.diversity(data1_diversity, 3, 'full', 1))
data1_q_validation3_bestFirst = mean(results.diversity(data1_diversity, 3, 'bestFirst', 1))
data1_q_validation3_reduceError = mean(results.diversity(data1_diversity, 3, 'reduceError', 1))


data1_kappa_validation1_noPruning = mean(results.diversity(data1_diversity, 1, 'full', 2))
data1_kappa_validation1_bestFirst = mean(results.diversity(data1_diversity, 1, 'bestFirst', 2))
data1_kappa_validation1_reduceError = mean(results.diversity(data1_diversity, 1, 'reduceError', 2))

data1_kappa_validation2_noPruning = mean(results.diversity(data1_diversity, 2, 'full', 2))
data1_kappa_validation2_bestFirst = mean(results.diversity(data1_diversity, 2, 'bestFirst', 2))
data1_kappa_validation2_reduceError = mean(results.diversity(data1_diversity, 2, 'reduceError', 2))

data1_kappa_validation3_noPruning = mean(results.diversity(data1_diversity, 3, 'full', 2))
data1_kappa_validation3_bestFirst = mean(results.diversity(data1_diversity, 3, 'bestFirst', 2))
data1_kappa_validation3_reduceError = mean(results.diversity(data1_diversity, 3, 'reduceError', 2))

# Data 2
data2_q_validation1_noPruning = mean(results.diversity(data2_diversity, 1, 'full', 1))
data2_q_validation1_bestFirst = mean(results.diversity(data2_diversity, 1, 'bestFirst', 1))
data2_q_validation1_reduceError = mean(results.diversity(data2_diversity, 1, 'reduceError', 1))

data2_q_validation2_noPruning = mean(results.diversity(data2_diversity, 2, 'full', 1))
data2_q_validation2_bestFirst = mean(results.diversity(data2_diversity, 2, 'bestFirst', 1))
data2_q_validation2_reduceError = mean(results.diversity(data2_diversity, 2, 'reduceError', 1))

data2_q_validation3_noPruning = mean(results.diversity(data2_diversity, 3, 'full', 1))
data2_q_validation3_bestFirst = mean(results.diversity(data2_diversity, 3, 'bestFirst', 1))
data2_q_validation3_reduceError = mean(results.diversity(data2_diversity, 3, 'reduceError', 1))


data2_kappa_validation1_noPruning = mean(results.diversity(data2_diversity, 1, 'full', 2))
data2_kappa_validation1_bestFirst = mean(results.diversity(data2_diversity, 1, 'bestFirst', 2))
data2_kappa_validation1_reduceError = mean(results.diversity(data2_diversity, 1, 'reduceError', 2))

data2_kappa_validation2_noPruning = mean(results.diversity(data2_diversity, 2, 'full', 2))
data2_kappa_validation2_bestFirst = mean(results.diversity(data2_diversity, 2, 'bestFirst', 2))
data2_kappa_validation2_reduceError = mean(results.diversity(data2_diversity, 2, 'reduceError', 2))

data2_kappa_validation3_noPruning = mean(results.diversity(data2_diversity, 3, 'full', 2))
data2_kappa_validation3_bestFirst = mean(results.diversity(data2_diversity, 3, 'bestFirst', 2))
data2_kappa_validation3_reduceError = mean(results.diversity(data2_diversity, 3, 'reduceError', 2))