library('rpart')
library('ROSE')
library('caret')
library('dbscan')

path = 'C:/Projects/lista1_multiple_classifier_system/projeto'
setwd(path)

data1 = read.table('data.csv', sep=',', header=TRUE)

lengthMajority = length(which(data1$target == 'false'))
lengthMinority = length(which(data1$target == 'true'))
numOfBins = floor(lengthMajority/lengthMinority)

binsIdx = createFolds(data1[which(data1$target == 'false'), ]$target, k = numOfBins)