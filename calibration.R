############ FUSION #################

source("/mnt/SSD/polymer/polymeRID/code/setup_website.R")
source("/mnt/SSD/polymer/polymeRID/code/functions.R")
#system("source misc/cuda10.1-env")
# reading data based on class control file
classes = readLines(paste0(ref, "classes.txt"))
data = lapply(classes,function(class){
  print(class)
  files = list.files(ref, full.names=TRUE)
  file = files[grep(paste("_",class, ".csv", sep=""), files)]
  data = read.csv(file, header = TRUE)
  return(data)
})
data = do.call("rbind",data)

folds = 1:10
repeats = 1:5
folds = paste("fold",folds,sep="")
repeats = paste("rep",repeats,sep="")

set.seed(42)
seeds = sample(1:1000, length(folds) * length(repeats))

sg.data = preprocess(data[,1:ncol(data)-1], type = "sg")
d2.data = preprocess(data[,1:ncol(data)-1], type = "raw.d2")
normd2.data = preprocess(data[,1:ncol(data)-1], type = "norm.d2")
sg.data$class = data$class
d2.data$class = data$class
normd2.data$class = data$class

classResults = list()
accuracyResults = list()
counter = 1
for (rep in repeats){
  for (fold in folds){

    # create data split
    set.seed(seeds[counter])
    index = caret::createDataPartition(data$class, times = 1, p = 0.5)

    trainingRaw = data[index$Resample1,]
    testingRaw = data[-index$Resample1,]
    trainingSG = sg.data[index$Resample1,]
    testingSG = sg.data[-index$Resample1,]
    trainingD2 = d2.data[index$Resample1,]
    testingD2 = d2.data[-index$Resample1,]
    trainingNormD2 = normd2.data[index$Resample1,]
    testingNormD2 = normd2.data[-index$Resample1,]


    # preparing RF Models
    pcaRaw = prcomp(trainingRaw[,1:ncol(trainingRaw)-1,])
    varInfo = factoextra::get_eigenvalue(pcaRaw)
    thresInd = which(varInfo$cumulative.variance.percent >= 99)[1]
    pcaRaw_training = pcaRaw$x[ ,1:thresInd]
    pcaRaw_testing = predict(pcaRaw, testingRaw)[ ,1:thresInd]

    rfModRaw = randomForest::randomForest(pcaRaw_training,trainingRaw$class, ntree = 500)

    pcaSG = prcomp(trainingSG[,1:ncol(trainingSG)-1,])
    varInfo = factoextra::get_eigenvalue(pcaSG)
    thresInd = which(varInfo$cumulative.variance.percent >= 99)[1]
    pcaSG_training = pcaSG$x[ ,1:thresInd]
    pcaSG_testing = predict(pcaSG, testingSG)[ ,1:thresInd]

    rfModSG = randomForest::randomForest(pcaSG_training,trainingSG$class, ntree = 500)

    # preparing CNN Models
    K <- keras::backend()
    x_train = as.matrix(trainingD2[,1:ncol(trainingD2)-1])
    x = K$expand_dims(x_train, axis = 2L)
    x_train = K$eval(x)
    y_train = keras::to_categorical(as.numeric(trainingD2$class)-1, length(unique(trainingD2$class)))

    x_testD2 = as.matrix(testingSG[,1:ncol(testingD2)-1])
    x = K$expand_dims(x_testD2, axis = 2L)
    x_testD2 = K$eval(x)
    y_testD2 = keras::to_categorical(as.numeric(testingD2$class)-1, length(unique(testingD2$class)))

    cnnD2 = prepNNET(kernel = 90, variables = ncol(d2.data)-1, nOutcome = length(unique(d2.data$class)))
    history = fit(cnnD2, x_train, y_train, batch_size = 10, epochs = 300)


    x_train = as.matrix(trainingNormD2[,1:ncol(trainingNormD2)-1])
    x = K$expand_dims(x_train, axis = 2L)
    x_train = K$eval(x)
    y_train = keras::to_categorical(as.numeric(trainingNormD2$class)-1, length(unique(trainingNormD2$class)))

    x_testND2 = as.matrix(testingNormD2[,1:ncol(testingNormD2)-1])
    x = K$expand_dims(x_testND2, axis = 2L)
    x_testND2 = K$eval(x)
    y_testND2 = keras::to_categorical(as.numeric(testingNormD2$class)-1, length(unique(testingNormD2$class)))

    cnnND2 = prepNNET(kernel = 90, variables = ncol(normd2.data)-1, nOutcome = length(unique(normd2.data$class)))
    history = fit(cnnND2, x_train, y_train, batch_size = 10, epochs = 300)



    # predicting
    classes = unique(trainingRaw$class)
    classRFRaw = as.character(predict(rfModRaw, pcaRaw_testing))
    propRFRaw =  predict(rfModRaw, pcaRaw_testing, type = "prob")
    classRFSG = as.character(predict(rfModSG, pcaSG_testing))
    propRFSG = predict(rfModSG, pcaSG_testing, type = "prob")
    classCNND2 = as.character(classes[keras::predict_classes(cnnD2, x_testD2)+1])
    propCNND2 = keras::predict_proba(cnnD2, x_testD2)
    classCNNND2 = as.character(classes[keras::predict_classes(cnnND2, x_testND2)+1])
    propCNNND2 = keras::predict_proba(cnnND2, x_testND2)

    # probability
    probs = (propRFRaw + propRFSG + propCNND2 + propCNNND2) / 4
    pred = lapply(1:nrow(probs), function(x){
      which.max(probs[x,])
    })

    predVals = lapply(1:nrow(probs), function(x){
      probs[x,unlist(pred)[x]]
    })


    predVals = unlist(predVals)
    pred= names(unlist(pred))
    obsv = as.character(testingRaw$class)

    pred[which(pred %in% c("FIBRE","FUR","WOOD"))] = "OTHER"
    obsv[which(obsv %in% c("FIBRE","FUR","WOOD"))] = "OTHER"

    obsv = as.factor(obsv)
    pred = as.factor(pred)
    cfMat = caret::confusionMatrix(pred,obsv)
    classValues = cfMat$byClass
    accVals = cfMat$overall

    classResults[[rep]][[fold]] = classValues
    accuracyResults[[rep]][[fold]] = accVals

    counter = counter + 1
  }
}

saveRDS(classResults, file = paste0(output, "fusion/classResults.rds"))
saveRDS(accuracyResults, file = paste0(output, "fusion/accuracyResults.rds"))


accuracies = as.data.frame(matrix(unlist(accuracyResults),ncol=7,nrow=50, byrow = T))
names(accuracies) = names(accuracyResults[[1]][[1]])
accuracies = colMeans(accuracies, na.rm = T)
accuracies = round(accuracies, 4)
saveRDS(accuracies, file = paste0(output, "fusion/meanAccuracyResults.rds"))


classInfo = array(unlist(classResults), dim = c(dim(classResults$rep1$fold1)[1], dim(classResults$rep1$fold1)[2], 50 ))
meanClassInfo = apply(classInfo, c(1,2), mean, na.rm=TRUE)
dimnames(meanClassInfo)= dimnames(classResults$rep1$fold1)
saveRDS(meanClassInfo, file = paste0(output, "fusion/meanClassResults.rds"))



# training of the final models
# RF SG
pca = prcomp(sg.data[,1:ncol(sg.data)-1])
varInfo = factoextra::get_eigenvalue(pca)
thresInd = which(varInfo$cumulative.variance.percent >= 99)[1]
x_train = pca$x[ ,1:thresInd]
y_train = sg.data$class

rfModSG = randomForest::randomForest(x_train, y_train, ntree=500)
saveRDS(rfModSG, file = paste0(mod, "BASE/rfModSG.rds"))
saveRDS(pca, file = paste0(mod, "BASE/rfModSGPCA.rds"))
#RF Raw
pca = prcomp(data[,1:ncol(data)-1])
varInfo = factoextra::get_eigenvalue(pca)
thresInd = which(varInfo$cumulative.variance.percent >= 99)[1]
x_train = pca$x[ ,1:thresInd]
y_train = data$class

rfModRaw = randomForest::randomForest(x_train, y_train, ntree=500)
saveRDS(rfModSG, file = paste0(mod, "BASE/rfModRaw.rds"))
saveRDS(pca, file = paste0(mod, "BASE/rfModRawPCA.rds"))

#CNN Raw
K <- keras::backend()
x_train = as.matrix(d2.data[,1:ncol(d2.data)-1])
x = K$expand_dims(x_train, axis = 2L)
x_train = K$eval(x)
y_train = keras::to_categorical(as.numeric(d2.data$class)-1, length(unique(d2.data$class)))

cnnD2 = prepNNET(kernel = 90, variables = ncol(d2.data)-1, nOutcome = length(unique(d2.data$class)))
history = fit(cnnD2, x_train, y_train, batch_size = 10, epochs = 300)
keras::save_model_hdf5(cnnD2, filepath = paste0(mod, "BASE/cnnD2.hdf"))

#CNN D1
x_train = as.matrix(normd2.data[,1:ncol(normd2.data)-1])
x = K$expand_dims(x_train, axis = 2L)
x_train = K$eval(x)
y_train = keras::to_categorical(as.numeric(normd2.data$class)-1, length(unique(normd2.data$class)))

cnnND2 = prepNNET(kernel = 90, variables = ncol(normd2.data)-1, nOutcome = length(unique(normd2.data$class)))
history = fit(cnnND2, x_train, y_train, batch_size = 10, epochs = 300)
keras::save_model_hdf5(cnnND2, filepath = paste0(mod, "BASE/cnnND2.hdf"))


