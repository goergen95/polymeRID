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
d1.data = preprocess(data[,1:ncol(data)-1], type = "raw.d1")
sg.data$class = data$class
d1.data$class = data$class

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
    trainingD1 = d1.data[index$Resample1,]
    testingD1 = d1.data[-index$Resample1,]


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
    x_train = as.matrix(trainingSG[,1:ncol(trainingSG)-1])
    x = K$expand_dims(x_train, axis = 2L)
    x_train = K$eval(x)
    y_train = keras::to_categorical(as.numeric(trainingSG$class)-1, length(unique(trainingSG$class)))

    x_testSG = as.matrix(testingSG[,1:ncol(testingSG)-1])
    x = K$expand_dims(x_testSG, axis = 2L)
    x_testSG = K$eval(x)
    y_testSG = keras::to_categorical(as.numeric(testingSG$class)-1, length(unique(testingSG$class)))

    cnnSG = prepNNET(kernel = 70, variables = ncol(sg.data)-1, nOutcome = length(unique(sg.data$class)))
    history = fit(cnnSG, x_train, y_train, batch_size = 10, epochs = 300)


    x_train = as.matrix(trainingD1[,1:ncol(trainingD1)-1])
    x = K$expand_dims(x_train, axis = 2L)
    x_train = K$eval(x)
    y_train = keras::to_categorical(as.numeric(trainingD1$class)-1, length(unique(trainingD1$class)))

    x_testD1 = as.matrix(testingD1[,1:ncol(testingD1)-1])
    x = K$expand_dims(x_testD1, axis = 2L)
    x_testD1 = K$eval(x)
    y_testD1 = keras::to_categorical(as.numeric(testingD1$class)-1, length(unique(testingD1$class)))

    cnnD1 = prepNNET(kernel = 70, variables = ncol(d1.data)-1, nOutcome = length(unique(d1.data$class)))
    history = fit(cnnD1, x_train, y_train, batch_size = 10, epochs = 300)



    # predicting
    classes = unique(trainingRaw$class)
    classRFRaw = as.character(predict(rfModRaw, pcaRaw_testing))
    propRFRaw =  predict(rfModRaw, pcaRaw_testing, type = "prob")
    classRFSG = as.character(predict(rfModSG, pcaSG_testing))
    propRFSG = predict(rfModSG, pcaSG_testing, type = "prob")
    classCNNSG = as.character(classes[keras::predict_classes(cnnSG, x_testSG)+1])
    propCNNSG = keras::predict_proba(cnnSG, x_testSG)
    classCNND1 = as.character(classes[keras::predict_classes(cnnD1, x_testD1)+1])
    propCNND1 = keras::predict_proba(cnnD1, x_testD1)

    # probability
    probs = (propRFRaw + propRFSG + propCNND1 + propCNNSG) / 4
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
x_train = as.matrix(sg.data[,1:ncol(sg.data)-1])
x = K$expand_dims(x_train, axis = 2L)
x_train = K$eval(x)
y_train = keras::to_categorical(as.numeric(sg.data$class)-1, length(unique(sg.data$class)))

cnnSG = prepNNET(kernel = 70, variables = ncol(sg.data)-1, nOutcome = length(unique(sg.data$class)))
history = fit(cnnSG, x_train, y_train, batch_size = 10, epochs = 300)
keras::save_model_hdf5(cnnSG, filepath = paste0(mod, "BASE/cnnSG.hdf"))

#CNN D1
x_train = as.matrix(d1.data[,1:ncol(d1.data)-1])
x = K$expand_dims(x_train, axis = 2L)
x_train = K$eval(x)
y_train = keras::to_categorical(as.numeric(d1.data$class)-1, length(unique(d1.data$class)))

cnnD1 = prepNNET(kernel = 70, variables = ncol(d1.data)-1, nOutcome = length(unique(d1.data$class)))
history = fit(cnnD1, x_train, y_train, batch_size = 10, epochs = 300)
keras::save_model_hdf5(cnnD1, filepath = paste0(mod, "BASE/cnnD1.hdf"))


