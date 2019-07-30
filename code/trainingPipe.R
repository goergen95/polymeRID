#training funtion:
# training process is a chain of CV ->  PCA -> FFS
#create base plots for every polymer in database
source("code/setup.R")

classes = readLines(paste0(ref,"classes.txt"))
data = lapply(classes,function(x){
  specs = read.csv(list.files(ref,full.names=T)[grep(paste(x,".csv",sep=""),list.files(ref))],header=T)
  return(specs)
})
data = do.call("rbind",data)
wavenumbers = readRDS(paste0(ref,"wavenumbers.rds"))
TIME = format(Sys.time(),"%Y%m%d_%H%M")

## CV
# funcionality: get indices for the different folds by using caret functions

pcaCV = function(data,folds=15,repeats=10,threshold=99,metric="Kappa"){

  foldIndex = caret::createMultiFolds(data$class,k=folds,times=repeats)
  pcaData = lapply(1:repeats,function(x) {return(0)})
  # starting training loop
  for (rep in 1:repeats){
    tmpIndex = foldIndex[(rep*folds-folds+1):(rep*folds)]
    pcaDataFold = lapply(1:folds,function(x){
      training = data[unlist(tmpIndex[x]),]
      validation = data[-unlist(tmpIndex[x]),]
      responseTrain = training$class
      responseVal = validation$class
      pcaMod = prcomp(training[,1:ncol(data)-1])
      varInfo = factoextra::get_eigenvalue(pcaMod)
      thresInd = which(varInfo$cumulative.variance.percent>=threshold)[1]
      pcaTrain = pcaMod$x[,1:thresInd]
      pcaVal = predict(pcaMod,validation)[,1:thresInd]
      training = as.data.frame(pcaTrain)
      training$response = responseTrain
      validation = as.data.frame(pcaVal)
      validation$response = responseVal
      foldtmp = list(training,validation)
      return(foldtmp)
    })
    pcaData[[rep]] = pcaDataFold
  }


  results = c()
  for (rep in 1:repeats){
    for (fold in 1:folds){
      variables = ncol(pcaData[[rep]][[fold]][[1]])-1
      x_train = pcaData[[rep]][[fold]][[1]][,1:variables]
      y_train = unlist(pcaData[[rep]][[fold]][[1]][1+variables])
      x_test = pcaData[[rep]][[fold]][[2]][,1:variables]
      y_test = unlist(pcaData[[rep]][[fold]][[2]][1+variables])

      first = floor(sqrt(ncol(x_train)))/3
      if(first==0) first <- 1
      second = floor(sqrt(ncol(x_train)))
      last = ncol(x_train)
      mtries = c(first,second,last)

      rfMods = lapply(1:length(mtries),function(x){return(0)})
      accuracy = c()
      for (mtry in mtries){
        rfMods[which(mtries==mtry)] = list(randomForest::randomForest(x_train,y_train,x_test,y_test,ntree=500,mtry=mtry))
        met = confusionMatrix(rfMods[[which(mtries==mtry)]]$test$predicted,y_test)$overall[metric]
        accuracy = c(accuracy,met)
      }
      rfMod = rfMods[[which(accuracy == max(accuracy))[1]]]
      pred = rfMod$test$predicted
      obsv = y_test
      confMat = caret::confusionMatrix(pred,obsv)
      foldMetric = confMat$overall[metric]
      results = c(results,foldMetric)
    }
  }
  resNames = unlist(lapply(1:rep,function(x){
    return(paste("rep",rep(x,folds),sep=""))
  }))
  names(results) = paste(resNames,rep(paste("fold",1:folds,sep=""),rep),sep="")
  acc_metric = data.frame(metric=mean(results))
  names(acc_metric) = metric
  pcaMod = prcomp(data[,1:ncol(data)-1])
  varInfo = factoextra::get_eigenvalue(pcaMod)
  thresInd = which(varInfo$cumulative.variance.percent>=threshold)[1]
  predictors = pcaMod$x[,1:thresInd]
  response = data[,ncol(data)]
  rfModFinal = randomForest::randomForest(predictors,response,ntree=500)
  output = list()
  output[[1]] = acc_metric
  output[[2]] = results
  output[[3]] = rfModFinal
  return(output)
}


test2 = pcaCV(data,folds=10,repeats = 6)
