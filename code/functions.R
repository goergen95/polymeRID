createTestDataset = function(data,category = "Abbreviation",noise = c(0,10,100,250,500),savG = list(p=3,w=11)){
  if(!category %in% names(data)) print("The categorial variable you provided does not match any column in the dataframe")

  addNoise = function(data,nlevel){
    tmp = as.matrix(data[,1:ncol(data)-1])
    tmp = as.data.frame(jitter(tmp, nlevel))
    tmp[category] = data[category]
    return(tmp)
  }
  data.return = list()
  for (n in noise){

    dataSave = data
    data = addNoise(data,nlevel=n)

    # center and scale data
    data.norm = as.data.frame(base::scale(data[,-which(names(data)==category)]))
    data.norm[category] = data[category]
    # savitzkiy golay filter on raw data
    data.sg = as.data.frame(prospectr::savitzkyGolay(data[,-which(names(data)==category)], p = savG[[1]], w = savG[[2]], m = 0))
    data.sg[category] = data[category]
    data.sg.d1 = as.data.frame(prospectr::savitzkyGolay(data[,-which(names(data)==category)], p = savG[[1]], w = savG[[2]], m = 1))
    data.sg.d1[category] = data[category]
    data.sg.d2 = as.data.frame(prospectr::savitzkyGolay(data[,-which(names(data)==category)], p = savG[[1]], w = savG[[2]], m = 2))
    data.sg.d2[category] = data[category]
    # savitzkiy golay filter on normalized data
    data.sg.norm = as.data.frame(prospectr::savitzkyGolay(data.norm[,-which(names(data.norm)==category)], p = savG[[1]], w = savG[[2]], m = 0))
    data.sg.norm[category] = data.norm[category]
    data.sg.d1.norm = as.data.frame(prospectr::savitzkyGolay(data.norm[,-which(names(data.norm)==category)], p = savG[[1]], w = savG[[2]], m = 1))
    data.sg.d1.norm[category] = data.norm[category]
    data.sg.d2.norm = as.data.frame(prospectr::savitzkyGolay(data.norm[,-which(names(data.norm)==category)], p = savG[[1]], w = savG[[2]], m = 2))
    data.sg.d2.norm[category] = data.norm[category]
    # 1st and 2nd derivative on raw data
    data.d1 = as.data.frame(t(diff(t(data[,-which(names(data)==category)]), differences = 1, lag = 11)))
    data.d1[category] = data[category]
    data.d2 = as.data.frame(t(diff(t(data[,-which(names(data)==category)]), differences = 2, lag = 11)))
    data.d2[category] = data[category]
    # 1st and 2nd derivative on normalized data
    data.d1.norm = as.data.frame(t(diff(t(data.norm[,-which(names(data.norm)==category)]), differences = 1, lag = 11)))
    data.d1.norm[category] = data.norm[category]
    data.d2.norm = as.data.frame(t(diff(t(data.norm[,-which(names(data.norm)==category)]), differences = 2, lag = 11)))
    data.d2.norm[category] = data.norm[category]

    # prepare for adding noises
    data.noised = list(data,
                       data.norm,
                       data.sg,
                       data.sg.d1,
                       data.sg.d2,
                       data.sg.norm,
                       data.sg.d1.norm,
                       data.sg.d2.norm,
                       data.d1,
                       data.d2,
                       data.d1.norm,
                       data.d2.norm)
    data = dataSave
    data.return[[which(noise == n)]] = data.noised
  }
  return(data.return)
}



trainTestDataset = function(data,category = "Abbreviation", ntree = 200, metric = "Kappa", clusterNumber = 7, levels = c("clean","noise10","noise50","noise100","noise250","noise500"),
                            types = c("raw","norm","sg","sg.d1","sg.d2","sg.norm","sg.norm.d1","sg.norm.d2","raw.d1","raw.d2","norm.d1","norm.d2")){
  levels = levels
  types = types
  variables = data.frame(var = 1:20,imp = 1:20)
  for (level in 1:length(levels)){
    levelData = data[[level]]
    for (type in 1:length(types)){
      trainingData = levelData[[type]]
      trCnt = trainCt = trainControl(method = "LOOCV", classProbs = TRUE)
      trainingData[,category] = as.factor(trainingData[,category])

      cl = parallel::makeCluster(clusterNumber)
      doParallel::registerDoParallel(cl)
      rfModel = train(x = trainingData[,1:ncol(trainingData)-1], y = trainingData[,category], method = "rf", trControl = trCnt, metric = metric, ntree = ntree)
      parallel::stopCluster(cl)
      saveRDS(rfModel, file = paste0("models/rfmodel_",levels[level],"_",types[type],".rds"))
      imp = varImp(rfModel)

      variables$var = attributes(imp$importance)$row.names[which(imp$importance$Overall %in% sort(imp$importance$Overall, decreasing = T)[1:20])]
      variables$imp = imp$importance$Overall[which(imp$importance$Overall %in% sort(imp$importance$Overall, decreasing = T)[1:20])]
      accuracy = rfModel$results[which(rfModel$results$Kappa == max(rfModel$results$Kappa)),]
      print(paste0("Level: ",levels[level]," Type: ",types[type]))
      print(accuracy)
      predictive = rfModel$pred
      conf = rfModel$finalModel$confusion
      results = list(variables,accuracy,predictive,conf)
      saveRDS(results,file = paste0("results/results_",levels[level],"_",types[type],".rds"))

    }
  }
}



meanplot = function(data,wavenumbers,class){
  #prpare data
  cldata = data[data$class == class,]
  MIN = Rfast::colMins(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
  MAX = Rfast::colMaxs(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
  cldata$id = 1:length(cldata$class)
  cldata = melt(cldata,id.vars = c("id","class"))
  cldata = Rmisc::summarySE(cldata,measurevar="value",groupvars = "variable")
  names(cldata)[3] ="mean"
  cldata$min = MIN
  cldata$max = MAX


  tmp = ggplot(data=cldata,aes(x=wavenumbers))+
    geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),fill="lightgrey",alpha=0.8)+
    geom_line(aes(y=mean),alpha=0.4)+
    geom_line(aes(y=max),linetype="dotted")+
    geom_line(aes(y=min),linetype="dotted")+
    annotate(geom="text",label=paste0("class: ",class,"\nsamples: ",cldata$N[1]),x=0,y=max(cldata$mean))+
    ylab(label="reflectance")+
    theme_minimal()
  return(tmp)}



samplePlot = function(data,sample,class,probs="",name=""){
  cldata = data[data$class == class,]
  MIN = Rfast::colMins(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
  MAX = Rfast::colMaxs(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
  cldata$id = 1:length(cldata$class)
  cldata = melt(cldata,id.vars = c("id","class"))
  cldata = Rmisc::summarySE(cldata,measurevar="value",groupvars = "variable")
  names(cldata)[3] ="mean"
  cldata$min = MIN
  cldata$max = MAX
  #if (probs<0.5) prop = "no confidence"
  #if (probs>=0.5 & probs<0.6) prop = "very low confidence"
  #if (probs>=0.6 & probs<0.7) prop = "low confidence"
  #if (probs>=0.7 & probs<0.8) prop = "medium confidence"
  #if (probs>=0.8 & probs<0.9) prop = "high confidence"
  #if (probs>=0.9) prop = "very high confidence"
  figure = ggplot(data=cldata,aes(x=wavenumbers))+
    geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),fill="lightgrey",alpha=0.8)+
    geom_line(aes(y=mean),alpha=0.4)+
    geom_line(aes(y=max),linetype="dotted")+
    geom_line(aes(y=min),linetype="dotted")+
    geom_line(data=sample,aes(y=reflectance),color="red")+
    annotate(geom="text",label=paste0("Class: ",class,
                                      "\nSamples: ",cldata$N[1],
                                      #"\nProbability: ",round(probs,3),
                                      #"\nConfidence: ",
                                      "\n",probs),x=3500,y=Inf,hjust=1,vjust=1)+
    annotate(geom="text",label=name,x=1000,y=Inf,hjust=1,vjust=1)+
    ylab(label="reflectance")+
    theme_minimal()
  return(figure)
}




## CV
# funcionality: get indices for the different folds by using caret functions

pcaCV = function(data,folds=15,repeats=10,threshold=99,metric="Kappa",seed=42,p=0.5,method="svm"){
  set.seed(seed)
  foldIndex = lapply(1:repeats,caret::createDataPartition,y=data$class,times = folds,p=p)
  foldIndex = do.call(c,foldIndex)
  #foldIndex = caret::createMultiFolds(data$class,k=folds,times=repeats)
  pcaData = lapply(1:repeats,function(x) {return(0)})
  # starting training loop
  for (rep in 1:repeats){
    tmpIndex = foldIndex[(rep*folds-folds+1):(rep*folds)] #always jump to the correct number of folds forward for each repeat
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
  if (method == "svm"){
    metrics = data.frame(gamma=rep(0,repeats*folds),
                         cost=rep(0,repeats*folds),
                         metric=rep(0,repeats*folds))
  }
  counter = 1
  for (rep in 1:repeats){
    for (fold in 1:folds){
      variables = ncol(pcaData[[rep]][[fold]][[1]])-1
      x_train = pcaData[[rep]][[fold]][[1]][,1:variables]
      y_train = unlist(pcaData[[rep]][[fold]][[1]][1+variables])
      x_test = pcaData[[rep]][[fold]][[2]][,1:variables]
      y_test = unlist(pcaData[[rep]][[fold]][[2]][1+variables])


      if (method == "rf"){
        first = floor(sqrt(ncol(x_train)))/3
        if(first <= 1) first <- 1
        second = floor(sqrt(ncol(x_train)))
        last = ncol(x_train)
        mtries = c(first,second,last)

        Mods = lapply(1:length(mtries),function(x){return(0)})
        accuracy = c()
        for (mtry in mtries){
          Mods[which(mtries==mtry)] = list(randomForest::randomForest(x_train,y_train,ntree=500,mtry=mtry))
          pred = predict(Mods[[which(mtries==mtry)]],x_test)
          met = confusionMatrix(pred,y_test)$overall[metric]
          accuracy = c(accuracy,met)
        }
        Mod = Mods[[which(accuracy == max(accuracy))[1]]]
        pred = predict(Mod,x_test)
        confMat = caret::confusionMatrix(pred,y_test)
        foldMetric = confMat$overall[metric]
        results = c(results,foldMetric)
      }

      if (method == "svm"){
        tuneGrid = expand.grid(gamma =10^(-10:-1),cost = 10^(-10:1) )
        for ( i in nrow(tuneGrid)){
          Mods = parallelSVM::parallelSVM(x = x_train, y = y_train,
                                          numberCores = 1,
                                          samplingSize = 0.8,
                                          scale = FALSE,type = "C",
                                          kernel = "radial",
                                          gamma = tuneGrid$gamma[i],
                                          cost = tuneGrid$cost[i])
        }

        # Mods = tune.svm(x = x_train,y = y_train,data=data,gamma=10^(-10:-1),cost=10^(-10:1))
        pred = predict(Mods$best.model,x_test)
        confMat = caret::confusionMatrix(pred,y_test)
        foldMetric = confMat$overall[metric]
        paras = Mods$best.parameters
        paras[,metric] = confMat$overall[metric]
        metrics[counter,] = paras
        counter = counter + 1
        results = c(results,foldMetric)
      }

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

  if (method == "rf"){
    ModFinal = randomForest::randomForest(predictors,response,ntree=500)
  }
  if (method == "svm"){
    index = which(metrics$metric == max(metrics$metric))
    ModFinal = svm(predictors,response,gamma=metrics$gamma[index],cost = metrics$cost[index])
  }
  output = list()
  output[[1]] = acc_metric
  output[[2]] = thresInd
  output[[3]] = results
  output[[4]] = ModFinal
  return(output)
}
