addNoise = function(data,levels = c(0,10,100,250,500),category="class"){
  data.return = list()
  for (n in levels){
    tmp = as.matrix(data[ , 1:ncol(data)-1])
    tmp = as.data.frame(jitter(tmp, n))
    tmp[category] = data[category]
    data.return[[paste("noise", n, sep="")]] = tmp
  }
  return(data.return)
}

preprocess = function(data, SGpara = list(p=3,w=11), lag = 15,
                      type = c("norm", "sg", "sg.d1", "sg.d2",
                               "sg.norm", "sg.norm.d1", "sg.norm.d2",
                               "raw.d1", "raw.d2", "norm.d1", "norm.d2")){
  if (length(type)!= 1){
    stop("Please provide only one type argument ot preprocess.")
  }
  types =  c("norm", "sg", "sg.d1", "sg.d2",
             "sg.norm", "sg.norm.d1", "sg.norm.d2",
             "raw.d1", "raw.d2", "norm.d1", "norm.d2")
  if (!type %in%  types){
    stop("The provided pre-processing type is unknown.")
  }

  if (type == "norm"){
    data.return = as.data.frame(base::scale(data, center = TRUE, scale = TRUE))
  }

  if (type == "sg"){
    data.return = as.data.frame(prospectr::savitzkyGolay(data, p = SGpara[[1]], w = SGpara[[2]], m = 0))
  }

  if (type == "sg.d1"){
    data.return = as.data.frame(prospectr::savitzkyGolay(data, p = SGpara[[1]], w = SGpara[[2]], m = 1))
  }

  if (type == "sg.d2"){
    data.return = as.data.frame(prospectr::savitzkyGolay(data, p = SGpara[[1]], w = SGpara[[2]], m = 2))
  }

  if (type == "sg.norm"){
    data_norm = base::scale(data, center = TRUE, scale = TRUE)
    data.return= as.data.frame(prospectr::savitzkyGolay(data_norm, p = SGpara[[1]], w = SGpara[[2]], m = 0))
  }

  if (type == "sg.norm.d1"){
    data_norm = base::scale(data, center = TRUE, scale = TRUE)
    data.return = as.data.frame(prospectr::savitzkyGolay(data_norm, p = SGpara[[1]], w = SGpara[[2]], m = 1))
  }

  if (type == "sg.norm.d2"){
    data_norm = base::scale(data, center = TRUE, scale = TRUE)
    data.return = as.data.frame(prospectr::savitzkyGolay(data_norm, p = SGpara[[1]], w = SGpara[[2]], m = 2))
  }

  if (type == "raw.d1"){
    data.return = as.data.frame(t(diff(t(data), differences = 1, lag = lag)))
  }

  if (type == "raw.d2"){
    data.return = as.data.frame(t(diff(t(data), differences = 2, lag = lag)))
  }

  if (type == "raw.norm.d1"){
    data_norm = base::scale(data, center = TRUE, scale = TRUE)
    data.return = as.data.frame(t(diff(t(data_norm), differences = 1, lag = lag)))
  }

  if ("raw.norm.d2" %in% type){
    data_norm = base::scale(data, center = TRUE, scale = TRUE)
    data.return = as.data.frame(t(diff(t(data_norm), differences = 2, lag = lag)))
  }

  return(data.return)
}



createTrainingSet = function(data, category = "class",
                             SGpara = list(p=3,w=11), lag = 15,
                             type = c("raw", "norm", "sg", "sg.d1", "sg.d2",
                               "sg.norm", "sg.norm.d1", "sg.norm.d2",
                               "raw.d1", "raw.d2", "norm.d1", "norm.d2")){



  types =  c("raw","norm", "sg", "sg.d1", "sg.d2",
             "sg.norm", "sg.norm.d1", "sg.norm.d2",
             "raw.d1", "raw.d2", "norm.d1", "norm.d2")
  if (sum(!type %in%  types) != 0 ){
    stop("There are unknown preprocessing types provided.")
  }


  data.return = list()
  for (noise in names(data)){

    tmp = as.data.frame(data[[noise]])
    classes = tmp[,category]
    tmp = tmp[!names(tmp) %in% category]

    if ("raw" %in% type){
      data.return[[noise]][["raw"]] = as.data.frame(data[[noise]])
    }

    if ("norm" %in% type){
      data_norm = preprocess(tmp, type="norm")
      data_norm[category] = classes
      data.return[[noise]][["norm"]] = data_norm
    }

    if ("sg" %in% type){
      data_sg = preprocess(tmp, type="sg", SGpara = SGpara)
      data_sg[category] = classes
      data.return[[noise]][["sg"]] = data_sg
    }

    if ("sg.d1" %in% type){
      data_sgd1 = preprocess(tmp, type="sg.d1", SGpara = SGpara)
      data_sgd1[category] = classes
      data.return[[noise]][["sg.d1"]] = data_sgd1
    }

    if ("sg.d2" %in% type){
      data_sgd2 = preprocess(tmp, type="sg.d2", SGpara = SGpara)
      data_sgd2[category] = classes
      data.return[[noise]][["sg.d2"]] = data_sgd2
    }

    if ("sg.norm" %in% type){
      data_sgnorm = preprocess(tmp, type="sg.norm", SGpara = SGpara)
      data_sgnorm[category] = classes
      data.return[[noise]][["sg.norm"]] = data_sgnorm
    }

    if ("sg.norm.d1" %in% type){
      data_sgnormd1 = preprocess(tmp, type="sg.norm.d1", SGpara = SGpara)
      data_sgnormd1[category] = classes
      data.return[[noise]][["sg.norm.d1"]] = data_sgnormd1
    }

    if ("sg.norm.d2" %in% type){
      data_sgnormd2 = preprocess(tmp, type="sg.norm.d2", SGpara = SGpara)
      data_sgnormd2[category] = classes
      data.return[[noise]][["sg.norm.d2"]] = data_sgnormd2
    }

    if ("raw.d1" %in% type){
      data_rawd1 = preprocess(tmp, type="raw.d1", lag = lag)
      data_rawd1[category] = data[category]
      data.return[[noise]][["raw.d1"]] = data_rawd1
    }

    if ("raw.d2" %in% type){
      data_rawd2 = preprocess(tmp, type="raw.d2", lag = lag)
      data_rawd2[category] = data[category]
      data.return[[noise]][["raw.d2"]] = data_rawd2
    }

    if ("raw.norm.d1" %in% type){
      data_norm = preprocess(tmp, type="raw.norm.d1", lag = lag)
      data_normd1[category] = data[category]
      data.return[[noise]][["norm.d1"]] = data_normd1
    }

    if ("raw.norm.d2" %in% type){
      data_norm = preprocess(tmp, type="raw.norm.d2", lag = lag)
      data_normd2[category] = data[category]
      data.return[[noise]][["norm.d2"]] = data_norm2
    }

  }
  return(data.return)
}


meanplot = function(data,wavenumbers,class){
  #prpare data
  cldata = data[data$class == class,]
  MIN = Rfast::colMins(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
  MAX = Rfast::colMaxs(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
  cldata$id = 1:length(cldata$class)
  cldata = reshape2::melt(cldata,id.vars = c("id","class"))
  cldata = Rmisc::summarySE(cldata,measurevar="value",groupvars = "variable")
  names(cldata)[3] ="mean"
  cldata$min = MIN
  cldata$max = MAX


  tmp = ggplot2::ggplot(data=cldata,aes(x=wavenumbers))+
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
    #print(paste0("Starting repeat ",rep," out of ",repeats,"."))
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
        tuneGrid = expand.grid(gamma =seq(0.1,1,0.2),cost = seq(1,5,1) )
        acc = c()
        models = list()
        for ( i in 1:nrow(tuneGrid)){
          Mods = e1071::svm(x = x_train, y = y_train,
                            kernel = "radial",
                            gamma = tuneGrid$gamma[i],
                            cost = tuneGrid$cost[i])
          acc = c(acc,Mods$fitted.accuracy)
          models[[i]] = Mods
        }

        bestMod = models[[which(acc == max(acc))[1]]]
        pred = predict(bestMod,x_test)
        confMat = caret::confusionMatrix(pred,y_test)
        foldMetric = confMat$overall[metric]
        paras = data.frame(gamma = bestMod$gamma, cost = bestMod$cost)
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
    ModFinal = e1071::svm(predictors,response,gamma=metrics$gamma[index],cost = metrics$cost[index])
  }
  output = list()
  output[[1]] = acc_metric
  output[[2]] = thresInd
  output[[3]] = results
  output[[4]] = ModFinal
  return(output)
}
