#' Adding noise to spectra data
#'
#' Function to add different levels of noise to a dataset containing
#' reflectance spectra and class information.
#'
#' @param data A data frame object containing observations in rows and variables
#' in columns. The 'data' object is suspected to contain one column which can be
#' specified with the parameter 'category' which contains information about the
#' classes of the observations. All other columns are treated as variables.
#' @param levels A vector specifying the levels of noise that should be added by
#' \code{\link{base::jitter}} function. Note that the value '0' has to be added
#' to get an unjitereted version of 'data' as well.
#' @param category A string specifying the name of the column which contains
#' information about the classes of the observations.
#'
#' @return A list with as many elements as specified in the parameter 'levels'
#' each containing the jittered data corresponding to the 'level' values.
#' @export
#'
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

#' Apply a preprocessing technique to spectral data
#'
#'This function applies a specificable pre-processing technique to a data frame
#'containing spectral reflectance values.
#'
#' @param data A data frame object containing reflectance values only. This means,
#' class information about the observations should be excluded from the object.
#' @param SGpara List of integer values used to apply a Savitzkiy-Golay filter
#' from the 'prospectr' package. \code{\link{prospectr::jitter}}. These are 'p' the
#' polynomial order and 'w' the window size which must be odd.
#' @param lag An integer specifying the lag which should be used when calculating
#' the first or second derivative.
#' @param type A single charachter vector specifying the pre-processing to be
#' applied to the 'data' object.
#'
#' @return A data frame object with the same number of observations (rows) as the
#' input 'data'. Depending on the pre-processing 'type' number of columns might
#' be lower that in the input 'data'.
#' @export
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

  if (type == "norm.d1"){
    data_norm = base::scale(data, center = TRUE, scale = TRUE)
    data.return = as.data.frame(t(diff(t(data_norm), differences = 1, lag = lag)))
  }

  if ("norm.d2" %in% type){
    data_norm = base::scale(data, center = TRUE, scale = TRUE)
    data.return = as.data.frame(t(diff(t(data_norm), differences = 2, lag = lag)))
  }

  return(data.return)
}



#' Creating a Traning Set
#'
#' This function takes a single data frame or a list of data frames (e.g. an object
#' returned by the \code{\link{addNoise}} function) containing 'raw' reflectance
#' values and applies specificable pre-processing techniques. In the end, each of
#' the input elements undergoes the same pre-process and the resulting object
#' can be used in a training process.
#' @param data A data frame or a list of data frames containing 'raw' spectral
#' reflectance values. Note, that only numeric values are allowed except for one
#' column containing information of the classes of the observations. This column
#' can be specified in the 'category' parameter.
#' @param category A single string object specifiying the name of the column with
#' the information on the classes of the observations. This column will be excluded
#' from the pre-processing, but re-attached before the data objects are returned.
#' @param SGpara List of integer values used to apply a Savitzkiy-Golay filter
#' from the 'prospectr' package. \code{\link{prospectr::jitter}}. These are 'p' the
#' polynomial order and 'w' the window size which must be odd.
#' @param lag An integer specifying the lag which should be used when calculating
#' the first or second derivative.
#' @param type A vector of strings specifying the pre-processing techniques which
#' should be applied to the 'data' object.
#'
#' @return A list with elements equal to the product of the number of input
#' elements and the number of specified pre-processing techniques. At the first
#' level, the elements equal to the input 'data' can be accessed. Below that
#' level, the individual pre-processed data is found.
#' @export
createTrainingSet = function(data, category = "class",
                             SGpara = list(p=3,w=11), lag = 15,
                             type = c("raw", "norm", "sg", "sg.d1", "sg.d2",
                               "sg.norm", "sg.norm.d1", "sg.norm.d2",
                               "raw.d1", "raw.d2", "norm.d1", "norm.d2")){

  types = c("raw", "norm", "sg", "sg.d1", "sg.d2",
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
      data_rawd1[category] = classes
      data.return[[noise]][["raw.d1"]] = data_rawd1
    }

    if ("raw.d2" %in% type){
      data_rawd2 = preprocess(tmp, type="raw.d2", lag = lag)
      data_rawd2[category] = classes
      data.return[[noise]][["raw.d2"]] = data_rawd2
    }

    if ("raw.norm.d1" %in% type){
      data_norm = preprocess(tmp, type="raw.norm.d1", lag = lag)
      data_normd1[category] = classes
      data.return[[noise]][["norm.d1"]] = data_normd1
    }

    if ("raw.norm.d2" %in% type){
      data_norm = preprocess(tmp, type="raw.norm.d2", lag = lag)
      data_normd2[category] = classes
      data.return[[noise]][["norm.d2"]] = data_norm2
    }

  }
  return(data.return)
}


#' Spectra plot of polymer class
#'
#'This function executes some calculations to get a plot for a specificable
#'class found in the 'data' object. It calculates the mean value of all samples
#'from the class, indicates the standard deviation from that mean value with a
#'grey ribbon and additionaly shows the mimimal and maximum values for every
#'wavenumber.
#' @param data A data frame object containing spectral reflectance data of different
#' classes. All columns need to contain numeric values, except the last column
#' which is expected to contain the class information for the observations.
#' @param class A single string indicating the class for which a plot should
#' be created.
#' @param wavenumbers A numeric vector which specifies the wavenumbers which
#' should be included on the x-axis of the plot.
#'
#' @return A 'ggplot' object.
#' @export
meanplot = function(data, class, wavenumbers = NULL){
  #prpare data
  wvn =  as.numeric(stringr::str_remove(names(data), "wvn")[-ncol(data)])
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
    annotate(geom="text",label=paste0("class: ",class,"\nsamples: ",cldata$N[1]),x=500,y=max(cldata$mean))+
    ylab(label="reflectance")+
    theme_minimal()
  return(tmp)}



#' Sample plot of classfied spectra
#'
#'This function takes an vector of reflectanve values and plots it together
#'with a specified 'class' found in the 'data' object for comparison.
#' @param data A data frame object containng only numeric values except for the last
#' column which is expected to contain information on the class of the observations.
#' @param sample A numeric vector containing the spectral reflectance values of
#' a sample. It is expected that the wavenumbers are the same as in the 'data'
#' object.
#' @param class A charachter string specifying the class for which statistical metrics
#' should be included in the plot. Must be present in the last column of the
#' 'data' object.
#' @param probs Optional string containing the probability with which the sample
#' belongs to a specific class.
#' @param anno Optional string object which can be used to annotate the plot.
#'
#' @return A 'ggplot' object.
#' @export
#'
#' @examples
samplePlot = function(data,sample,class,probs="",anno=""){
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
    annotate(geom="text",label=anno,x=1000,y=Inf,hjust=1,vjust=1)+
    ylab(label="reflectance")+
    theme_minimal()
  return(figure)
}


#' Cross-Validation for RF and SVM
#'
#'This function is used to apply a cross-validation for Random Forest (RF)
#'and Support-Vector-Machine (SVM) with an included Principal Component Analysis
#'(PCA) used for dimensionality reduction of spectral reflectance data. Before
#'applying the PCA, the training and test set for each fold are splitted, so that
#'the PCA is truely independent from the test set.
#' @param data A data frame object which is expected to contain only numeric values
#' except for the last column where information about the classes of the observations
#' is expected
#' @param folds An integer specifying the number of folds in the cross-validation.
#' @param repeats An integer specifying the number of repeats for the cross-validation.
#' @param threshold An integer between 1 and 100 specifiying the cumulative explained
#' variance in the principal components which shall be used in training.
#' @param metric A charachter string specyfiyng the metric to keep track of. Currently,
#' only "Kappa" and "Accuracy" are valid parameters.
#' @param seed An integer used to ensure reproducibility when splitting the data.
#' @param p A numeric between 0 and 1, specifying the percentage of observations going
#' into training.
#' @param method A charachter specifiying if RF or SVM should be applied.
#'
#' @return A list object with four elements. The first element contains the
#' mean value of 'metric' accross all folds and repeats. The second element contains
#' the number of variables used after applying the PCA. The third element contains a vector
#' with all values of the 'metric' during CV. The fourth element contains
#' a final modell trained on all observations present in the 'data' object.
#' @export
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
      if (method == "convNet"){
        foldtmp = list(training,validation)
        return(foldtmp)
        next
      } else{
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
      }
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
      nObsv = nrow(x_train)
      nOutcome = length(levels(y_train))

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
          met = caret::confusionMatrix(pred,y_test)$overall[metric]
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
          pred = predict(Mods, x_train)
          conf = caret::confusionMatrix(pred, y_train)
          acc = c(acc, conf$overall[metric])
          models[[i]] = Mods
        }

        bestMod =  models[[which(acc == max(acc))[1]]]
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


#' Build and compile a CNN
#'
#'This function builds and compiles a convolutional neural network using the Keras
#'package with specified kernel size, number of variables an number of outcome
#'classes.
#' @param kernel An integer specifying the 'kernel_size' used in 1D-convolutional
#' layers in the CNN.
#' @param variables An integer speciying the input size. It is equal to the number
#' of variables of a spectral reflectance data set.
#' @param nOutcome The number of classes the model should be able to predict.
#'
#' @return A keras model object which can be used in training.
#' @export
prepCNN <- function(kernel,variables,nOutcome){
  model = keras_model_sequential()
  model %>%
  # block 1
  layer_conv_1d(filters = 8,
                kernel_size = kernel,
                input_shape = c(variables,1),
                name = "block1_conv1",) %>%
  layer_activation_relu(name="block1_relu1") %>%
  layer_conv_1d(filters = 16,
                kernel_size = kernel,
                name = "block1_conv2") %>%
  layer_activation_relu(name="block1_relu2") %>%
  layer_max_pooling_1d(strides=2,
                       pool_size = 5,
                       name="block1_max_pool1") %>%

  # block 2
  layer_conv_1d(filters = 32,
                kernel_size = kernel,
                name = "block2_conv1") %>%
  layer_activation_relu(name="block2_relu1") %>%
  layer_conv_1d(filters = 64,
                kernel_size = kernel,
                name = "block2_conv2") %>%
  layer_activation_relu(name="block2_relu2") %>%
  layer_max_pooling_1d(strides=2,
                       pool_size = 5,
                       name="block2_max_pool1") %>%

  # exit block
  layer_global_max_pooling_1d(name="exit_max_pool") %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = nOutcome, activation = "softmax")

# we compile for a classification with the categorcial crossentropy loss function
# and use adam as optimizer function
compile(model,loss="categorical_crossentropy",optimizer="adam",metrics="accuracy")
}


#' Cross-Validation for CNN
#'
#'This function applies a cross-validation for a CNN network compiled with
#'the keras package.
#' @param data A data frame object containing only numeric values except the last
#' column which is expected to cintain information on the classes of the observations.
#' @param folds An integer specyfiying the number of folds to be applied.
#' @param repeats An integer specyfiying the number of repeats to be applied.
#' @param p  An value between 0 and 1 specifying the percentage of observations
#' contained in the training dataset.
#' @param kernel An integer specifying the kernel_size in 1D-convolutional layers.
#' It is used when calling the \code{\link{prepCNN}}
#' @param seed An integer used to ensure reproducibility of the training/test split.
#' @param nOutcome An integer specifiying the number of classes in the outcome.
#'
#' @return A data frame object containing accuracy values for every fold and repeat.
#' @export
nnetCV <- function(data,folds=10,repeats=15,p=0.5, kernel, seed=42, nOutcome){

  # preparing data inputs
  set.seed(seed)
  foldIndex = lapply(1:repeats,caret::createDataPartition,y=data$class,times = folds,p=p)
  foldIndex = do.call(c,foldIndex)
  #foldIndex = caret::createMultiFolds(data$class,k=folds,times=repeats)
  cvData = lapply(1:repeats,function(x) {return(0)})
  # starting training loop
  for (rep in 1:repeats){
    tmpIndex = foldIndex[(rep*folds-folds+1):(rep*folds)] #always jump to the correct number of folds forward for each repeat
    dataFold = lapply(1:folds,function(x){

      training = data[unlist(tmpIndex[x]),]
      validation = data[-unlist(tmpIndex[x]),]
      foldtmp = list(training,validation)
      return(foldtmp)
    })
    cvData[[rep]] = dataFold
  }
  results = data.frame(kernel = rep(kernel, repeats*folds),
                       repeats = rep(0,repeats*folds),
                       fold = rep(0,repeats*folds),
                       loss = rep(0,repeats*folds),
                       acc = rep(0,repeats*folds))
  counter = 1
  for (rep in 1:repeats){
    #print(paste0("Starting repeat ",rep," out of ",repeats,"."))
    for (fold in 1:folds){
      variables = ncol(cvData[[rep]][[fold]][[1]])-1
      x_train = cvData[[rep]][[fold]][[1]][,1:variables]
      y_train = unlist(cvData[[rep]][[fold]][[1]][1+variables])
      x_test = cvData[[rep]][[fold]][[2]][,1:variables]
      y_test = unlist(cvData[[rep]][[fold]][[2]][1+variables])
      nObsv = nrow(x_train)
      nOutcome = length(levels(y_train))

      # function to get keras array for dataframes
      K <- keras::backend()
      df_to_karray <- function(df){
        tmp = as.matrix(df)
        tmp = K$expand_dims(tmp, axis = 2L)
        tmp = K$eval(tmp)
      }

      # coerce data to keras structure
      x_train = df_to_karray(x_train)
      x_test = df_to_karray(x_test)
      y_train = keras::to_categorical(as.numeric(y_train)-1,nOutcome)
      y_test = keras::to_categorical(as.numeric(y_test)-1,nOutcome)

      print(kernel)
      # fitting the model
      kernelMod = prepNNET(kernel, variables, nOutcome = nOutcome)
      historyMod =  keras::fit(kernelMod, x = x_train, y = y_train,
                               epochs=300,
                               #callbacks =  callback_tensorboard(paste0(output,"nnet/logs")),
                               batch_size = 10 )
      #saveRDS(historyMod, file = paste0(output,"cv/history_K",kernel,".rds"))
      #saveRDS(kernelMod, file = paste0(output,"cv/model_K",kernel,".rds"))

      evalK = keras::evaluate(kernelMod, x=x_test, y=y_test)
      results$kernel[counter] = kernel
      results$repeats[counter] = rep
      results$fold[counter] = fold
      results$loss[counter] = evalK$loss
      results$acc[counter] = evalK$acc
      print(results[counter,])
      counter = counter + 1
      #write.csv(results, file = paste0(output,"nnet/cv/cvResults_K",kernel,".csv"))
    }
  }
  return(results)
}
