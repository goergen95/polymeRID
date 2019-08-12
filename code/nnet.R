source("/mnt/SSD/polymer/polymeRID/code/setup.R")
source("/mnt/SSD/polymer/polymeRID/code/functions.R")
#system("source misc/cuda10.1-env")
# reading data based on class control file
classes = readLines(paste0(ref, "classes.txt"))
data = lapply(classes,function(class){
  print(class)
  files = list.files(ref, full.names=TRUE)
  file = files[grep(paste(class, ".csv", sep=""), files)]
  data = read.csv(file, header = TRUE)
  return(data)
})
data2 = do.call("rbind",data)


kernels = c(2:100)
types = c("raw","norm", "sg", "sg.d1", "sg.d2",
         "sg.norm", "sg.norm.d1", "sg.norm.d2",
         "raw.d1", "raw.d2", "norm.d1", "norm.d2")
types = "raw"


results = data.frame(kernel =kernels,
                     loss = rep(0,length(kernels)),
                     acc = rep(0,length(kernels)),
                     val_loss=rep(0,length(kernels)),
                     val_acc=rep(0,length(kernels)))


for (kernel in kernels){


  for (type in types){
    if (type == "raw"){
      data = data2
      }else{
      data = preprocess(data2[,1:variables], type = type)
      data$class =data2$class
      }
    # we keep kernel size fixed at 50
    # this is apprx. window for peaks
    variables = ncol(data)-1
    # splitting between training and test
    set.seed(42)
    index = caret::createDataPartition(y=data$class,p=.5)
    train = data[index$Resample1,]
    test = data[-index$Resample1,]


    # splitting predictors and labels
    x_train = train[,1:variables]
    y_train = train[,1+variables]
    x_test = test[,1:variables]
    y_test = test[,1+variables]

    #number of preditors and unique targets
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

    # we apply two different neural nets to the data,
    # one representing a "large" neural network, e.g. much units, few layers,
    # and a "deep" neural network, e.g. few units much layers

    # contstruction of "large" neural network
prepNNET <- function(kernel,variables){modelL = keras_model_sequential()
    modelL %>%
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
      layer_global_average_pooling_1d(name="exit_average_pool") %>%
      layer_dropout(rate=0.5) %>%
      layer_dense(units = nOutcome, activation = "softmax")

    # we compile for a classification with the categorcial crossentropy loss function
    # and use adam as optimizer function
    compile(modelL,loss="categorical_crossentropy",optimizer="adam",metrics="accuracy")
  }
  modelL = prepNNET(kernel, variables)

    print(paste0("Training model with kernel size ",kernel," and preprocessing ",type))
    historyL = keras::fit(modelL, x = x_train, y = y_train,
                          epochs=100, validation_data = list(x_test,y_test),
                          callbacks =  callback_tensorboard(paste0(output,"nnet/logs")),
                          batch_size = 10 )

    saveRDS(modelL, file = paste0(output,"nnet/large/large_model_",type,"_kernel_",kernel,".rds"))
    saveRDS(historyL, file = paste0(output,"nnet/large/large_history_",type,"_kernel_",kernel,".rds"))
    results$loss[results$kernel == kernel] = historyL$metrics$loss[100]
    results$acc[results$kernel == kernel] = historyL$metrics$acc[100]
    results$val_loss[results$kernel == kernel] = historyL$metrics$val_loss[100]
    results$val_acc[results$kernel == kernel] = historyL$metrics$val_acc[100]

  }

print(results)
}

write.csv(results, file = paste0(output,"nnet/large/large_kernels.csv"))
#results = read.csv(paste0(output,"nnet/large/large_kernels.csv"))

# preparation of CV

# get kernel numbers for accuracies higher than 90
kernelInd = results$kernel[which(results$val_acc>0.9)]

nnetCV <- function(data,folds=10,repeats=15,p=0.5, kernel, seed=42){

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
      kernelMod = prepNNET(kernel, variables)
      historyMod =  keras::fit(kernelMod, x = x_train, y = y_train,
                               epochs=100,
                               callbacks =  callback_tensorboard(paste0(output,"nnet/logs")),
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
      write.csv(results, file = paste0(output,"cv/cvResults_K",kernel,".csv"))
    }
  }
  return(results)
}


cvResults = lapply(kernelInd,nnetCV,
                   data=data,
                   folds=10,
                   repeats=15,
                   p=0.5,
                   seed = 42)


