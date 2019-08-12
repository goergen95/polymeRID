source("/mnt/SSD/polymer/polymeRID/code/setup.R")
classes = readLines(paste0(ref, "classes.txt"))
data = lapply(classes,function(class){
  print(class)
  files = list.files(ref, full.names=TRUE)
  file = files[grep(paste(class, ".csv", sep=""), files)]
  data = read.csv(file, header = TRUE)
  return(data)
})
data = do.call("rbind",data)

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
                               epochs=200,
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
      #write.csv(results, file = paste0(output,"nnet/cv/cvResults_K",kernel,".csv"))
    }
  }
  return(results)
}












prepNNET <- function(kernel,variables,nOutcome){
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
    layer_global_max_pooling_1d(name="exit_average_pool") %>%
    layer_dropout(rate=0.5) %>%
    layer_dense(units = nOutcome, activation = "softmax")

  # we compile for a classification with the categorcial crossentropy loss function
  # and use adam as optimizer function
  compile(model,loss="categorical_crossentropy",optimizer="adam",metrics="accuracy")
}


cvResults = nnetCV(data=data,nOutcome = 14,folds=10,repeats=1,p=0.5,seed=42, kernel =89)

