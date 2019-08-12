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


