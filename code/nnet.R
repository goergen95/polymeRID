source("/mnt/SSD/polymer/polymeRID/code/setup_website.R")
source("/mnt/SSD/polymer/polymeRID/code/functions.R")
#system("source misc/cuda10.1-env")
# reading data based on class control file
classes = readLines(paste0(ref, "classes.txt"))
data = lapply(classes,function(class){
  print(class)
  files = list.files(ref, full.names=TRUE)
  file = files[grep(paste("_", class,".csv", sep=""), files)]
  data = read.csv(file, header = TRUE)
  return(data)
})
data = do.call("rbind",data)

kernels = c(10,20,30,40,50,60,70,80,90,100,125,150,175,200)
types = c("raw","norm","sg","sg.norm","raw.d1")


results = data.frame(types = rep(0, length(kernels) * length(types)),
                     kernel =rep(0, length(kernels) * length(types)),
                     loss = rep(0, length(kernels) * length(types)),
                     acc = rep(0, length(kernels) * length(types)),
                     val_loss=rep(0, length(kernels) * length(types)),
                     val_acc=rep(0, length(kernels) * length(types)))

variables = ncol(data)-1
counter = 1

  for (type in types){
    if (type == "raw"){
      tmp = data
      variables = ncol(data)-1
    }else{
      tmp = preprocess(data[ ,1:ncol(data)-1], type = type)
      variables = ncol(tmp)
      tmp$class =data$class
    }



for (kernel in kernels){


    # splitting between training and test
    set.seed(42)
    index = caret::createDataPartition(y=tmp$class,p=.5)
    training = tmp[index$Resample1,]
    validation = tmp[-index$Resample1,]


    # splitting predictors and labels
    x_train = training[,1:variables]
    y_train = training[,1+variables]
    x_test = validation[,1:variables]
    y_test = validation[,1+variables]

    #number of preditors and unique targets
    nOutcome = length(levels(y_train))

    # function to get keras array for dataframes
    K <- keras::backend()
    df_to_karray <- function(df){
      d = as.matrix(df)
      d = K$expand_dims(d, axis = 2L)
      d = K$eval(d)
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
    model = prepNNET(kernel, variables, nOutcome)

    print(paste0("Training model with kernel size ",kernel," and preprocessing ",type))
    history = keras::fit(model, x = x_train, y = y_train,
                          epochs=200, validation_data = list(x_test,y_test),
                          #callbacks =  callback_tensorboard(paste0(output,"nnet/logs")),
                          batch_size = 10 )
    saveRDS(history, file = paste0(output,"nnet/history_",type,"_kernel_",kernel,".rds"))
    results$types[counter] = type
    results$kernel[counter] = kernel
    results$loss[counter] = history$metrics$loss[100]
    results$acc[counter] = history$metrics$acc[100]
    results$val_loss[counter] = history$metrics$val_loss[100]
    results$val_acc[counter] = history$metrics$val_acc[100]
    write.csv(results, file = paste0(output,"nnet/kernels.csv"))
    counter = counter + 1
  }

  print(results)
}




