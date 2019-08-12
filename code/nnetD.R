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
type = "raw"
for (kernel in kernels){
  results = data.frame(type = types,
                       loss = rep(0,length(types)),
                       acc = rep(0,length(types)),
                       val_loss=rep(0,length(types)),
                       val_acc=rep(0,length(types)))

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

    #consruction of "deep" neural network

    modelD = keras_model_sequential()
    modelD %>%
      # block 1
      layer_conv_1d(filters = 1,
                    kernel_size = kernel,
                    input_shape = c(variables,1),
                    name = "block1_conv1",) %>%
      layer_activation_relu(name="block1_relu1") %>%
      layer_conv_1d(filters = 1,
                    kernel_size = kernel,
                    name = "block1_conv2") %>%
      layer_activation_relu(name="block1_relu2") %>%
      layer_max_pooling_1d(strides=2,
                           pool_size = 2,
                           name="block1_pool") %>%

      # block 2
      layer_conv_1d(filters = 2,
                    kernel_size = kernel,
                    name = "block2_conv1") %>%
      layer_activation_relu(name="block2_relu1") %>%
      layer_conv_1d(filters = 2,
                    kernel_size = kernel,
                    name = "block2_conv2") %>%
      layer_activation_relu(name="block2_relu2") %>%
      # layer_max_pooling_1d(strides=2,
      #                      pool_size = 5,
      #                      name="block2_pool") %>%
      # block 3
      layer_conv_1d(filters = 3,
                    kernel_size = kernel,
                    name = "block3_conv1") %>%
      layer_activation_relu(name="block3_relu1") %>%
      layer_conv_1d(filters = 3,
                    kernel_size = kernel,
                    name = "block3_conv2") %>%
      layer_activation_relu(name="block3_relu2") %>%
      layer_max_pooling_1d(strides=2,
                           pool_size = 5,
                           name="block3_pool") %>%
      #block 4
      layer_conv_1d(filters = 4,
                    kernel_size = kernel,
                    name = "block4_conv1") %>%
      layer_activation_relu(name="block4_relu1") %>%
      layer_conv_1d(filters = 4,
                    kernel_size = kernel,
                    name = "block4_conv2") %>%
      layer_activation_relu(name="block4_relu2") %>%
      # layer_max_pooling_1d(strides=2,
      #                      pool_size = 5,
      #                      name="block4_pool") %>%
      # block 5
      layer_conv_1d(filters = 5,
                    kernel_size = kernel/2,
                    name = "block5_conv1") %>%
      layer_activation_relu(name="block5_relu1") %>%
      layer_conv_1d(filters = 5,
                    kernel_size = kernel/2,
                    name = "block5_conv2") %>%
      layer_activation_relu(name="block5_relu2") %>%
      layer_max_pooling_1d(strides=2,
                           pool_size = 5,
                           name="block5_pool") %>%

      # block 6
      # layer_conv_1d(filters = 64,
      #               kernel_size = kernel/3,
      #               name = "block6_conv1") %>%
      # layer_activation_relu(name="block6_relu1") %>%
      # layer_conv_1d(filters = 64,
      #               kernel_size = kernel/3,
      #               name = "block6_conv2") %>%
      # layer_activation_relu(name="block6_relu2") %>%
      # layer_max_pooling_1d(strides=2,
      #                      pool_size = 5,
      #                      name="block6_pool") %>%
      # block 7
      # layer_conv_1d(filters = 128,
      #               kernel_size = kernel/4,
      #               name = "block7_conv1") %>%
      # layer_activation_relu(name="block7_relu1") %>%
      # layer_conv_1d(filters = 128,
      #               kernel_size = kernel/4,
      #               name = "block7_conv2") %>%
      # layer_activation_relu(name="block7_relu2") %>%
      # layer_max_pooling_1d(strides=2,
      #                      pool_size = 5,
      #                      name="block7_pool") %>%

      # exit block
      layer_global_average_pooling_1d(name="Exit_average_pool1") %>%
      layer_dropout(rate=0.5) %>%
      layer_dense(units = nOutcome, activation = "softmax")


    # # we use the same compilation as for the "large" neural network
    compile(modelD,loss="categorical_crossentropy",optimizer="adam",metrics="accuracy")


    # let's proceed with a test run wich inclueds saving the results in a tensorflow
    # format to a log directory
    print(paste0("Training model with kernel size ",kernel," and preprocessing ",type))
    historyD = keras::fit(modelD, x = x_train, y = y_train, batch_size = 10,
                          epochs=100, #steps_per_epoch= 5,
                          callbacks =  callback_tensorboard(paste0(output,"logs")),
                          validation_data=list(x_test,y_test))
    # saveRDS(modelD, file = paste0(output,"modelD.rds"))
    # saveRDS(historyD, file = paste0(output,"historyD.rds"))
    #



    saveRDS(modelD, file = paste0(output,"nnet/deep/deep_modelD_",type,"_kernel_",kernel,".rds"))
    saveRDS(historyD, file = paste0(output,"nnet/deep/deep_historyD_",type,"_kernel_",kernel,".rds"))
    results$loss[results$type == type] = historyL$metrics$loss[100]
    results$acc[results$type == type] = historyL$metrics$acc[100]
    results$val_loss[results$type == type] = historyL$metrics$val_loss[100]
    results$val_acc[results$type == type] = historyL$metrics$val_acc[100]

  }

  write.csv(results, file = paste0(output,"nnet/depp/deep_kernel",kernel,".csv"))
}


