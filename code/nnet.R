source("code/setup.R")

# reading data based on class control file
classes = readLines(paste0(ref, "classes.txt"))
data = lapply(classes,function(class){
  print(class)
  files = list.files(ref, full.names=TRUE)
  file = files[grep(paste(class, ".csv", sep=""), files)]
  data = read.csv(file, header = TRUE)
  return(data)
})
data = do.call("rbind",data)

# we keep kernel size fixed at 50
# this is apprx. window for peaks
kernel = 50

# splitting between training and test
index = caret::createDataPartition(y=data$class,p=.7)
train = data[index$Resample1,]
test = data[-index$Resample1,]

#number of preditors and unique targets
variables = ncol(data)-1
nOutcome = length(levels(y_train))

# splitting predictors and labels
x_train = train[,1:variables]
y_train = train[,1+variables]
x_test = test[,1:variables]
y_test = test[,1+variables]

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

modelL = keras_model_sequential()
modelL %>%
  # block 1
  layer_conv_1d(filters = 100,
                kernel_size = kernel,
                input_shape = c(variables,1),
                name = "block1_conv1",) %>%
  layer_activation_relu(name="block1_relu1") %>%
  layer_conv_1d(filters = 100,
                kernel_size = kernel,
                name = "block1_conv2") %>%
  layer_activation_relu(name="block1_relu2") %>%
  layer_max_pooling_1d(strides=2,
                       pool_size = 5,
                       name="block1_max_pool1") %>%

  # block 2
  layer_conv_1d(filters = 200,
                kernel_size = kernel,
                name = "block2_conv1") %>%
  layer_activation_relu(name="block2_relu1") %>%
  layer_conv_1d(filters = 200,
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

#consruction of "deep" neural network

modelD = keras_model_sequential()
modelD %>%
  # block 1
  layer_conv_1d(filters = 8,
                kernel_size = kernel,
                input_shape = c(variables,1),
                name = "block1_conv1",) %>%
  layer_activation_relu(name="block1_relu1") %>%
  layer_conv_1d(filters = 8,
                kernel_size = kernel,
                name = "block1_conv2") %>%
  layer_activation_relu(name="block1_relu2") %>%
  layer_max_pooling_1d(strides=2,
                       pool_size = 2,
                       name="block1_pool") %>%

  # block 2
  layer_conv_1d(filters = 16,
                kernel_size = kernel,
                name = "block2_conv1") %>%
  layer_activation_relu(name="block2_relu1") %>%
  layer_conv_1d(filters = 16,
                kernel_size = kernel,
                name = "block2_conv2") %>%
  layer_activation_relu(name="block2_relu2") %>%
  layer_max_pooling_1d(strides=2,
                       pool_size = 5,
                       name="block2_pool") %>%
  # block 3
  layer_conv_1d(filters = 32,
                kernel_size = kernel,
                name = "block3_conv1") %>%
  layer_activation_relu(name="block3_relu1") %>%
  layer_conv_1d(filters = 32,
                kernel_size = kernel,
                name = "block3_conv2") %>%
  layer_activation_relu(name="block3_relu2") %>%
  # layer_max_pooling_1d(strides=2,
  #                      pool_size = 5,
  #                      name="block3_pool") %>%
  # block 4
  layer_conv_1d(filters = 32,
                kernel_size = kernel,
                name = "block4_conv1") %>%
  layer_activation_relu(name="block4_relu1") %>%
  layer_conv_1d(filters = 32,
                kernel_size = kernel,
                name = "block4_conv2") %>%
  layer_activation_relu(name="block4_relu2") %>%
  layer_max_pooling_1d(strides=2,
                       pool_size = 5,
                       name="block4_pool") %>%
  # block 5
  layer_conv_1d(filters = 16,
                kernel_size = kernel/2,
                name = "block5_conv1") %>%
  layer_activation_relu(name="block5_relu1") %>%
  layer_conv_1d(filters = 16,
                kernel_size = kernel/2,
                name = "block5_conv2") %>%
  layer_activation_relu(name="block5_relu2") %>%

  # exit block
  layer_global_average_pooling_1d(name="Exit_average_pool1") %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = nOutcome, activation = "softmax")


# we use the same compilation as for the "large" neural network
compile(modelD,loss="categorical_crossentropy",optimizer="adam",metrics="accuracy")


# let's proceed with a test run wich inclueds saving the results in a tensorflow
# format to a log directory

historyD = keras::fit(modelD, x = x_train, y = y_train,
                      epochs=50, steps_per_epoch= 5,
                      callbacks =  callback_tensorboard(paste0(output,"logs")))

historyL = keras::fit(modelL, x = x_train, y = y_train,
                      epochs=50, steps_per_epoch= 5,
                      callbacks =  callback_tensorboard(paste0(output,"logs")))





