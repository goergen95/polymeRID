kernel = 10
folds=15
repeats=10
threshold=99
metric="Kappa"
seed=42
p=0.5
method="convNet"
rep = 1
fold = 1



model = keras_model_sequential()
model %>%
  # block 1
  layer_conv_1d(filters = 50,
                kernel_size = kernel,
                input_shape = c(variables,1),
                name = "block1_conv1") %>%
  layer_activation_relu(name="block1_relu1") %>%
  layer_conv_1d(filters = 50,
                kernel_size = kernel,
                name = "block1_conv2") %>%
  layer_activation_relu(name="block1_relu2") %>%
  layer_max_pooling_1d(strides=2,
                       pool_size = 5,
                       name="block1_pool1") %>%

  # block 2
  layer_conv_1d(filters = 100,
                kernel_size = kernel,
                name = "block2_conv1") %>%
  layer_activation_relu(name="block2_relu1") %>%
  layer_conv_1d(filters = 100,
                kernel_size = kernel,
                name = "block2_conv2") %>%
  layer_activation_relu(name="block2_relu2") %>%
  layer_max_pooling_1d(strides=2,
                       pool_size = 5,
                       name="block2_pool1") %>%
  # block 3
  # layer_conv_1d(filters = 150,
  #               kernel_size = kernel,
  #               name = "block3_conv1") %>%
  # layer_activation_relu(name="block3_relu1") %>%
  # layer_conv_1d(filters = 150,
  #               kernel_size = kernel,
  #               name = "block3_conv2") %>%
  # layer_activation_relu(name="block3_relu2") %>%
  # layer_max_pooling_1d(strides=2,pool_size = 5,name="block3_pool1") %>%
  # #block 4
  # layer_conv_1d(filters = 200,
  #               kernel_size = kernel,
  #               name = "block4_conv1") %>%
  # layer_activation_relu(name="block4_relu1") %>%
  # layer_conv_1d(filters = 200,
  #               kernel_size = kernel,
  #               name = "block4_conv2") %>%
  # layer_activation_relu(name="block4_relu2") %>%
  # #block 5
  # layer_conv_1d(filters = 250,
  #               kernel_size = kernel,
  #               name = "block5_conv1") %>%
  # layer_activation_relu(name="block5_relu1") %>%
  # layer_conv_1d(filters = 250,
  #               kernel_size = kernel,
  #               name = "block5_conv2") %>%
  # layer_activation_relu(name="block5_relu2") %>%

  # exit block
  layer_global_average_pooling_1d(name="Exit_average_pool1") %>%
  layer_dropout(rate=0.5) %>%
  layer_dense(units = nOutcome, activation = "softmax")

model

# compile option 1
compile(model,loss="categorical_crossentropy",optimizer=adam(),metrics="accuracy")
# compile option 2
compile(model,loss="sparse_categorical_crossentropy",optimizer=optimizer_rmsprop(),metrics="accuracy")
predictors = as.matrix(as.matrix(x_train))
predictors = keras::k_expand_dims(predictors, axis = 0)
target = to_categorical(as.integer(y_train)-1,nOutcome, "double")
history = keras::fit(model,predictors,target,epochs=50,batch_size = 5, steps_per_epoch= 10)
