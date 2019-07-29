# prepare training process
source("polymeRID/setup.R")
wavenumbers = readRDS(paste0(ref,"wavenumbers.rds"))

classes = readLines(paste0(ref,"classes.txt"))
data = lapply(classes,function(x){
  specs = read.csv(list.files(ref,full.names=T)[grep(paste(x,".csv",sep=""),list.files(ref))],header=T)
  return(specs)
})
data = do.call("rbind",data)

dataValsraw = as.matrix(data[,1:length(wavenumbers)])
dataVals = scale(dataValsraw,center=TRUE,scale=TRUE)
dataPCA = prcomp(dataVals)
varInfo = get_eigenvalue(dataPCA)
threshold = 99
thresInd = which(varInfo$cumulative.variance.percent>=threshold)[1]
varInfo$cumulative.variance.percent[1:thresInd]
pca.data = dataPCA$x[,1:thresInd]


# we see here that the first 47 principal components explain more
# than 99% of the variance in the dataset
# by using the PCA for the model stage we can reduce the input 
# dimensions very drastically from 1863 to 47 dimensions only
# this can significantly reduce the computation time
# let's test this with a simpe benchmark of a random forest model

target = data$class

# equal training conditions
control = trainControl(method="cv",number=10 ,classProbs=TRUE,allowParallel=TRUE)

# benchmark PCA based training
system.time({pcaMod = train(pca.data,target,trControl=control,method="rf",)})
# finished after 6.45 seconds and max kappa of 0.89
# benchmark raw data based training
system.time({rawMod = train(dataValsraw,target,trControl=control,method="rf",)})
#finished after 171 seconds and max kappa of 94

# significant different in kappa values




model = keras_model_sequential()
model %>%
  # block 1
  layer_conv_1d(filters = 100,
                kernel_size = 10,
                input_shape = c(dim(dataValsraw)[2],1),
                name = "conv1")%>% 
  layer_activation_relu(input_shape = c(1854,100),name="relu1") %>%
  layer_conv_1d(filters = 100,
                kernel_size = 10,
                name = "conv2")%>%
  layer_activation_relu(input_shape = c(1854,100),name="relu2") %>%
  layer_max_pooling_1d(strides=2,pool_size = 5,name="pool1")%>%
  # block 2
  layer_conv_1d(filters=64, kernel_size = 5, name="conv3")%>% 
  layer_activation_relu(input_shape = c(921,64),name="relu3")%>% 
  #layer_max_pooling_1d(strides=2,pool_size=3,name="pool2")%>%
  #block 3
  layer_conv_1d(filters=32,kernel_size = 3,name="conv4")%>% 
  layer_activation_relu(input_shape = c(915,32),name="relu4")%>% 
  layer_max_pooling_1d(strides=2,pool_size=3,name="pool3")%>% 
  #block 4
  layer_conv_1d(filters=16,kernel_size = 3,name="conv5")%>% 
  layer_activation_relu(input_shape = c(457,16),name="relu5")%>% 
  layer_max_pooling_1d(strides=2,pool_size=3,name="pool4")%>%
  #block 5
  layer_conv_1d(filters=16,kernel_size = 3,name="conv6")%>% 
  layer_activation_relu(input_shape = c(225,16),name="relu6")%>% 
  layer_max_pooling_1d(strides=2,pool_size=3,name="pool5")%>% 
  layer_flatten(name="flatter")%>%
  # prepare output
  layer_dense(units=896,name="dens1")%>% 
  #layer_activation_relu(input_shape = c(896),name="relu7")%>%
  layer_dense(units=224,name="dens2")%>% 
  #layer_activation_relu(input_shape = c(224),name="relu8")%>% 
  layer_dense(units=10,name="dens3") %>%
  #layer_activation_relu(input_shape = c(10),name="relu9")
print(model)


compile(model,loss="sparse_categorical_crossentropy",optimizer=optimizer_rmsprop(),metrics="accuracy")

train_x = as.array(dataValsraw)
train_x = array(NA,dim=c(dim(train_x),1))
train_x[1:185,1:1863,] = dataValsraw

nmtarget = as.numeric(target)-1

history = fit(model,train_x,nmtarget,epochs=100,validation_split = 0.5,batch_size = 5)






model = keras_model_sequential()
model %>%
  # block 1
  layer_conv_1d(filters = 100,
                kernel_size = 10,
                input_shape = c(dim(dataValsraw)[2],1),
                name = "conv1")%>% 
  layer_activation_relu(input_shape = c(1854,100),name="relu1") %>%
  layer_conv_1d(filters = 50,
                kernel_size = 10,
                name = "conv2",strides = 2)%>%
  layer_activation_relu(input_shape = c(923,50),name="relu2") %>%
  #layer_max_pooling_1d(strides=2,pool_size = 5,name="pool1")%>%
  #ayer_conv_1d(filters=25, kernel_size = 5,strides=2, name="conv3")%>%
  layer_activation_relu(input_shape = c(228,25),name="relu3")%>%
  #layer_max_pooling_1d(strides=2,pool_size = 5,name="pool2")%>%
  layer_conv_1d(filters=12, kernel_size = 3, strides=2, name="conv4")%>%
  layer_activation_relu(input_shape = c(228,25),name="relu4")%>%
  layer_flatten()%>%
  layer_dense(units=330,name="dens2")%>% 
  layer_dense(units=10,name="dens3") 
compile(model,loss="sparse_categorical_crossentropy",optimizer=optimizer_rmsprop(),metrics="accuracy")
history = fit(model,train_x,nmtarget,epochs=100,validation_split = 0.5,batch_size = 5)



target = data$class

# equal training conditions
control = trainControl(method="cv",number=10 ,classProbs=TRUE,allowParallel=TRUE)

# benchmark PCA based training
system.time({rfMod = train(pca.data,target,trControl=control,method="rf")})
system.time({svmMod = train(pca.data,target,trControl=control,method="svmRadial")})
system.time({adaMod = train(pca.data,target,trControl=control,method="ada")})
system.time({cfMod = train(pca.data,target,trControl=control,method="cforest")})
system.time({knnMod = train(pca.data,target,trControl=control,method="knn")})
system.time({mlpMod = train(pca.data,target,trControl=control,method="mlp")})
system.time({mlpMLMod = train(pca.data,target,trControl=control,method="mlpML")})
system.time({nnetMod = train(pca.data,target,trControl=control,method="nnet")})
system.time({svmPolyMod = train(pca.data,target,trControl=control,method="svmPoly")})
