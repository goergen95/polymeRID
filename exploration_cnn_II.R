source("code/setup.R")
source("code/functions.R")
#system("source misc/cuda10.1-env")
# reading data based on class control file
data = read.csv(file = paste0(ref, "reference_database.csv"), header = TRUE)
data.norm = preprocess(data[,1:ncol(data)-1], type = "norm")

kernels = c(90,70,90,150)
folds = 10
repeats = 5
p = 0.5
nOutcome = length(unique(data$class))





dataList = list()

normd2.data = preprocess(data[,1:ncol(data)-1], type = "norm.d2")
normd2.data$class = data$class
dataList[["norm.d2"]] = normd2.data

sg.data = preprocess(data[,1:ncol(data)-1], type = "norm")
sg.data$class = data$class
dataList[["sg"]] = sg.data

rawd2.data = preprocess(data[,1:ncol(data)-1], type = "raw.d2")
rawd2.data$class = data$class
dataList[["raw.d2"]] = rawd2.data

normd1.data = preprocess(data[,1:ncol(data)-1], type = "norm.d1")
normd1.data$class = data$class
dataList[["norm.d1"]] = normd1.data


for (i in 1:length(dataList)){
  kernel = kernels[i]
  tmp = dataList[[i]]
  name = names(dataList)[i]

  # preparing data inputs
  set.seed(42)
  foldIndex = lapply(1:repeats, caret::createDataPartition, y=tmp$class, times = folds, p=p)
  foldIndex = do.call(c,foldIndex)

  cvData = list()
  for (rep in 1:repeats){
    rep_Index = foldIndex[(rep*folds-folds+1):(rep*folds)] #always jump to the correct number of folds forward for each repeat

    dataFold = lapply(1:folds,function(x){

      training = tmp[unlist(rep_Index[x]), ]
      validation = tmp[-unlist(rep_Index[x]), ]
      foldtmp = list(training,validation)
      names(foldtmp) = c("training","validation")
      return(foldtmp)
    })
    cvData[[rep]] = dataFold
  }
  results = data.frame(repeats = rep(0,repeats*folds),
                       fold = rep(0,repeats*folds),
                       loss = rep(0,repeats*folds),
                       acc = rep(0,repeats*folds))
  counter = 1
  for (rep in 1:repeats){
    #print(paste0("Starting repeat ",rep," out of ",repeats,"."))
    for (fold in 1:folds){

      variables = ncol(cvData[[rep]][[fold]][[1]])-1
      x_train = cvData[[rep]][[fold]][["training"]][,1:variables]
      y_train = unlist(cvData[[rep]][[fold]][["training"]][1+variables])
      x_test = cvData[[rep]][[fold]][["validation"]][,1:variables]
      y_test = unlist(cvData[[rep]][[fold]][["validation"]][1+variables])

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

      # fitting the model
      kernelMod = prepNNET(kernel, variables, nOutcome = nOutcome)
      historyMod =  keras::fit(kernelMod, x = x_train, y = y_train,
                               epochs=300,
                               batch_size = 10 )
      saveRDS(historyMod, file = paste0(output,"nnet/cv/history_K",kernel,"_",name,".rds"))

      evalK = keras::evaluate(kernelMod, x=x_test, y=y_test)
      results$repeats[counter] = rep
      results$fold[counter] = fold
      results$loss[counter] = evalK$loss
      results$acc[counter] = evalK$acc
      print(name)
      print(results[counter,])
      counter = counter + 1
      write.csv(results, file = paste0(output,"nnet/cv/cvResults_K",kernel,"_",name,".csv"))
    }
  }
}


