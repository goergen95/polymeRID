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
data = do.call("rbind",data)


results = read.csv(paste0(output,"nnet/large/large_kernels.csv"))
# get kernel numbers for accuracies higher than 90
kernelInd = results$kernel[which(results$val_acc>0.9)]

cvResults = lapply(kernelInd, nnetCV,
                   nOutcome = length(levels(data$class)),
                   data=data,
                   folds=10,
                   repeats=5,
                   p=0.5,
                   seed = 42)


