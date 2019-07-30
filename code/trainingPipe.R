#training funtion:
# training process is a chain of CV ->  PCA -> FFS
#create base plots for every polymer in database
source("code/setup.R")

classes = readLines(paste0(ref,"classes.txt"))
data = lapply(classes,function(x){
  specs = read.csv(list.files(ref,full.names=T)[grep(paste(x,".csv",sep=""),list.files(ref))],header=T)
  return(specs)
})
data = do.call("rbind",data)
wavenumbers = readRDS(paste0(ref,"wavenumbers.rds"))
TIME = format(Sys.time(),"%Y%m%d_%H%M")

## CV
# funcionality: get indices for the different folds by using caret functions
folds = 20
repeats = 10
threshold = 99
itertations = folds * repeats
foldIndex = caret::createMultiFolds(data$class,k=folds,times=repeats)
length(foldIndex[[2]])

# starting training loop
for (rep in 1:length(repeats)){
  tmpIndex = foldIndex[(rep*folds-folds+1):(rep*folds)]
  lapply(1:folds,function(x){
    training = data[unlist(tmpIndex[x]),]
    validation = data[-unlist(tmpIndex[x]),]
    responseTrain = training$class
    responseVal = validation$class
    pcaMod = prcomp(training[,1:ncol(data)-1])
    varInfo = factoextra::get_eigenvalue(pcaMod)
    thresInd = which(varInfo$cumulative.variance.percent>=threshold)[1]
    pcaTrain = pcaMod$x[,1:thresInd]
    pcaVal = predict(pcaMod,validation)[,1:thresInd]
  })
  
}
