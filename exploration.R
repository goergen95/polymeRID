# test run
source("code/setup.R")
source("code/functions.R")

# name of the column containing the class information
category = "class"
TIME = format(Sys.time(), "%Y%m%d_%H%M")
dir.create(paste0(output,TIME))
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
wavenumbers = readRDS(paste0(ref,"wavenumbers.rds"))

#==============================================================================#
                      # starting the exploration stage #

# First lets create some noise in our dataset
noisyData = addNoise(data,levels = c(0,10,100,250,500), category = category)

# Second we apply different types of pre-processing to the data
testDataset = createTrainingSet(noisyData, category = category,
                                SGpara = list(p=3, w=11), lag=15,
                                type = c("raw", "norm", "sg", "sg.d1", "sg.d2",
                                  "sg.norm", "sg.norm.d1", "sg.norm.d2",
                                  "raw.d1", "raw.d2", "norm.d1", "norm.d2"))

# we obtained a large list object with the following structure
#   $noise0
#     $type1
#     $...
#     $typeN
#   $...
#     $type1
#     $...
#     $typeN
#   $noiseN
#     $type1
#     $...
#     $typeN
# we will use that object in a training loop, extracting the desired accuracy
# value and saving the models to disk
# we use another user defined function for a cross validation process

# lets prepare the selection vectors for the loop
types = names(testDataset[[1]])
levels = lapply(names(testDataset), function(x){
  rep(x, length(types))
})
levels = unlist(levels)
results = data.frame(level=levels,type = types, kappa = rep(0,length(levels)))

for (level in levels){
  for (type in types){

    print(paste0("Level: ",level," Type: ",type))
    tmpData = testDataset[[level]][[type]]
    tmpData[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0 # setting C02 window to 0
    tmpModel = pcaCV(tmpData,folds = 10,repeats = 5,threshold = 99,metric = "Kappa",p=0.5,method="rf")
    saveRDS(tmpModel,file = paste0(output,TIME,"/model_",level,"_",type,"_",round(tmpModel[[1]],2),".rds"))
    results[which(results$level==level & results$type==type),"kappa"] = as.numeric(tmpModel[[1]])
    print(results)

  }
}
saveRDS(results,file=paste0(output,TIME,"/exploration_",TIME,".rds"))
