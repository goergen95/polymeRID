# test run
source("code/setup.R")
source("code/functions.R")


classes = readLines(paste0(ref,"classes.txt"))
data = lapply(classes,function(x){
  specs = read.csv(list.files(ref,full.names=T)[grep(paste(x,".csv",sep=""),list.files(ref))],header=T)
  return(specs)
})
data = do.call("rbind",data)
wavenumbers = readRDS(paste0(ref,"wavenumbers.rds"))
TIME = format(Sys.time(),"%Y%m%d_%H%M")


testDataset = createTestDataset(data,category = "class",noise=c(10,50,100),savG = list(p=3,w=11))
levels = c("clean","noise10","noise50","noise100")
types = c("raw","norm","sg","sg.d1","sg.d2","sg.norm","sg.norm.d1","sg.norm.d2","raw.d1","raw.d2","norm.d1","norm.d2")

levelNames = unlist(lapply(levels,function(x){
  return(rep(x,length(types)))
}))

results = data.frame(level=levelNames,type = types, kappa = rep(0,length(types)))
for (level in 1:length(levels)){
  for (type in 1:length(types)){
    print(levels[level])
    print(types[type])
    tmpData = testDataset[[level]][[type]]
    tmpData[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
    tmpModel = pcaCV(tmpData,folds = 15,repeats = 10,threshold = 99,metric = "Kappa")
    saveRDS(tmpModel,file = paste0(output,"testRun/model_",levels[level],"_",types[type],"_",round(tmpModel[[1]],2),".rds"))
    results[which(results$level==levels[level] & results$type==types[type]),"kappa"] = as.numeric(tmpModel[[1]])
    print(results)
  }
}
saveRDS(results,file=paste0(output,"testRunII/first_testRun.rds"))
