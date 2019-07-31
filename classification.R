# classification
source("code/setup.R")
source("code/functions.R")
MODEL = "BASE"
FORMAT = ".txt"
TIME = format(Sys.time(),"%Y%m%d_%H%M")
dir.create(paste0(smp,TIME))


classes = readLines(paste0(ref,"classes.txt"))
data = lapply(classes,function(x){
  specs = read.csv(list.files(ref,full.names=T)[grep(paste(x,".csv",sep=""),list.files(ref))],header=T)
  return(specs)
})
data = do.call("rbind",data)
wavenumbers = readRDS(paste0(mod,MODEL,"/wavenumbers_",MODEL,".rds"))


sampleList = list.files(smp,pattern=FORMAT,full.names = TRUE)
Nsamples = length(sampleList)
prepSMP = function(x,wvn){
  tmp = read.table(x)
  names(tmp) = c("wavenumbers","reflectance")
  tmp = prospectr::resample2(tmp$reflectance,tmp$wavenumbers,wvn)
  return(tmp)
}

samples = lapply(sampleList,prepSMP,wavenumbers)
samples = as.data.frame(do.call("rbind",samples))
names(samples) = names(data[,-ncol(data)])

modsList = list.files(paste0(mod,MODEL),full.names = TRUE)
modsList = modsList[-grep("wavenumbers",modsList)]

# prepare PCA MODEL
pcaMOD = prcomp(data[,1:length(wavenumbers)])
ids = list.files(smp,pattern = FORMAT)
lapply(ids,function(x){dir.create(paste0(smp,TIME,"/",x))})
if (length(modsList == 3)){
  # starting decision fusion process
  rfNORM = readRDS(modsList[grep("rfNORM",modsList)])
  rfSGD2 = readRDS(modsList[grep("rfSGD2",modsList)])
  rfSGNORM = readRDS(modsList[grep("rfSGNORM",modsList)])
  results = list()
  plotID = list()
  for (i in 1:Nsamples){
    predNORM = predict(rfNORM[[4]],predict(pcaMOD,samples[i,])[,1:rfNORM[[2]]],type="prob")
    predSGD2 = predict(rfSGD2[[4]],predict(pcaMOD,samples[i,])[,1:rfSGD2[[2]]],type="prob")
    predSGNORM = predict(rfSGNORM[[4]],predict(pcaMOD,samples[i,])[,1:rfSGNORM[[2]]],type="prob")
    fusion = (predNORM + predSGD2 + predSGNORM) / 3
    fusion = as.data.frame(fusion)
    classVec = names(fusion)
    hit = which(fusion == max(fusion))
    pred = data.frame(ID = ids[i], class = classVec[hit],probability = as.numeric(fusion[hit]))
    if (pred$probability<0.5) pred$confidence = "no confidence"
    if (pred$probability>=0.5 & pred$probability<0.6) pred$confidence = "very low confidence"
    if (pred$probability>=0.6 & pred$probability<0.7) pred$confidence = "low confidence"
    if (pred$probability>=0.7 & pred$probability<0.8) pred$confidence = "medium confidence"
    if (pred$probability>=0.8 & pred$probability<0.9) pred$confidence = "high confidence"
    if (pred$probability>=0.9) pred$confidence = "very high confidence"
    results[[i]] = pred

    # get data for similarity plots
    hits = sort(fusion,decreasing = TRUE)[1:3]
    plotID[[i]] = hits
  }
  results = do.call("rbind",results)
}
write.csv(results,file = paste0(smp,TIME,"/results.csv"),row.names = FALSE)

# function in action
# first let's prepare the sample data
for (id in ids){
  sample_raw = read.table(paste0(smp,id))
  names(sample_raw) = c("wavenumbers","reflectance")
  wn_res = prospectr::resample2(sample_raw$reflectance,sample_raw$wavenumbers,wavenumbers)
  sample = data.frame(wavenumbers=wavenumbers,reflectance=wn_res)
  class = plotID[[which(ids == id)]]
  class1 = samplePlot(data = data, sample = sample, class = names(class)[1],prob = class[1])
  class2 = samplePlot(data = data, sample = sample, class = names(class)[2],prob = class[2])
  class3 = samplePlot(data = data, sample = sample, class = names(class)[3],prob = class[3])
  multiclass = gridExtra::grid.arrange(class1,class2,class3)
  ggsave(plot=multiclass,file=paste0(smp,TIME,"/",id,"/",id,"_probClasses.png"),dpi=600,device="png",units="cm",width=40,height=20)
}

for (i in 1:length(sampleList)){
  file.copy(sampleList[i],paste0(smp,TIME,"/",ids[i]))
  file.remove(sampleList[i])
}
