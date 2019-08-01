# classification
source("code/setup.R")
source("code/functions.R")
MODEL = "20190801_1458"
TYPE = "SGD2"
FORMAT = ".txt"
TIME = format(Sys.time(),"%Y%m%d_%H%M")
root = paste0(smp,TIME,"_",TYPE)
plots = paste0(root,"/plots")
raw = paste0(root,"/files")
dir.create(root)
dir.create(plots)
dir.create(raw)
model = paste0(mod,MODEL)

classes = readLines(paste0(ref,"classes.txt"))
data = lapply(classes,function(x){
  specs = read.csv(list.files(ref,full.names=T)[grep(paste(x,".csv",sep=""),list.files(ref))],header=T)
  return(specs)
})
data = do.call("rbind",data)
wavenumbers = readRDS(paste0(model,"/wavenumbers_",MODEL,".rds"))

wvn = as.numeric(str_remove(names(data)[-ncol(data)],"wvn"))
index = which(wvn %in% wavenumbers)
data = data[,c(index,ncol(data))]

sampleList = list.files(smp,pattern=FORMAT,full.names = TRUE)
if (length(sampleList)==0){
  cat("No samples present in sample directory")
  #quit(status = 1)
}
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
dummy = as.matrix(samples)
baslineDummy = baseline(dummy,method="rfbaseline",span=NULL,NoXP=64,maxit=c(10))
spectra = getCorrected(baslineDummy)
samples = as.data.frame(spectra)


modsList = list.files(model,full.names = TRUE)
modsList = modsList[-grep("wavenumbers",modsList)]

ids = list.files(smp,pattern = FORMAT)
#lapply(ids,function(x){dir.create(paste0(smp,TIME,"/",x))})
if (TYPE == "FUSION"){

  # prepare data
  sampleRAW = samples
  sampleRAW[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  sampleNORM = as.data.frame(base::scale(samples))
  sampleSGD2 = as.data.frame(prospectr::savitzkyGolay(samples, p = 3, w = 11, m = 2))
  sampleSGNORM = as.data.frame(prospectr::savitzkyGolay(sampleNORM, p = 3, w = 11, m = 0))
  sampleSGD2[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  sampleSGNORM[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  sampleNORM[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  sampleSGNORM[is.na(sampleSGNORM)] = 0
  sampleNORM[is.na(sampleNORM)] = 0


  dataNORM = as.data.frame(base::scale(data[,1:length(wavenumbers)]))
  dataSGD2 = as.data.frame(prospectr::savitzkyGolay(data[,1:length(wavenumbers)], p = 3, w = 11, m = 2))
  dataSGNORM = as.data.frame(prospectr::savitzkyGolay(dataNORM, p = 3, w = 11, m = 0))
  dataSGD2[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  dataSGNORM[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  dataNORM[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  dataRAW = data[,-ncol(data)]
  dataRAW[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0

  pcaRAW = prcomp(dataRAW)
  pcaNORM = prcomp(dataNORM)
  pcaSGD2 = prcomp(dataSGD2)
  pcaSGNORM = prcomp(dataSGNORM)

  # starting decision fusion process
  rfRAW = readRDS(modsList[grep("rfRAW",modsList)])
  rfNORM = readRDS(modsList[grep("rfNORM",modsList)])
  rfSGD2 = readRDS(modsList[grep("rfSGD2",modsList)])
  rfSGNORM = readRDS(modsList[grep("rfSGNORM",modsList)])
  results = list()
  plotID = list()
  for (i in 1:Nsamples){
    predRAW = predict(rfRAW[[4]],predict(pcaRAW,samples[i,])[,1:rfRAW[[2]]],type="prob")
    predNORM = predict(rfNORM[[4]],predict(pcaNORM,sampleNORM[i,])[,1:rfNORM[[2]]],type="prob")
    predSGD2 = predict(rfSGD2[[4]],predict(pcaSGD2,sampleSGD2[i,])[,1:rfSGD2[[2]]],type="prob")
    predSGNORM = predict(rfSGNORM[[4]],predict(pcaSGNORM,sampleSGNORM[i,])[,1:rfSGNORM[[2]]],type="prob")
    fusion = (predNORM + predSGD2 + predSGNORM + predRAW) / 4
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
  write.csv(results,file = paste0(root,"/results.csv"),row.names = FALSE)
  # function in action
  # first let's prepare the sample data
  data[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  samples[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  for (id in ids){
    sample_raw = read.table(paste0(smp,id))
    names(sample_raw) = c("wavenumbers","reflectance")
    wn_res = prospectr::resample2(sample_raw$reflectance,sample_raw$wavenumbers,wavenumbers)
    sample = data.frame(wavenumbers=wavenumbers,reflectance=wn_res)
    class = plotID[[which(ids == id)]]
    class1 = samplePlot(data = data, sample = sample, class = names(class)[1],prob = class[1], name = id)
    class2 = samplePlot(data = data, sample = sample, class = names(class)[2],prob = class[2])
    class3 = samplePlot(data = data, sample = sample, class = names(class)[3],prob = class[3])
    multiclass = gridExtra::grid.arrange(class1,class2,class3)
    ggsave(plot=multiclass,file=paste0(plots,"/",id,"_probClasses.png"),dpi=300,device="png",units="cm",width=50,height=30)
  }

  for (i in 1:length(sampleList)){
    file.copy(sampleList[i],paste0(raw,"/",ids[i]))
    #file.remove(sampleList[i])
  }
}


if (TYPE == "SGD2"){
  # prepare data
  sampleSGD2 = as.data.frame(prospectr::savitzkyGolay(samples, p = 3, w = 11, m = 2))
  sampleSGD2[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0

  dataSGD2 = as.data.frame(prospectr::savitzkyGolay(data[,1:length(wavenumbers)], p = 3, w = 11, m = 2))
  dataSGD2[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0

  pcaSGD2 = prcomp(dataSGD2)
  rfSGD2 = readRDS(modsList[grep("rfSGD2",modsList)])

  results = list()
  plotID = list()
  for (i in 1:Nsamples){
    predSGD2 = predict(rfSGD2[[4]],predict(pcaSGD2,sampleSGD2[i,])[,1:rfSGD2[[2]]],type="prob")
    probs = as.data.frame(predSGD2)
    classVec = names(probs)
    hit = which(probs == max(probs))
    pred = data.frame(ID = ids[i], class = classVec[hit],probability = as.numeric(probs[hit]))
    if (pred$probability<0.5) pred$confidence = "no confidence"
    if (pred$probability>=0.5 & pred$probability<0.6) pred$confidence = "very low confidence"
    if (pred$probability>=0.6 & pred$probability<0.7) pred$confidence = "low confidence"
    if (pred$probability>=0.7 & pred$probability<0.8) pred$confidence = "medium confidence"
    if (pred$probability>=0.8 & pred$probability<0.9) pred$confidence = "high confidence"
    if (pred$probability>=0.9) pred$confidence = "very high confidence"
    results[[i]] = pred

    # get data for similarity plots
    hits = sort(probs,decreasing = TRUE)[1:3]
    plotID[[i]] = hits
  }
  results = do.call("rbind",results)
  write.csv(results,file = paste0(root,"/results.csv"),row.names = FALSE)

  # function in action
  # first let's prepare the sample data
  data[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  samples[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  for (id in ids){
    sample_raw = read.table(paste0(smp,id))
    names(sample_raw) = c("wavenumbers","reflectance")
    wn_res = prospectr::resample2(sample_raw$reflectance,sample_raw$wavenumbers,wavenumbers)
    sample = data.frame(wavenumbers=wavenumbers,reflectance=wn_res)
    class = plotID[[which(ids == id)]]
    class1 = samplePlot(data = data, sample = sample, class = names(class)[1],prob = class[1], name = id)
    class2 = samplePlot(data = data, sample = sample, class = names(class)[2],prob = class[2])
    class3 = samplePlot(data = data, sample = sample, class = names(class)[3],prob = class[3])
    multiclass = gridExtra::grid.arrange(class1,class2,class3)
    ggsave(plot=multiclass,file=paste0(plots,"/",id,"_probClasses.png"),dpi=300,device="png",units="cm",width=50,height=30)
  }

  for (i in 1:length(sampleList)){
    file.copy(sampleList[i],paste0(raw,"/",ids[i]))
    #file.remove(sampleList[i])
  }

}

if (TYPE == "SGNORM"){
  # prepare data
  sampleNORM = as.data.frame(base::scale(samples))
  sampleSGNORM = as.data.frame(prospectr::savitzkyGolay(sampleNORM, p = 3, w = 11, m = 0))

  dataNORM = as.data.frame(base::scale(data[,1:length(wavenumbers)]))
  dataSGNORM = as.data.frame(prospectr::savitzkyGolay(dataNORM, p = 3, w = 11, m = 0))

  samplesSGNORM[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  dataSGNORM[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0

  pcaSGNORM = prcomp(dataSGNORM)
  rfSGNORM = readRDS(modsList[grep("rfSGNORM",modsList)])

  results = list()
  plotID = list()
  for (i in 1:Nsamples){
    predSGNORM = predict(rfSGNORM[[4]],predict(pcaSGNORM,sampleSGNORM[i,])[,1:rfSGNORM[[2]]],type="prob")
    probs = as.data.frame(predSGNORM)
    classVec = names(probs)
    hit = which(probs == max(probs))
    pred = data.frame(ID = ids[i], class = classVec[hit],probability = as.numeric(probs[hit]))
    if (pred$probability<0.5) pred$confidence = "no confidence"
    if (pred$probability>=0.5 & pred$probability<0.6) pred$confidence = "very low confidence"
    if (pred$probability>=0.6 & pred$probability<0.7) pred$confidence = "low confidence"
    if (pred$probability>=0.7 & pred$probability<0.8) pred$confidence = "medium confidence"
    if (pred$probability>=0.8 & pred$probability<0.9) pred$confidence = "high confidence"
    if (pred$probability>=0.9) pred$confidence = "very high confidence"
    results[[i]] = pred

    # get data for similarity plots
    hits = sort(probs,decreasing = TRUE)[1:3]
    plotID[[i]] = hits
  }
  results = do.call("rbind",results)
  write.csv(results,file = paste0(root,"/results.csv"),row.names = FALSE)

  # function in action
  # first let's prepare the sample data
  data[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  samples[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  for (id in ids){
    sample_raw = read.table(paste0(smp,id))
    names(sample_raw) = c("wavenumbers","reflectance")
    wn_res = prospectr::resample2(sample_raw$reflectance,sample_raw$wavenumbers,wavenumbers)
    sample = data.frame(wavenumbers=wavenumbers,reflectance=wn_res)
    class = plotID[[which(ids == id)]]
    class1 = samplePlot(data = data, sample = sample, class = names(class)[1],prob = class[1], name = id)
    class2 = samplePlot(data = data, sample = sample, class = names(class)[2],prob = class[2])
    class3 = samplePlot(data = data, sample = sample, class = names(class)[3],prob = class[3])
    multiclass = gridExtra::grid.arrange(class1,class2,class3)
    ggsave(plot=multiclass,file=paste0(plots,"/",id,"_probClasses.png"),dpi=300,device="png",units="cm",width=50,height=30)
  }

  for (i in 1:length(sampleList)){
    file.copy(sampleList[i],paste0(raw,"/",ids[i]))
    #file.remove(sampleList[i])
  }
}

if (TYPE == "NORM"){
  # prepare data
  sampleNORM = as.data.frame(base::scale(samples))
  sampleNORM[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0

  dataNORM = as.data.frame(base::scale(data[,1:length(wavenumbers)]))
  dataNORM[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0

  pcaNORM = prcomp(dataNORM)
  rfNORM = readRDS(modsList[grep("rfNORM",modsList)])

  results = list()
  plotID = list()
  for (i in 1:Nsamples){
    predNORM = predict(rfNORM[[4]],predict(pcaNORM,sampleNORM[i,])[,1:rfNORM[[2]]],type="prob")
    probs = as.data.frame(predNORM)
    classVec = names(probs)
    hit = which(probs == max(probs))
    pred = data.frame(ID = ids[i], class = classVec[hit],probability = as.numeric(probs[hit]))
    if (pred$probability<0.5) pred$confidence = "no confidence"
    if (pred$probability>=0.5 & pred$probability<0.6) pred$confidence = "very low confidence"
    if (pred$probability>=0.6 & pred$probability<0.7) pred$confidence = "low confidence"
    if (pred$probability>=0.7 & pred$probability<0.8) pred$confidence = "medium confidence"
    if (pred$probability>=0.8 & pred$probability<0.9) pred$confidence = "high confidence"
    if (pred$probability>=0.9) pred$confidence = "very high confidence"
    results[[i]] = pred

    # get data for similarity plots
    hits = sort(probs,decreasing = TRUE)[1:3]
    plotID[[i]] = hits
  }
  results = do.call("rbind",results)
  write.csv(results,file = paste0(root,"/results.csv"),row.names = FALSE)

  # function in action
  # first let's prepare the sample data
  data[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  samples[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  for (id in ids){
    sample_raw = read.table(paste0(smp,id))
    names(sample_raw) = c("wavenumbers","reflectance")
    wn_res = prospectr::resample2(sample_raw$reflectance,sample_raw$wavenumbers,wavenumbers)
    sample = data.frame(wavenumbers=wavenumbers,reflectance=wn_res)
    class = plotID[[which(ids == id)]]
    class1 = samplePlot(data = data, sample = sample, class = names(class)[1],prob = class[1], name = id)
    class2 = samplePlot(data = data, sample = sample, class = names(class)[2],prob = class[2])
    class3 = samplePlot(data = data, sample = sample, class = names(class)[3],prob = class[3])
    multiclass = gridExtra::grid.arrange(class1,class2,class3)
    ggsave(plot=multiclass,file=paste0(plots,"/",id,"_probClasses.png"),dpi=300,device="png",units="cm",width=50,height=30)
  }

  for (i in 1:length(sampleList)){
    file.copy(sampleList[i],paste0(raw,"/",ids[i]))
    #file.remove(sampleList[i])
  }
}

if (TYPE == "RAW"){
  # prepare data
  samplesRAW = samples
  samplesRAW[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0

  dataRAW = as.matrix(data[,-ncol(data)])
  dataRAW[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0

  pcaRAW = prcomp(dataRAW)
  rfRAW = readRDS(modsList[grep("rfRAW",modsList)])

  results = list()
  plotID = list()
  for (i in 1:Nsamples){
    predRAW = predict(rfRAW[[4]],predict(pcaRAW,sampleRAW[i,])[,1:rfRAW[[2]]],type="prob")
    probs = as.data.frame(predRAW)
    classVec = names(probs)
    hit = which(probs == max(probs))
    pred = data.frame(ID = ids[i], class = classVec[hit],probability = as.numeric(probs[hit]))
    if (pred$probability<0.5) pred$confidence = "no confidence"
    if (pred$probability>=0.5 & pred$probability<0.6) pred$confidence = "very low confidence"
    if (pred$probability>=0.6 & pred$probability<0.7) pred$confidence = "low confidence"
    if (pred$probability>=0.7 & pred$probability<0.8) pred$confidence = "medium confidence"
    if (pred$probability>=0.8 & pred$probability<0.9) pred$confidence = "high confidence"
    if (pred$probability>=0.9) pred$confidence = "very high confidence"
    results[[i]] = pred

    # get data for similarity plots
    hits = sort(probs,decreasing = TRUE)[1:3]
    plotID[[i]] = hits
  }
  results = do.call("rbind",results)
  write.csv(results,file = paste0(root,"/results.csv"),row.names = FALSE)

  # function in action
  # first let's prepare the sample data
  data[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  samples[which(wavenumbers<=2420 & wavenumbers>=2200)] = 0
  for (id in ids){
    sample_raw = read.table(paste0(smp,id))
    names(sample_raw) = c("wavenumbers","reflectance")
    wn_res = prospectr::resample2(sample_raw$reflectance,sample_raw$wavenumbers,wavenumbers)
    sample = data.frame(wavenumbers=wavenumbers,reflectance=wn_res)
    class = plotID[[which(ids == id)]]
    class1 = samplePlot(data = data, sample = sample, class = names(class)[1],prob = class[1], name = id)
    class2 = samplePlot(data = data, sample = sample, class = names(class)[2],prob = class[2])
    class3 = samplePlot(data = data, sample = sample, class = names(class)[3],prob = class[3])
    multiclass = gridExtra::grid.arrange(class1,class2,class3)
    ggsave(plot=multiclass,file=paste0(plots,"/",id,"_probClasses.png"),dpi=300,device="png",units="cm",width=50,height=30)
  }

  for (i in 1:length(sampleList)){
    file.copy(sampleList[i],paste0(raw,"/",ids[i]))
    #file.remove(sampleList[i])
  }
}


