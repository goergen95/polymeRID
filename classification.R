# classification
source("code/setup_website.R")
source("code/functions.R")
MODEL = "BASE"
TYPE = "FUSION"
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
  print(x)
  specs = read.csv(list.files(ref,full.names=T)[grep(paste("_",x,".csv",sep=""),list.files(ref))],header=T)
  return(specs)
})
data = do.call("rbind",data)
wavenumbers = readRDS(paste0(model,"/wavenumbers.rds"))
# #wavenumbers = wavenumbers[1:2300]
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
names(samples) = names(data)[-ncol(data)]

dummy = as.matrix(samples)
baslineDummy = baseline(dummy,method="rfbaseline",span=NULL,NoXP=64,maxit=c(10))
spectra = getCorrected(baslineDummy)
samples = as.data.frame(spectra)

files = list.files(model, full.names = TRUE)
rfModRaw = readRDS(files[grep("rfModRaw.rds", files)])
rfModSG = readRDS(files[grep("rfModSG.rds", files)])
pcaRaw = readRDS(files[grep("rfModRawPCA.rds", files)])
pcaSG = readRDS(files[grep("rfModSGPCA.rds", files)])
cnnSG = keras::load_model_hdf5(files[grep("cnnSG",files)])
cnnD1 = keras::load_model_hdf5(files[grep("cnnD1", files)])


ids = list.files(smp,pattern = FORMAT)
ids = str_remove(ids, FORMAT)

if (TYPE == "FUSION"){

  # prepare data
  sampleRAW = samples
  sampleRAW[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  sampleSG =  preprocess(samples, type = "sg")
  sampleSG[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  sampleD1 = preprocess(samples, type = "raw.d1")


  nVarrfRaw = nrow(rfModRaw$importance)
  nVarrfSG = nrow(rfModSG$importance)
  pcaRAW = predict(pcaRaw, sampleRAW)
  pcaSG = predict(pcaSG, sampleSG)


  K = keras::backend()
  x_sampleSG = as.matrix(sampleSG)
  x = K$expand_dims(x_sampleSG, axis = 2L)
  x_sampleSG = K$eval(x)
  x_sampleD1 = as.matrix(sampleD1)
  x = K$expand_dims(x_sampleD1, axis = 2L)
  x_sampleD1 = K$eval(x)

  # dataNORM = as.data.frame(base::scale(data[,1:length(wavenumbers)]))
  #   dataSG = as.data.frame(prospectr::savitzkyGolay(data[,1:length(wavenumbers)], p = 3, w = 11, m = 0))
  #   dataSGNORM = as.data.frame(prospectr::savitzkyGolay(dataNORM, p = 3, w = 11, m = 0))
  #   dataSG[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  #   dataSGNORM[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  #   dataNORM[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  #   dataRAW = data[,-ncol(data)]
  #   dataRAW[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0


  # starting decision fusion process
  results = list()
  plotID = list()
  # predicting
  classRFRaw = as.character(stats::predict(rfModRaw, pcaRAW))
  propRFRaw =  stats::predict(rfModRaw, pcaRAW, type = "prob")
  classRFSG = as.character(stats::predict(rfModSG, pcaSG))
  propRFSG = stats::predict(rfModSG, pcaSG, type = "prob")
  classCNNSG = as.character(classes[keras::predict_classes(cnnSG, x_sampleSG)+1])
  propCNNSG = keras::predict_proba(cnnSG, x_sampleSG)
  classCNND1 = as.character(classes[keras::predict_classes(cnnD1, x_sampleD1)+1])
  propCNND1 = keras::predict_proba(cnnD1, x_sampleD1)

  # restructuring results
  probs = (propRFRaw + propRFSG + propCNND1 + propCNNSG) / 4
  pred = lapply(1:nrow(probs), function(x){
    which.max(probs[x,])
  })
  predVals = lapply(1:nrow(probs), function(x){
    probs[x,unlist(pred)[x]]
  })
  hits = lapply(1:nrow(probs), function(x){
    hits = sort(probs[x, ], decreasing = T)[1:3]
  })

  predVals = unlist(predVals)
  pred = names(unlist(pred))
  pred[which(pred %in% c("FIBRE","FUR","WOOD"))] = "OTHER"
  results = data.frame(id = ids, class = pred, prob = predVals, level = rep(0,Nsamples))
  for (id in 1:length(ids)){
    hit = hits[[id]]
    classes = names(hit)
    values = as.numeric(hit)
    sample = read.table(sampleList[id])
    names(sample) = c("wavenumbers", "reflectance")
    if(values[1] < .5) level = "very low agreement"
    if(values[1] >= .5 & values[1] < .6) level = "low agreement"
    if(values[1] >= .6 & values[1] < .7) level = "medium agreement"
    if(values[1] >= .7 & values[1] < .8) level = "high agreement"
    if(values[1] >= .9 ) level = "very high agreement"
    results$level[id] = level

    annotation = paste0(level,"\n",
                        classes[1], ": ", round(values[1], 3), "\n",
                        classes[2], ": ", round(values[2], 3), "\n",
                        classes[3], ": ", round(values[3], 3))
    class1 = samplePlot(data = data, sample = sample, class = classes[1], prob = annotation, name = ids[id], wavenumbers = wavenumbers)
    class2 = samplePlot(data = data, sample = sample, class = classes[2], wavenumbers = wavenumbers)
    class3 = samplePlot(data = data, sample = sample, class = classes[3], wavenumbers = wavenumbers)
    multiclass = gridExtra::grid.arrange(class1,class2,class3)
    ggsave(plot=multiclass,file=paste0(plots,"/",ids[id],"_probClasses.png"),dpi=300,device="png",units="cm",width=50,height=30)
  }
  write.csv(results, paste0(root, "/results_",TIME,".csv"))
}

