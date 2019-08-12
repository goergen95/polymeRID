# classification
source("code/setup.R")
source("code/functions.R")
MODEL = "20190812_1717"
TYPE = "NNET"
FORMAT = ".txt"
PROBS = TRUE
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
names(samples) = names(data[,-ncol(data)])
dummy = as.matrix(samples)
baslineDummy = baseline(dummy,method="rfbaseline",span=NULL,NoXP=64,maxit=c(10))
spectra = getCorrected(baslineDummy)
samples = as.data.frame(spectra)
wvn = as.numeric(str_remove(names(samples),"wvn"))
index = which(wvn %in% wavenumbers)
samples = samples[,index]


modsList = list.files(model,full.names = TRUE)
modsList = modsList[-grep("wavenumbers",modsList)]

ids = list.files(smp,pattern = FORMAT)



if (TYPE == "NNET"){
  index = which(wavenumbers<=2420 & wavenumbers>=1900)
  samples[,index] = 0
  model = keras::load_model_hdf5(paste0(mod,MODEL,"/",MODEL,"_convnet.hdf"))
  K <- keras::backend()
  df_to_karray <- function(df){
    tmp = as.matrix(df)
    tmp = K$expand_dims(tmp, axis = 2L)
    tmp = K$eval(tmp)
  }
  predictors = df_to_karray(samples)
  pred = keras::predict_proba(model,predictors)

  outcomes = data.frame(class1 = rep(0,nrow(pred)),
                        class2 = rep(0,nrow(pred)),
                        class3 = rep(0,nrow(pred)))
  probs = data.frame(class1 = rep(0,nrow(pred)),
                     class2 = rep(0,nrow(pred)),
                     class3 = rep(0,nrow(pred)))
  conf = data.frame(conf = rep(0,nrow(pred)))
  for (id in ids){
    index = which(id == ids)
    sample = as.data.frame(t(samples[index,]))
    sample$wavenmubers = wavenumbers
    names(sample)[1] = c("reflectance")

    hits = sort(pred[index,],decreasing = TRUE)[1:3]
    hitsIndex = which(pred[index,] %in% hits)
    classVec = classes[hitsIndex]
    outcomes[index,] = classVec
    probs[index,] = hits
    if (hits[1]<0.5) confidence = "no confidence"
    if (hits[1]>=0.5 & hits[1]<0.6) confidence = "very low confidence"
    if (hits[1]>=0.6 & hits[1]<0.7) confidence = "low confidence"
    if (hits[1]>=0.7 & hits[1]<0.8) confidence = "medium confidence"
    if (hits[1]>=0.8 & hits[1]<0.9) confidence = "high confidence"
    if (hits[1]>=0.9) confidence = "very high confidence"
    conf[index,confidence]
    annotation = paste0(confidence," ",
                        classVec[1],": ",round(hits[1],10),"\n",
                        classVec[2],": ",round(hits[2],10),"\n",
                        classVec[3],": ",round(hits[3],10))
    class1 = samplePlot(data = data, sample = sample, class = classVec[1],prob = annotation, name = id)
    class2 = samplePlot(data = data, sample = sample, class = classVec[2])
    class3 = samplePlot(data = data, sample = sample, class = classVec[3])
    multiclass = gridExtra::grid.arrange(class1,class2,class3)
    ggsave(plot=multiclass,file=paste0(plots,"/",id,"_probClasses.png"),dpi=300,device="png",units="cm",width=50,height=30)
  }
  results = data.frame(id = ids,conf = conf$conf,
                       class1=outcomes$class1,prop1=probs$class1,
                       class2=outcomes$class2,prop2=probs$class2,
                       class3=outcomes$class3,prop3=probs$class3)
  write.csv(results, file = paste0(file = paste0(root,"/",TYPE,"_results.csv"),row.names = FALSE))
  for (i in 1:length(sampleList)){
    file.copy(sampleList[i],paste0(raw,"/",ids[i]))
  }
}
#lapply(ids,function(x){dir.create(paste0(smp,TIME,"/",x))})
if (TYPE == "FUSION"){

  # prepare data
  sampleRAW = samples
  sampleRAW[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  sampleNORM = as.data.frame(base::scale(samples))
  sampleSG = as.data.frame(prospectr::savitzkyGolay(samples, p = 3, w = 11, m = 0))
  sampleSGNORM = as.data.frame(prospectr::savitzkyGolay(sampleNORM, p = 3, w = 11, m = 0))
  sampleSG[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  sampleSGNORM[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  sampleNORM[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  sampleSGNORM[is.na(sampleSGNORM)] = 0
  sampleNORM[is.na(sampleNORM)] = 0


  dataNORM = as.data.frame(base::scale(data[,1:length(wavenumbers)]))
  dataSG = as.data.frame(prospectr::savitzkyGolay(data[,1:length(wavenumbers)], p = 3, w = 11, m = 0))
  dataSGNORM = as.data.frame(prospectr::savitzkyGolay(dataNORM, p = 3, w = 11, m = 0))
  dataSG[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  dataSGNORM[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  dataNORM[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  dataRAW = data[,-ncol(data)]
  dataRAW[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0

  pcaRAW = prcomp(dataRAW)
  pcaNORM = prcomp(dataNORM)
  pcaSG = prcomp(dataSG)
  pcaSGNORM = prcomp(dataSGNORM)

  # starting decision fusion process
  rfRAW = readRDS(modsList[grep("rfRAW",modsList)])
  rfNORM = readRDS(modsList[grep("rfNORM",modsList)])
  rfSG = readRDS(modsList[grep("rfSG_",modsList)])
  rfSGNORM = readRDS(modsList[grep("rfSGNORM",modsList)])
  results = list()
  plotID = list()

  if (!PROBS){
    for (i in 1:Nsamples){
      predRAW = predict(rfRAW[[4]],predict(pcaRAW,samples[i,])[,1:rfRAW[[2]]])
      predNORM = predict(rfNORM[[4]],predict(pcaNORM,sampleNORM[i,])[,1:rfNORM[[2]]])
      predSG = predict(rfSG[[4]],predict(pcaSG,sampleSG[i,])[,1:rfSG[[2]]])
      predSGNORM = predict(rfSGNORM[[4]],predict(pcaSGNORM,sampleSGNORM[i,])[,1:rfSGNORM[[2]]])
      classVec = levels(predRAW)
      fusion = as.vector(c(predNORM, predSG, predSGNORM, predRAW))
      fusionResult = table(fusion)

      if (length(names(fusionResult))==4){
        class = "NA"
        probs = "no agreement"
      }else if(length(names(fusionResult))==2 & as.numeric(fusionResult[1])==2){
        class1 = classVec[as.numeric(names(fusionResult))[1]]
        class2 = classVec[as.numeric(names(fusionResult))[2]]
        class = paste0(class1,"_",class2)
        probs = "tie"
      }else{
        hit = which(fusionResult == max(fusionResult))
        class = classVec[as.numeric(names(fusionResult))[hit]]
        counter = as.numeric(fusionResult[hit])
        if (counter==2){
          probs = "low agreement"
        }else if (counter==3){
          probs = "medium agreement"
        }else{
          probs = "high agreement"
        }
      }
      pred = data.frame(ID = ids[i], class = class, probability = probs)
      results[[i]] = pred

      # get data for similarity plots
      predRAW = predict(rfRAW[[4]],predict(pcaRAW,samples[i,])[,1:rfRAW[[2]]],type="prob")
      predNORM = predict(rfNORM[[4]],predict(pcaNORM,sampleNORM[i,])[,1:rfNORM[[2]]],type="prob")
      predSG = predict(rfSG[[4]],predict(pcaSG,sampleSG[i,])[,1:rfSG[[2]]],type="prob")
      predSGNORM = predict(rfSGNORM[[4]],predict(pcaSGNORM,sampleSGNORM[i,])[,1:rfSGNORM[[2]]],type="prob")
      fusion = (predNORM + predSG + predSGNORM + predRAW) / 4
      fusion = as.data.frame(fusion)
      hits = sort(fusion,decreasing = TRUE)[1:3]
      plotID[[i]] = hits
    }
    results = do.call("rbind",results)
    write.csv(results,file = paste0(root,"/",TYPE,"_results.csv"),row.names = FALSE)
    # function in action
    # first let's prepare the sample data
    data[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
    samples[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
    for (id in ids){
      sample = as.data.frame(t(samples[which(ids == id),]))
      sample$wavenmubers = wavenumbers
      names(sample)[1] = c("reflectance")
      #wn_res = prospectr::resample2(sample_raw$reflectance,sample_raw$wavenumbers,wavenumbers)
      #sample = data.frame(wavenumbers=wavenumbers,reflectance=wn_res)
      #class = plotID[[which(ids == id)]]
      if(results$probability[results$ID == id] == "no agreement"){
        class = plotID[[which(ids == id)]]
        annotation = paste0("No agreement\n",
                            names(class)[1],": ",class[1],"\n",
                            names(class)[2],": ",class[2],"\n",
                            names(class[3]),": ",class[3])
        class1 = samplePlot(data = data, sample = sample, class = names(class)[1],prob = annotation, name = id)
        class2 = samplePlot(data = data, sample = sample, class = names(class)[2])
        class3 = samplePlot(data = data, sample = sample, class = names(class)[3])
        multiclass = gridExtra::grid.arrange(class1,class2,class3)
        ggsave(plot=multiclass,file=paste0(plots,"/",id,"_probClasses.png"),dpi=300,device="png",units="cm",width=50,height=30)
      } else if (results$probability[results$ID == id] == "tie"){
        classes = results$class[results$ID == id]
        tie1 = stringr::str_split(classes,pattern="_")[[1]][1]
        tie2 = stringr::str_split(classes,pattern="_")[[1]][2]
        third = class = names(plotID[[which(ids == id)]])[3]
        class = plotID[[which(ids == id)]]
        annotation = annotation = paste0("Tie between: ",tie1," and ",tie2,"\n",
                                         names(class)[1],": ",class[1],"\n",
                                         names(class)[2],": ",class[2],"\n",
                                         names(class[3]),": ",class[3])
        class1 = samplePlot(data = data, sample = sample, class = tie1, prob = annotation, name = id)
        class2 = samplePlot(data = data, sample = sample, class = tie2)
        class3 = samplePlot(data = data, sample = sample, class = third)
        multiclass = gridExtra::grid.arrange(class1,class2,class3)
        ggsave(plot=multiclass,file=paste0(plots,"/",id,"_probClasses.png"),dpi=300,device="png",units="cm",width=50,height=30)
      }else{
        class = as.character(results$class[results$ID==id])
        addClasses = as.character(names(plotID[[which(ids == id)]][2:3]))
        class1 = samplePlot(data = data, sample = sample, class = class, prob = probs, name = id)
        class2 = samplePlot(data = data, sample = sample, class = addClasses[1])
        class3 = samplePlot(data = data, sample = sample, class = addClasses[2])
        multiclass = gridExtra::grid.arrange(class1,class2,class3)
        ggsave(plot=multiclass,file=paste0(plots,"/",id,"_probClasses.png"),dpi=300,device="png",units="cm",width=50,height=30)
      }
    }
  }
  if (PROBS){
    for (i in 1:Nsamples){
      predRAW = predict(rfRAW[[4]],predict(pcaRAW,samples[i,])[,1:rfRAW[[2]]],type="prob")
      predNORM = predict(rfNORM[[4]],predict(pcaNORM,sampleNORM[i,])[,1:rfNORM[[2]]],type="prob")
      predSG = predict(rfSG[[4]],predict(pcaSG,sampleSG[i,])[,1:rfSG[[2]]],type="prob")
      predSGNORM = predict(rfSGNORM[[4]],predict(pcaSGNORM,sampleSGNORM[i,])[,1:rfSGNORM[[2]]],type="prob")
      fusion = (predRAW + predNORM + predSG + predSGNORM) / 4
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
    write.csv(results,file = paste0(root,"/",TYPE,"_results.csv"),row.names = FALSE)

    # function in action
    # first let's prepare the sample data
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
      ggsave(plot=multiclass,file=paste0(plots,"/",id,"_probClasses.png"),dpi=300,device="png",units="cm",width=40,height=20)
    }

    for (i in 1:length(sampleList)){
      file.copy(sampleList[i],paste0(raw,"/",ids[i]))
      #file.remove(sampleList[i])
    }
  }
}



if (TYPE == "SG"){
  # prepare data
  sampleSG = as.data.frame(prospectr::savitzkyGolay(samples, p = 3, w = 11, m = 0))
  sampleSG[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0

  dataSG = as.data.frame(prospectr::savitzkyGolay(data[,1:length(wavenumbers)], p = 3, w = 11, m = 0))
  dataSG[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0

  pcaSG = prcomp(dataSG)
  rfSG = readRDS(modsList[grep("rfSG_",modsList)])

  results = list()
  plotID = list()
  for (i in 1:Nsamples){
    predSG = predict(rfSG[[4]],predict(pcaSG,sampleSG[i,])[,1:rfSG[[2]]],type="prob")
    probs = as.data.frame(predSG)
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
  write.csv(results,file = paste0(root,"/",TYPE,"_results.csv"),row.names = FALSE)

  # function in action
  # first let's prepare the sample data
  data[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  samples[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
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

  sampleSGNORM[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  dataSGNORM[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0

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
  write.csv(results,file = paste0(root,"/",TYPE,"_results.csv"),row.names = FALSE)

  # function in action
  # first let's prepare the sample data
  data[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  samples[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
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
  sampleNORM[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0

  dataNORM = as.data.frame(base::scale(data[,1:length(wavenumbers)]))
  dataNORM[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0

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
  write.csv(results,file = paste0(root,"/",TYPE,"_results.csv"),row.names = FALSE)

  # function in action
  # first let's prepare the sample data
  data[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  samples[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
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
  sampleRAW = samples
  sampleRAW[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0

  dataRAW = as.matrix(data[,-ncol(data)])
  dataRAW[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0

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
  write.csv(results,file = paste0(root,"/",TYPE,"_results.csv"),row.names = FALSE)

  # function in action
  # first let's prepare the sample data
  data[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
  samples[which(wavenumbers<=2420 & wavenumbers>=1900)] = 0
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


