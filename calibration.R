# this script is used to calibrate a training model to the data present in the ref directory
# the data is supposed to be provided in the csv file format
# all measurements of different individuals of  reference polymers are found in the same csv file.
# the csv file should be named with an string lateral uniquley identifying the polymer class
# the same string should be put into the classes.txt as only the classes present in this file will be trained.
# the csv files are supposed to be in wide format, the first row containing numerics for the wavenumbers
# the second row containing the measured reflectance values. "," should be used as cell delimieters, "." as decimal limiter
source("code/setup.R")
source("code/functions.R")

TYPE = "FUSION"
# alternatives would be
#TYPE = "SGD2"
#TYPE = "SGNORM"
#TYPE = "NORM"
category = "class"
TIME = format(Sys.time(),"%Y%m%d_%H%M")
dir.create(paste0(mod,TIME))



classes = readLines(paste0(ref,"classes.txt"))
data = lapply(classes,function(x){
  specs = read.csv(list.files(ref,full.names=T)[grep(paste(x,".csv",sep=""),list.files(ref))],header=T)
  return(specs)
})
data = do.call("rbind",data)
window = c(3800,400)
wavenumbers = as.numeric(stringr::str_remove(names(data),pattern = "wvn"))[1:ncol(data)-1]
index = which(wavenumbers<=window[1] & wavenumbers>=window[2])
wavenumbers = wavenumbers[index]
saveRDS(wavenumbers,paste0(mod,TIME,"/wavenumbers_",TIME,".rds"))
data = data[,c(index,ncol(data))]


if (TYPE == "FUSION"){
  data.norm = as.data.frame(base::scale(data[,-which(names(data)==category)]))
  data.norm[category] = data[category]
  data.sg.norm = as.data.frame(prospectr::savitzkyGolay(data.norm[,-which(names(data.norm)==category)], p = 3, w = 11, m = 0))
  data.sg.norm[category] = data[category]
  data.sg.d2 = as.data.frame(prospectr::savitzkyGolay(data[,-which(names(data)==category)], p = 3, w = 11, m = 2))
  data.sg.d2[category] = data[category]

  index = which(wavenumbers<=2420 & wavenumbers>=2200)
  data[,index] = 0
  data.norm[,index] = 0
  data.norm[is.na(data.norm)] = 0
  data.sg.d2[,index] = 0
  data.sg.norm[is.na(data.sg.norm)] = 0
  data.sg.norm[,index] = 0

  # apply PCA-CV to the individual data-frames
  rfRAW = pcaCV(data,folds=10,repeats = 5,threshold = 99,metric = "Kappa")
  saveRDS(rfRAW,file = paste0(mod,TIME,"/rfRAW_",TIME,".rds"))
  rfNORM = pcaCV(data.norm,folds=10,repeats = 5,threshold = 99,metric = "Kappa")
  saveRDS(rfNORM,file = paste0(mod,TIME,"/rfNORM_",TIME,".rds"))
  rfSGNORM = pcaCV(data.sg.norm,folds=10,repeats = 5,threshold = 99,metric = "Kappa")
  saveRDS(rfSGNORM,file = paste0(mod,TIME,"/rfSGNORM_",TIME,".rds"))
  rfSGD2 = pcaCV(data.sg.d2,folds=10,repeats = 4,threshold = 99,metric = "Kappa")
  saveRDS(rfSGD2,file = paste0(mod,TIME,"/rfSGD2_",TIME,".rds"))
  accuracy = unlist(c(rfNORM[[1]],rfSGNORM[[1]],rfSGD2[[1]]),rfRAW[[1]])
  cat(paste0("The mean kappa score for the decision fusion models is ",round(mean(accuracy),3)))
}


if (TYPE == "RAW"){
  rfRAW = pcaCV(data,folds=10,repeats = 4,threshold = 99,metric = "Kappa")
  saveRDS(rfRAW,file = paste0(mod,TIME,"/rfRAW_",TIME,".rds"))
  cat(paste0("The cross-validated kappa score for the RAW model is ",round(mean(unlist(rfSGD2[[1]])),3)))
}


if (TYPE == "SGD2"){
  data.sg.d2 = as.data.frame(prospectr::savitzkyGolay(data[,-which(names(data)==category)], p = 3, w = 11, m = 2))
  data.sg.d2[category] = data[category]
  rfSGD2 = pcaCV(data.sg.d2,folds=10,repeats = 4,threshold = 99,metric = "Kappa")
  saveRDS(rfSGD2,file = paste0(mod,TIME,"/rfSGD2_",TIME,".rds"))
  cat(paste0("The cross-validated kappa score for the SGD2 model is ",round(mean(unlist(rfSGD2[[1]])),3)))
}

if (TYPE == "SGNORM"){
  data.sg.norm = as.data.frame(prospectr::savitzkyGolay(data.norm[,-which(names(data.norm)==category)], p = 3, w = 11, m = 0))
  data.sg.norm[category] = data.norm[category]
  rfSGNORM = pcaCV(data.sg.norm,folds=10,repeats = 5,threshold = 99,metric = "Kappa")
  saveRDS(rfSGNORM,file = paste0(mod,TIME,"/rfSGNORM_",TIME,".rds"))
  cat(paste0("The cross-validated kappa score for the SGNORM model is ",round(mean(unlist(rfSGNORM[[1]])),3)))
}

if (TYPE == "NORM"){
  data.norm = as.data.frame(base::scale(data[,-which(names(data)==category)]))
  data.norm[category] = data[category]
  rfNORM = pcaCV(data.norm,folds=10,repeats = 5,threshold = 99,metric = "Kappa")
  saveRDS(rfNORM,file = paste0(mod,TIME,"/rfNORM_",TIME,".rds"))
  cat(paste0("The cross-validated kappa score for the NORM model is ",round(mean(unlist(rfNORM[[1]])),3)))
}
