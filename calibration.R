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
#TYPE = "SG"
#TYPE = "SGNORM"
#TYPE = "NORM"
category = "class"
window = c(3800,400) # window of wavnumbers included in training
TIME = format(Sys.time(), "%Y%m%d_%H%M")
dir.create(paste0(mod, TIME))

                    # Don't change anything below this line #
#==============================================================================#

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

# extracting wavenumbers from reference database
waveChar = stringr::str_remove(names(data[!names(data) %in% category]),
                               pattern = "wvn")
wavenumbers = as.numeric(waveChar)
index = which(wavenumbers<=window[1] & wavenumbers>=window[2])
wavenumbers = wavenumbers[index]
data = data[,c(index,which(names(data) %in% category))]
# save wavenumbers for classification purposes
saveRDS(wavenumbers,paste0(mod,TIME,"/wavenumbers_",TIME,".rds"))


if (TYPE == "FUSION"){
  data.norm = as.data.frame(base::scale(data[,-which(names(data)==category)]))
  data.norm[category] = data[category]
  data.sg.norm = as.data.frame(prospectr::savitzkyGolay(data.norm[,-which(names(data.norm)==category)], p = 3, w = 11, m = 0))
  data.sg.norm[category] = data[category]
  data.sg = as.data.frame(prospectr::savitzkyGolay(data[,-which(names(data)==category)], p = 3, w = 11, m = 0))
  data.sg[category] = data[category]

  index = which(wavenumbers<=2420 & wavenumbers>=1900)
  data[,index] = 0
  data.norm[,index] = 0
  data.norm[is.na(data.norm)] = 0
  data.sg[,index] = 0
  data.sg.norm[is.na(data.sg.norm)] = 0
  data.sg.norm[,index] = 0

  # apply PCA-CV to the individual data-frames
  rfRAW = pcaCV(data,folds=10,repeats = 5,threshold = 99,metric = "Kappa",method = "rf")
  saveRDS(rfRAW,file = paste0(mod,TIME,"/rfRAW_",TIME,".rds"))
  rfNORM = pcaCV(data.norm,folds=10,repeats = 5,threshold = 99,metric = "Kappa",method = "rf")
  saveRDS(rfNORM,file = paste0(mod,TIME,"/rfNORM_",TIME,".rds"))
  rfSGNORM = pcaCV(data.sg.norm,folds=10,repeats = 5,threshold = 99,metric = "Kappa",method = "rf")
  saveRDS(rfSGNORM,file = paste0(mod,TIME,"/rfSGNORM_",TIME,".rds"))
  rfSG = pcaCV(data.sg,folds=10,repeats = 5,threshold = 99,metric = "Kappa",method = "rf")
  saveRDS(rfSG,file = paste0(mod,TIME,"/rfSG_",TIME,".rds"))
  accuracy = unlist(c(rfNORM[[1]],rfSGNORM[[1]],rfSG[[1]]),rfRAW[[1]])
  cat(paste0("The mean kappa score for the decision fusion models is ",round(mean(accuracy),3)))
}


if (TYPE == "RAW"){
  rfRAW = pcaCV(data,folds=10,repeats = 4,threshold = 99,metric = "Kappa",method = "rf")
  saveRDS(rfRAW,file = paste0(mod,TIME,"/rfRAW_",TIME,".rds"))
  cat(paste0("The cross-validated kappa score for the RAW model is ",round(mean(unlist(rfSG[[1]])),3)))
}


if (TYPE == "SG"){
  data.sg = as.data.frame(prospectr::savitzkyGolay(data[,-which(names(data)==category)], p = 3, w = 11, m = 0))
  data.sg[category] = data[category]
  rfSG = pcaCV(data.sg,folds=10,repeats = 4,threshold = 99,metric = "Kappa",method = "rf")
  saveRDS(rfSG,file = paste0(mod,TIME,"/rfSG_",TIME,".rds"))
  cat(paste0("The cross-validated kappa score for the SG model is ",round(mean(unlist(rfSG[[1]])),3)))
}

if (TYPE == "SGNORM"){
  data.sg.norm = as.data.frame(prospectr::savitzkyGolay(data.norm[,-which(names(data.norm)==category)], p = 3, w = 11, m = 0))
  data.sg.norm[category] = data.norm[category]
  rfSGNORM = pcaCV(data.sg.norm,folds=10,repeats = 5,threshold = 99,metric = "Kappa",method = "rf")
  saveRDS(rfSGNORM,file = paste0(mod,TIME,"/rfSGNORM_",TIME,".rds"))
  cat(paste0("The cross-validated kappa score for the SGNORM model is ",round(mean(unlist(rfSGNORM[[1]])),3)))
}

if (TYPE == "NORM"){
  data.norm = as.data.frame(base::scale(data[,-which(names(data)==category)]))
  data.norm[category] = data[category]
  rfNORM = pcaCV(data.norm,folds=10,repeats = 5,threshold = 99,metric = "Kappa",method = "rf")
  saveRDS(rfNORM,file = paste0(mod,TIME,"/rfNORM_",TIME,".rds"))
  cat(paste0("The cross-validated kappa score for the NORM model is ",round(mean(unlist(rfNORM[[1]])),3)))
}
