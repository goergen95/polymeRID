#script to create a common data base from our own reference polymers
#and the data by Primpke et al. (2018) (https://link.springer.com/article/10.1007%2Fs00216-018-1156-x)
source("code/setup.R")

#rawPrimpke = read.csv(paste0(run,"216_2018_1156_MOESM2_ESM.csv"),header=F)
#wavenumbers = as.numeric(rawPrimpke[1,2:1864])
#saveRDS(wavenumbers,paste0(ref,"wavenumbers.rds"))
#rawPrimpke = read.csv(paste0(run,"216_2018_1156_MOESM2_ESM.csv"),header=T)
#rawPrimpke = rawPrimpke[which(rawPrimpke$Abbreviation %in%  c("PP","PE","PS","PA","HDPE","LDPE","PC","PES","PET","PVC","Nylon")),]
#abb = droplevels(rawPrimpke$Abbreviation)
#rawPrimpke = rawPrimpke[,c(2:1864)]
#names(rawPrimpke) = paste("wvn",wavenumbers,sep="")
#rawPrimpke$class = as.character(abb)

refFiles = list.files(run,full.names=TRUE,pattern=".txt")
resampSpectra = function(x){
  data = read.csv(x,header=F,sep="")
  names(data) = c("wavenumber","reflectance")
  wvn = data$wavenumber
  #tmp = prospectr::resample2(data$reflectance,data$wavenumber,resVec)
  data = as.data.frame(t(data$reflectance))
  names(data) = paste("wvn",wvn,sep="")
  filename = stringr::str_split(x,"/")[[1]][length(stringr::str_split(x,"/")[[1]])]
  className = stringr::str_split(filename,"_")[[1]][2]
  data$class = className
  return(data)
}
refData = lapply(refFiles,resampSpectra)
refData = do.call("rbind",refData)
#refData$class[grep("Nylon",refData$class)] = "PA"
#rawPrimpke$class[grep("Nylon",rawPrimpke$class)] = "PA"
#rawPrimpke$class[grep("HDPE",rawPrimpke$class)] = "PE"
#rawPrimpke$class[grep("LDPE",rawPrimpke$class)] = "PE"
# join data tables
#data = rbind(rawPrimpke,refData)
dummy = as.matrix(refData[,1:ncol(refData)-1])
baslineDummy = baseline(dummy,method="rfbaseline",span=NULL,NoXP=64,maxit=c(10))
spectra = getCorrected(baslineDummy)
data = as.data.frame(spectra)
data$class = as.factor(refData$class)
summary(data$class)
wavenumbers = as.numeric(stringr::str_remove(names(data),"wvn")[-ncol(data)])
write.csv(data,file = paste0(ref,"reference_database.csv"),row.names=FALSE)

classIndex = as.character(unique(data$class))

for (class in classIndex){
  tmp = data[data$class==class,]
  write.csv(tmp, file = paste0(ref,"reference_",class,".csv"),row.names=FALSE)
}

write(classIndex,paste0(ref,"classes.txt"))
saveRDS(wavenumbers,paste0(ref,"wavenumbers.rds"))
