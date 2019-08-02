#script to create a common data base from our own reference polymers
#and the data by Primpke et al. (2018) (https://link.springer.com/article/10.1007%2Fs00216-018-1156-x)
source("code/setup.R")

rawPrimpke = read.csv(paste0(run,"216_2018_1156_MOESM2_ESM.csv"),header=F)
wavenumbers = as.numeric(rawPrimpke[1,2:1864])
# saveRDS(wavenumbers,paste0(ref,"wavenumbers.rds"))
rawPrimpke = read.csv(paste0(run,"216_2018_1156_MOESM2_ESM.csv"),header=T)


furs = rawPrimpke[c(grep("fur",rawPrimpke$Abbreviation) , grep("wool",rawPrimpke$Abbreviation)),]
furs = furs[,c(2:1864)]
names(furs) = paste("wvn",wavenumbers,sep="")
furs$class = "FUR"

# wool = rawPrimpke[grep("wool",rawPrimpke$Abbreviation),]
# wool = wool[,c(2:1864)]
# names(wool) = paste("wvn",wavenumbers,sep="")
# wool$class = "WOOL"

fibre = rawPrimpke[grep("fibre",rawPrimpke$Abbreviation),]
fibre = fibre[,c(2:1864)]
names(fibre) = paste("wvn",wavenumbers,sep="")
fibre$class = "FIBRE"

wood = rawPrimpke[grep("wood",rawPrimpke$Abbreviation),]
wood = wood[,c(2:1864)]
names(wood) = paste("wvn",wavenumbers,sep="")
wood$class = "WOOD"

natural = rawPrimpke[rawPrimpke$Natural..Synthetic =="natural substance",]
natural = natural[,c(2:1864)]
names(natural) = paste("wvn",wavenumbers,sep="")
natural$class = "NATURAL"

syntPolymer = rawPrimpke[rawPrimpke$Natural..Synthetic =="synthetic polymer",]
syntPolymer = syntPolymer[which(syntPolymer$Abbreviation %in%  c("PP","PE","PS","PA","HDPE","LDPE","PC","PES","PET","PVC","Nylon")),]
abb = droplevels(syntPolymer$Abbreviation)
syntPolymer = syntPolymer[,c(2:1864)]
names(syntPolymer) = paste("wvn",wavenumbers,sep="")
syntPolymer$class = as.character(abb)

refFiles = list.files(run,full.names=TRUE,pattern=".txt")
resampSpectra = function(x,resVec){
  data = read.csv(x,header=F,sep="")
  names(data) = c("wavenumber","reflectance")
  #wvn = data$wavenumber
  tmp = prospectr::resample2(data$reflectance,data$wavenumber,resVec)
  data = as.data.frame(t(tmp))
  #data = as.data.frame(t(data$reflectance))
  names(data) = paste("wvn",resVec,sep="")
  filename = stringr::str_split(x,"/")[[1]][length(stringr::str_split(x,"/")[[1]])]
  className = stringr::str_split(filename,"_")[[1]][2]
  data$class = className
  return(data)
}

refData = lapply(refFiles,resampSpectra,resVec=wavenumbers)
refData = do.call("rbind",refData)
refData$class[grep("Nylon",refData$class)] = "PA"
syntPolymer$class[grep("Nylon",syntPolymer$class)] = "PA"
syntPolymer$class[grep("HDPE",syntPolymer$class)] = "PE"
syntPolymer$class[grep("LDPE",syntPolymer$class)] = "PE"

# baseline correction for our sample data
dummy = as.matrix(refData[,1:ncol(refData)-1])
baslineDummy = baseline(dummy,method="rfbaseline",span=NULL,NoXP=64,maxit=c(10))
spectra = getCorrected(baslineDummy)
corr = as.data.frame(spectra)
corr$class = as.factor(refData$class)

data = rbind(furs,wood,fibre,natural,syntPolymer,corr)
data$class = as.factor(data$class)
wavenumbers = as.numeric(stringr::str_remove(names(data),"wvn")[-ncol(data)])
write.csv(data,file = paste0(ref,"reference_database.csv"),row.names=FALSE)

classIndex = as.character(unique(data$class))

for (class in classIndex){
  tmp = data[data$class==class,]
  write.csv(tmp, file = paste0(ref,"reference_",class,".csv"),row.names=FALSE)
}

write(classIndex,paste0(ref,"classes.txt"))
saveRDS(wavenumbers,paste0(ref,"wavenumbers.rds"))
