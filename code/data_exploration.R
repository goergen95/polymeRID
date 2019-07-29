# data reconnasaince
source("polymeRID/setup.R")


raw = read.csv(paste0(run,"216_2018_1156_MOESM2_ESM.csv"),header=F)
raw = raw[which(raw$Abbreviation %in%  c("PP","PE","PS","PA","HDPE","LDPE","PC","PES","PET","PVC")),]
raw$Abbreviation = droplevels(raw$Abbreviation)
wavelengths = as.numeric(raw[1,2:1864])
#saveRDS(wavelengths,paste0(run,"wavelengths.rds"))

sample1 = read.csv(paste0(smp,"10012019_ABS_1.txt"),header=F,sep="")
sample2 = read.csv(paste0(smp,"10012019_PS_1.txt"),header=F,sep="")
sample3 = read.csv(paste0(smp,"17122018_PA_1.txt"),header=F,sep="")
sample4 = read.csv(paste0(smp,"18122018_PE_1.txt"),header=F,sep="")
sample5 = read.csv(paste0(smp,"18122018_PP_1.txt"),header=F,sep="")

names(sample1) = c("wavelength","reflectance")
names(sample2) = c("wavelength","reflectance")
names(sample3) = c("wavelength","reflectance")
names(sample4) = c("wavelength","reflectance")
names(sample5) = c("wavelength","reflectance")

wvSample = sample1$wavelength

length(wavelengths)
length(wvSample)

# resampling samples to database
samplesDF = data.frame(wavelengths=wavelengths,
                       sample1=rep(0,length(wavelengths)),
                       sample2=rep(0,length(wavelengths)),
                       sample3=rep(0,length(wavelengths)),
                       sample4=rep(0,length(wavelengths)),
                       sample5=rep(0,length(wavelengths)),
                       HDPE=rep(0,length(wavelengths)),
                       LDPE=rep(0,length(wavelengths)),
                       PA=rep(0,length(wavelengths)),
                       PC=rep(0,length(wavelengths)),
                       PE=rep(0,length(wavelengths)),
                       PES=rep(0,length(wavelengths)),
                       PET=rep(0,length(wavelengths)),
                       PP=rep(0,length(wavelengths)),
                       PS=rep(0,length(wavelengths)),
                       PVC=rep(0,length(wavelengths))
                       )
samplesDF$sample1 = prospectr::resample2(sample1$reflectance,sample1$wavelength,wavelengths)
samplesDF$sample2 = prospectr::resample2(sample2$reflectance,sample2$wavelength,wavelengths)
samplesDF$sample3 = prospectr::resample2(sample3$reflectance,sample3$wavelength,wavelengths)
samplesDF$sample4 = prospectr::resample2(sample4$reflectance,sample4$wavelength,wavelengths)
samplesDF$sample5 = prospectr::resample2(sample5$reflectance,sample5$wavelength,wavelengths)

samplesDF$HDPE = colMeans(raw[raw$Abbreviation=="HDPE",2:1864])
samplesDF$LDPE = colMeans(raw[raw$Abbreviation=="LDPE",2:1864])
samplesDF$PA = colMeans(raw[raw$Abbreviation=="PA",2:1864])
samplesDF$PC = colMeans(raw[raw$Abbreviation=="PC",2:1864])
samplesDF$PE = colMeans(raw[raw$Abbreviation=="PE",2:1864])
samplesDF$PES = colMeans(raw[raw$Abbreviation=="PES",2:1864])
samplesDF$PET = colMeans(raw[raw$Abbreviation=="PET",2:1864])
samplesDF$PP = colMeans(raw[raw$Abbreviation=="PP",2:1864])
samplesDF$PS = colMeans(raw[raw$Abbreviation=="PS",2:1864])
samplesDF$PVC = colMeans(raw[raw$Abbreviation=="PVC",2:1864])

summary(raw$Abbreviation)

ggplot(data=samplesDF,aes(x=wavelengths))+
  geom_line(aes(y=PS),col="blue")+
  geom_line(aes(y=sample2),col="red")+
  ggtitle("mean PS vs. resampled PS sample")+
  ylab("reflactance")

ggplot(data=samplesDF,aes(x=wavelengths))+
  geom_line(aes(y=PA),col="blue")+
  geom_line(aes(y=sample3),col="red")+
  ggtitle("mean PA vs. resampled PA sample")+
  ylab("reflactance")

ggplot(data=samplesDF,aes(x=wavelengths))+
  geom_line(aes(y=PE),col="blue")+
  geom_line(aes(y=sample4),col="red")+
  ggtitle("mean PE vs. resampled PE sample")+
  ylab("reflactance")

ggplot(data=samplesDF,aes(x=wavelengths))+
  geom_line(aes(y=PP),col="blue")+
  geom_line(aes(y=sample5),col="red")+
  ggtitle("mean PP vs. resampled PP sample")+
  ylab("reflactance")

ggplot(data=samplesDF,aes(x=wavelengths))+
  geom_line(aes(y=HDPE),col="blue")+
  geom_line(aes(y=LDPE),col="red")
  ggtitle("mean HDPE and mean LDPW ")+
    ylab("reflactance")

ggplot(data=samplesDF,aes(x=wavelengths))+
  geom_line(aes(y=PC),col="blue")+
  ggtitle("mean PC ")+
  ylab("reflactance")

ggplot(data=samplesDF,aes(x=wavelengths))+
  geom_line(aes(y=PES),col="blue")+
  ggtitle("mean PES ")+
  ylab("reflactance")

ggplot(data=samplesDF,aes(x=wavelengths))+
  geom_line(aes(y=PET),col="blue")+
  ggtitle("mean PET ")+
  ylab("reflactance")

ggplot(data=samplesDF,aes(x=wavelengths))+
  geom_line(aes(y=PVC),col="blue")+
  ggtitle("mean PVC ")+
  ylab("reflactance")





