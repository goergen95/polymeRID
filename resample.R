# data reconnasaince
source("polymeRID/setup.R")


raw = read.csv(paste0(ref,"216_2018_1156_MOESM2_ESM.csv"),header=F)
wavelengths = as.numeric(raw[1,2:1864])
saveRDS(wavelengths,paste0(run,"wavelengths.rds"))

sample = read.csv(paste0(smp,"18122018_PP_1.txt"),header=F,sep="")
names(sample) = c("wavelength","reflectance")
wv = sample$wavelength
index = which(wv>wavelengths[1] | wv<wavelengths[length(wavelengths)])
sample = sample[-index,]
new_sample = prospectr::resample2(sample$reflectance,sample$wavelength,wavelengths)
new_sample = data.frame(wavelength=wavelengths,reflectance=new_sample)
new_sample$ref = as.numeric(raw[243,2:1864])

ggplot(data=new_sample,aes(x=wavelength))+
  geom_line(aes(y=reflectance),col="blue")+
  geom_line(aes(y=ref),col="red")
