# comparisons of data
source("code/setup.R")

# function for plotting
meanplot = function(data,wavenumbers,class){
  #prpare data
  cldata = data[data$class == class,]
  MIN = Rfast::colMins(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
  MAX = Rfast::colMaxs(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
  cldata$id = 1:length(cldata$class)
  cldata = reshape2::melt(cldata,id.vars = c("id","class"))
  cldata = Rmisc::summarySE(cldata,measurevar="value",groupvars = "variable")
  names(cldata)[3] ="mean"
  cldata$min = MIN
  cldata$max = MAX


  tmp = ggplot2::ggplot(data=cldata,aes(x=wavenumbers))+
    geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),fill="lightgrey",alpha=0.8)+
    geom_line(aes(y=mean),alpha=0.4)+
    geom_line(aes(y=max),linetype="dotted")+
    geom_line(aes(y=min),linetype="dotted")+
    annotate(geom="text",label=paste0("class: ",class,"\nsamples: ",cldata$N[1]),x=0,y=max(cldata$mean))+
    ylab(label="reflectance")+
    theme_minimal()
  return(tmp)}
#########################################################################################################

rawPrimpke = read.csv(paste0(run,"216_2018_1156_MOESM2_ESM.csv"),header=F)
wavenumbers = as.numeric(rawPrimpke[1,2:1864])
# saveRDS(wavenumbers,paste0(ref,"wavenumbers.rds"))
rawPrimpke = read.csv(paste0(run,"216_2018_1156_MOESM2_ESM.csv"),header=T)
rawPrimpke = rawPrimpke[which(rawPrimpke$Abbreviation %in%  c("PP","PE","PS","PA","HDPE","LDPE","PC","PES","PET","PVC","Nylon")),]
abb = droplevels(rawPrimpke$Abbreviation)
rawPrimpke = rawPrimpke[,c(2:1864)]
names(rawPrimpke) = paste("wvn",wavenumbers,sep="")
rawPrimpke$class = as.character(abb)

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
refData$class[grep("Nylon",refData$class)] = "PA"
rawPrimpke$class[grep("Nylon",rawPrimpke$class)] = "PA"
rawPrimpke$class[grep("HDPE",rawPrimpke$class)] = "PE"
rawPrimpke$class[grep("LDPE",rawPrimpke$class)] = "PE"


dummy = as.matrix(refData[,1:ncol(refData)-1])
baselineDummy = baseline(dummy,method="rfbaseline",span=NULL,NoXP=64,maxit=c(10))
baselineDummy = baseline(dummy,method="peakDetection",left=500,right=500,lwin=100,rwin=100)
baselineDummy = baseline(dummy,method="rollingBall",wm=1000,ws=1000)
spectra = getCorrected(baselineDummy)
data = as.data.frame(spectra)
data$class = as.factor(refData$class)
wvP = wavenumbers
wvR = as.numeric(str_remove(names(refData),"wvn")[-length(names(refData))])


meanplot(rawPrimpke,wvP,"PE")
meanplot(data,wvR,"PE")
meanplot(rawPrimpke,wvP,"PA")
meanplot(data,wvR,"PA")
meanplot(rawPrimpke,wvP,"PET")
meanplot(data,wvR,"PET")
meanplot(rawPrimpke,wvP,"PP")
meanplot(data,wvR,"PP")
meanplot(rawPrimpke,wvP,"PS")
meanplot(data,wvR,"PS")
plot(rev(as.numeric(data[which(data$class=="PS")[2],-2527])),type="l")




PRlibrary = new("hyperSpec",spc=as.matrix(rawPrimpke[,1:1863]),wavelength=wvP,labels=rawPrimpke$class)
JPlibrary = new("hyperSpec",spc=as.matrix(refData[,1:1863]),wavelength=wvP,labels=refData$class)


baseline = spc.rubberband(JPlibrary)
#plot(baseline)
#JPcorr = JPlibrary - baseline
plot(JPlibrary,col ="lightgrey")
plot(baseline,col="red",add=TRUE)


