#create base plots for every polymer in database
source("code/setup.R")

classes = readLines(paste0(ref,"classes.txt"))
data = lapply(classes,function(x){
  specs = read.csv(list.files(ref,full.names=T)[grep(paste(x,".csv",sep=""),list.files(ref))],header=T)
  return(specs)
})
data = do.call("rbind",data)
wavenumbers = readRDS(paste0(ref,"wavenumbers.rds"))
TIME = format(Sys.time(),"%Y%m%d_%H%M")

for (i in 1:length(classes)){
cldata = data[data$class == classes[i],]
MIN = Rfast::colMins(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
MAX = Rfast::colMaxs(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
cldata$id = 1:length(cldata$class)
cldata = melt(cldata,id.vars = c("id","class"))
cldata = Rmisc::summarySE(cldata,measurevar="value",groupvars = "variable")
names(cldata)[3] ="mean"
cldata$min = MIN
cldata$max = MAX
meanplot = ggplot(data=cldata,aes(x=wavenumbers))+
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),fill="lightgrey",alpha=0.8)+
  geom_line(aes(y=mean),alpha=0.4)+
  geom_line(aes(y=max),linetype="dotted")+
  geom_line(aes(y=min),linetype="dotted")+
  annotate(geom="text",label=paste0("class: ",classes[i],"\nsamples: ",cldata$N[1]),x=0,y=max(cldata$mean))+
  ylab(label="reflectance")+
  theme_minimal()
ggsave(plot=meanplot,file=paste0(output,"mean_spectrum_",classes[i],".png"),dpi=600,device="png",units="cm",width=40,height=20)
}


# function in action
# first let's prepare the sample data
sample_raw = read.table(paste0(smp,"II_r_a1_1.txt"))
names(sample_raw) = c("wavenumbers","reflectance")
wn_res = prospectr::resample2(sample_raw$reflectance,sample_raw$wavenumbers,wavenumbers)
sample = data.frame(wavenumbers=wavenumbers,reflectance=wn_res)
# source the sample plotting script
source("code/functions.R")
fig1 = samplePlot(data,sample,"PES")
fig2 = samplePlot(data,sample,"PE")
dir.create(paste0(smp,TIME))
ggsave(plot=fig1,file=paste0(smp,TIME,"/",TIME,"_PES.png"),dpi=600,device="png",width=30,height=15)
ggsave(plot=fig2,file=paste0(smp,TIME,"/",TIME,"_PE.png"),dpi=600,device="png",width=30,height=15)
