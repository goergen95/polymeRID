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
N = length(cldata$class)
cldata$id = 1:length(cldata$class)
cldata = melt(cldata,id.vars = c("id","class"))
cldata = Rmisc::summarySE(cldata,measurevar="value",groupvars = "variable")
names(cldata)[3] ="reflectance"
meanplot = ggplot(data=cldata,aes(x=wavenumbers))+
  geom_ribbon(aes(ymin=reflectance-sd,ymax=reflectance+sd),fill="lightgrey",alpha=0.8)+
  geom_line(aes(y=reflectance),linetype="dotted")+
  annotate(geom="text",label=paste0("Class: ",classes[i],"\nSamples: ",N),x=3500,y=0.4)+
  theme_minimal()
ggsave(plot=meanplot,file=paste0(output,"mean_spectrum_",classes[i],".png"),dpi=600,device="png",units="cm",width=40,height=20)
}
