# function for plotting

meanplot = function(data, class, wavenumbers = NULL){
  #prpare data
  wvn =  as.numeric(stringr::str_remove(names(data), "wvn")[-ncol(data)])
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
    annotate(geom="text",label=paste0("class: ",class,"\nsamples: ",cldata$N[1]),x=500,y=max(cldata$mean))+
    ylab(label="reflectance")+
    theme_minimal()
  return(tmp)}



samplePlot = function(data,sample,class,probs="",name=""){
  cldata = data[data$class == class,]
  MIN = Rfast::colMins(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
  MAX = Rfast::colMaxs(as.matrix(cldata[,1:ncol(cldata)-1]),value=TRUE)
  cldata$id = 1:length(cldata$class)
  cldata = melt(cldata,id.vars = c("id","class"))
  cldata = Rmisc::summarySE(cldata,measurevar="value",groupvars = "variable")
  names(cldata)[3] ="mean"
  cldata$min = MIN
  cldata$max = MAX
  #if (probs<0.5) prop = "no confidence"
  #if (probs>=0.5 & probs<0.6) prop = "very low confidence"
  #if (probs>=0.6 & probs<0.7) prop = "low confidence"
  #if (probs>=0.7 & probs<0.8) prop = "medium confidence"
  #if (probs>=0.8 & probs<0.9) prop = "high confidence"
  #if (probs>=0.9) prop = "very high confidence"
  figure = ggplot(data=cldata,aes(x=wavenumbers))+
    geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),fill="lightgrey",alpha=0.8)+
    geom_line(aes(y=mean),alpha=0.4)+
    geom_line(aes(y=max),linetype="dotted")+
    geom_line(aes(y=min),linetype="dotted")+
    geom_line(data=sample,aes(y=reflectance),color="red")+
    annotate(geom="text",label=paste0("Class: ",class,
                                      "\nSamples: ",cldata$N[1],
                                      #"\nProbability: ",round(probs,3),
                                      #"\nConfidence: ",
                                      "\n",probs),x=3500,y=Inf,hjust=1,vjust=1)+
    annotate(geom="text",label=name,x=1000,y=Inf,hjust=1,vjust=1)+
    ylab(label="reflectance")+
    theme_minimal()
  return(figure)
}
