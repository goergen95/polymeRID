# test run plots
source("code/setup.R")
library(plotly)

results = readRDS(paste0(output,"testRun/first_testRun.rds"))

# let's visualize the accuracy results with noise levels on x-axis and kappa score on y-axis
test = ggplot(data=results)+
  geom_line(aes(y=kappa,group=type,x=level,color=type),size=1.5)+
  ylab("Kappa score")+
  xlab("Noise level")+
  scale_color_discrete(name="Type of\nPre-Processing")+
  theme_minimal()

# now we find out the three "optimal" preprocessing types by looking at the slope of the accuracy functions
results_LM = spread(results,key=level,value=kappa)
results_LM$slope = 0
x = 1:4
for ( i in 1:nrow(results_LM)){
results_LM$slope[i] = sqrt(lm(as.numeric(results_LM[i,2:5])~x)$coefficients[[2]]**2)
}

results_LM[order(results_LM$slope),]
#sg.d2
#sg.norm
#norm
