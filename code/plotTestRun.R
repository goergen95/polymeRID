# test run plots
source("code/setup.R")
library(plotly)

results = readRDS(paste0(output,"natural/natural_testRun.rds"))
results$level = as.character(results$level)
results$level[1:12] = "noise 0"
results$level[13:24] = "noise 1"
results$level[25:36] = "noise 2"
results$level[37:48] = "noise 3"
results$level[49:60] = "noise 4"
results$level[61:72] = "noise 5"
results$level = as.factor(results$level)
# let's visualize the accuracy results with noise levels on x-axis and kappa score on y-axis
test = ggplot(data=results)+
  geom_line(aes(y=kappa,group=type,x=level,color=type),size=1.5)+
  ylab("Kappa score")+
  xlab("Noise level")+
  scale_color_discrete(name="Type of\nPre-Processing")+
  theme_minimal()

t = ggplotly(test)


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
