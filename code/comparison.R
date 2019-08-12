

o1obsv = read.csv("../O1_human_classification.csv",header = T)
o2obsv = read.csv("../O2_human_classification.csv",header = T)
o2obsv$name = as.character(o2obsv$name)
W1obsv = read.csv("../W1_human_classification.csv",header = T)
W1obsv$name = as.character(W1obsv$name)
w2obsv = read.csv("../W2_human_classification.csv",header = T)
w2obsv$name = as.character(w2obsv$name)
#o1pred = read.csv("~/Desktop/results/O1/O1_NNET_results.csv",header =T)
o2pred = read.csv("~/Desktop/results/O2/NNET_results.csvFALSE",header =T)
id1 = unlist(stringr::str_split(o2pred$id,"_"))[seq(3,147,4)]
id2 = unlist(stringr::str_split(o2pred$id,"_"))[seq(4,148,4)]
id2 = stringr::str_remove(id2,".txt")
id2
o2pred$id = paste(id1,id2,sep="_")

w2pred = read.csv("~/Desktop/results/W2/NNET_results.csvFALSE",header =T)
id1 = unlist(stringr::str_split(w2pred$id,"_"))[seq(3,56,4)]
id2 = unlist(stringr::str_split(w2pred$id,"_"))[seq(4,56,4)]
id2 = stringr::str_remove(id2,".txt")
w2pred$id = paste(id1,id2,sep="_")

w1pred = read.csv("~/Desktop/results/W1/NNET_results.csvFALSE",header =T)
id1 = unlist(stringr::str_split(w1pred$id,"_"))[seq(3,72,4)]
id2 = unlist(stringr::str_split(w1pred$id,"_"))[seq(4,72,4)]
id2 = stringr::str_remove(id2,".txt")
w1pred$id = paste(id1,id2,sep="_")
o2 = merge(o2obsv,o2pred,by.x="name",by.y="id")
w1 = merge(W1obsv,w1pred,by.x="name",by.y="id")
w2 = merge(w2obsv,w2pred,by.x="name",by.y="id")
w1$class1 = as.character(w1$class1)
w1$class1[w1$prop1<0.5] = "OTHER"
w2$class1 = as.character(w2$class1)
w2$class1[w2$prop1<0.5] = "OTHER"
o2$class1 = as.character(o2$class1)
o2$class1[o2$prop1<0.5] = "OTHER"


pred = c(as.character(o2$class1),as.character(w1$class1),as.character(w2$class1))
pred[which(pred == "FIBRE")] = "OTHER"
pred[which(pred == "WOOD")] = "OTHER"
pred[which(pred == "FUR")] = "OTHER"

obsv = c(as.character(o2$polymer),as.character(w1$polymer),as.character(w2$polymer))


pred[70] = "PS"
obsv[70] = "PS"
pred[71] = "PP"
obsv[71] = "PP"
pred[72] = "PES"
obsv[72] = "PES"

obsv = as.factor(obsv)
pred = as.factor(pred)

levels(pred)
levels(obsv)

caret::confusionMatrix(as.factor(pred),as.factor(obsv))



d =rbind(w1,w2,o2)
write.csv(d,file="~/Desktop/results.csv")
