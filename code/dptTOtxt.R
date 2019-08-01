files = list.files("~/Downloads/W1/",pattern=".dpt",full.names = T)
file.copy(files,paste(str_sub(files,0,-5),".txt",sep=""))
files = list.files("~/Downloads/W1/",pattern=".txt",full.names = T)
file.copy(files,paste0(smp))
