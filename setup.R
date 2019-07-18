# set root folder
if(Sys.info()["sysname"] == "Windows"){
  root_folder = "~/polymer/polymeRID"
} else {
  root_folder = "~/polymer/polymeRID"
}

# loading necessary libraries
loadandinstall = function(mypkg) {
  if (!is.element(mypkg, installed.packages()[,1]))
    install.packages(mypkg)
  library(mypkg, character.only = TRUE)}
libs <-  c("dplyr", 
           "reshape2", 
           "ggplot2",
           "magrittr",
           "caret",
           "keras",
           "prospectr")
for (lib in libs){loadandinstall(lib)}

# creating mandatory directories
root= path.expand(root_folder)
subDir= c("fun", #directory containing scripts for functions
          "smp", #directory containing sample spectra files
          "ref", #directory containing reference spectra files
          "mod", #directory containing pre-trained and calibrated models
          "doc", #directory for documentation files
          "run") #directory for files created in runtime
for (dir in subDir){if(!dir.exists(file.path(root, dir))){dir.create(file.path(root, dir))}}

# create path variables
for (dir in subDir){assign(dir,paste(root,"/", paste(dir,"/",sep=""),sep=""))}
