# set root folder
if(Sys.info()["sysname"] == "Windows"){
  root_folder = "~/polymer/polymeRID"# adopt root folder to your personal dir sructure
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
           "prospectr",
           "tidyr",
           "stringr",
           "factoextra",
           "randomForest",
           "gridExtra",
           "hyperSpec",
           "workflowr")
for (lib in libs){loadandinstall(lib)}

# creating mandatory directories
root= path.expand(root_folder)
subDir= c("analysis", # directory containing rmd files which build docu website
          "code", #directory containing scripts and functions not
          "docs", #directory where htmls will be build
          "smp", #directory containing sample spectra files
          "ref", #directory containing reference spectra files
          "mod", #directory containing pre-trained and calibrated models
          "docs", #directory for documentation files
          "run", #directory for files created in runtime
          "output") # directory for additional outputs

for (dir in subDir){if(!dir.exists(file.path(root, dir))){dir.create(file.path(root, dir))}}

# create path variables
for (dir in subDir){assign(dir,paste(root,"/", paste(dir,"/",sep=""),sep=""))}
