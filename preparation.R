# script to create a common data base from our own reference polymers
# and the data by Primpke et al. (2018) which can be found here:
# (https://link.springer.com/article/10.1007%2Fs00216-018-1156-x)
source("code/setup.R")

url = "https://static-content.springer.com/esm/art%3A10.1007%2Fs00216-018-1156-x/MediaObjects/216_2018_1156_MOESM2_ESM.xlsx"

rawPrimpke = openxlsx::read.xlsx(url)
# extract wavenumbers from first row
wavenumbers = as.numeric(names(data)[2:1864])
# saving wavenumbers to reference sample directory
saveRDS(wavenumbers, paste0(ref, "wavenumbers.rds"))

#==============================================================================#
            ### Working on the database of Primpke et al. (2018) ###
# let's group together broad classes of natural materials
# furs and wools
indexFur = grep("fur", rawPrimpke$Abbreviation)
indexWool = grep("wool", rawPrimpke)
furs = rawPrimpke[c(indexFur, indexWool), ]
furs = furs[ , c(2:1864)] # leave out index column
names(furs) = paste("wvn", wavenumbers, sep="")
furs$class = "FUR"
rm(indexFur, indexWool)

# fibres
indexFibre = grep("fibre", rawPrimpke$Abbreviation)
fibre = rawPrimpke[indexFibre, ]
fibre = fibre[ , c(2:1864)] # leave out index column
names(fibre) = paste("wvn", wavenumbers, sep="")
fibre$class = "FIBRE"
rm(indexFibre)

# wood
indexWood = grep("wood", rawPrimpke$Abbreviation)
wood = rawPrimpke[indexWood, ]
wood = wood[ , c(2:1864)] # leave out index colums
names(wood) = paste("wvn", wavenumbers, sep="")
wood$class = "WOOD"
rm(indexWood)

# polymers
polyIndex = which(rawPrimpke$`Natural./Synthetic` =="synthetic polymer")
syntPolymer = rawPrimpke[polyIndex,]
counts = summary(as.factor(syntPolymer$Abbreviation))
polyNames = names(counts)[1:11] # only major polymers
syntPolymer = syntPolymer[which(syntPolymer$Abbreviation %in%  polyNames) , ]
classes = syntPolymer$Abbreviation
syntPolymer = syntPolymer[ , c(2:1864)] # leave out index column
names(syntPolymer) = paste("wvn",wavenumbers,sep="")
syntPolymer$class = as.character(classes)
rm (polyIndex, counts, classes)

# lets group together some polymer classes
syntPolymer$class[grep("Nylon",syntPolymer$class)] = "PA"
#syntPolymer$class[grep("HDPE",syntPolymer$class)] = "PE"
#syntPolymer$class[grep("LDPE",syntPolymer$class)] = "PE"

#==============================================================================#
              ### Working on our own sample data ###
#
# refFiles = list.files(run, full.names=TRUE, pattern=".txt") # files come as .txt
#
# resample_spectrum = function(file,targetRes){
#
#   data = read.csv(file, header= FALSE, sep="")
#   # Naming convention of reference sample files:
#    #01012000_CLASS_X.txt: date_classAbbreviation_number.txt"
#
#   if (ncol(data) != 2){
#     stop("Stopping! Sample data must be presented as a two columns dataframe!\n
#           Expecting wavenumbers in first column, reflectance values in second columns")
#   }
#   names(data) = c("wavenumber", "reflectance")
#   tmp = prospectr::resample2(data$reflectance, data$wavenumber, targetRes)
#   data = as.data.frame(t(tmp)) # transpose data to match data from Primpke
#   names(data) = paste("wvn", targetRes, sep="")
#   splitName = stringr::str_split(file, "/" )[[1]]
#   filename = gsub(dplyr::last(splitName), pattern =".txt", replacement="")
#   className = stringr::str_split(filename,"_")[[1]][2]
#   data$class = className
#   return(data)
#
# }
#
# refData = lapply(refFiles, resample_spectrum, targetRes=wavenumbers)
# refData = do.call("rbind", refData)
# refData$class[grep("Nylon", refData$class)] = "PA"
#
#
# # baseline correction for our sample data
# dummy = as.matrix(refData[ , 1:ncol(refData)-1])
# baslineDummy = baseline::baseline(dummy,
#                                   method="rfbaseline",
#                                   span=NULL,
#                                   NoXP=64,
#                                   maxit=c(10))
#
# corr_spectra = as.data.frame(baseline::getCorrected(baslineDummy))
# corr_spectra$class = as.factor(refData$class)


#==============================================================================#
                  ### Bringing the two databases together ###

data = rbind(furs,wood,fibre,syntPolymer) # currently only primpke data
data$class = as.factor(data$class)
descrip = as.data.frame(t(summary(data$class)))
print(descrip)
wavenumbers = as.numeric(stringr::str_remove(names(data), "wvn")[-ncol(data)])
write.csv(data, file = paste0(ref, "reference_database.csv"), row.names=FALSE)

# writing class control file
classIndex = as.character(unique(data$class))

for (class in classIndex){
  tmp = data[data$class==class , ]
  write.csv(tmp, file = paste0(ref, "reference_", class, ".csv"), row.names=FALSE)
}

write(classIndex, paste0(ref, "classes.txt"))
saveRDS(wavenumbers, paste0(ref, "wavenumbers.rds"))
