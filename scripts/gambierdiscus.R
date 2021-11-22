source("lib.R")

filename <- "../data/gambierdiscus/FINAL-Gambierdiscus_Fukuyoa_31 Dec_31_N246.xlsx"
folder <- "gambierdiscus"

# read file

hab <- read.xlsx(filename, sheet = 1, startRow = 1)
aphia <- read.xlsx(filename, sheet = 3, startRow = 1)

# process data

hab <- hab[2:nrow(hab),]
hab <- prepData(hab)

# taxonomy

#hab[hab$scientificName == "Gambeirdiscus carolinianus",]$curatedName <- "Gambierdiscus carolinianus"
#hab[hab$scientificName == "Gambierdiscus caribeaus",]$curatedName <- "Gambierdiscus caribaeus"

hab <- processTaxonomy(hab, aphia)

# generate dwc

occurrence <- generateOccurrence(hab)
mof <- generateMof(hab)

# output

write.table(occurrence, paste0("../output/", folder, "/occurrence.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)
write.table(mof, paste0("../output/", folder, "/measurementorfact.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)

