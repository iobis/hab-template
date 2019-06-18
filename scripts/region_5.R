source("lib.R")

filename <- "../data/region_5/OBIS  9 Feb region 5 NZ highlighted April 2018_convertedtov5.xlsx"
folder <- "region_5"

hab <- read.xlsx(filename, sheet = 1, startRow = 2)
hab <- prepData(hab)
hab <- processTaxonomy(hab)

occurrence <- generateOccurrence(hab)
mof <- generateMof(hab)

write.table(occurrence, paste0("../output/", folder, "/occurrence.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)
write.table(mof, paste0("../output/", folder, "/measurementorfact.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)

