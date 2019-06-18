source("lib.R")

# needs fixing!

filename <- "../data/region_1/Feb 2018 HAB list_Region_1_fensin_corrected.xlsx"
folder <- "region_1"

hab <- read.xlsx(filename, sheet = 1, startRow = 2)
hab <- prepData(hab)
hab <- processTaxonomy(hab)

occurrence <- generateOccurrence(hab)
mof <- generateMof(hab)

write.table(occurrence, paste0("../output/", folder, "/occurrence.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)
write.table(mof, paste0("../output/", folder, "/measurementorfact.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)

