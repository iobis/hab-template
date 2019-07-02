source("lib.R")

filename <- "../data/region_3/habtemplate_a_v5 FANSA 2019-06-30.xlsx"
folder <- "region_3"

hab <- read.xlsx(filename, sheet = 1, startRow = 1)
aphia <- read.xlsx(filename, sheet = 3, startRow = 1)
hab <- prepData(hab)
hab <- processTaxonomy(hab, aphia)

occurrence <- generateOccurrence(hab)
mof <- generateMof(hab)

write.table(occurrence, paste0("../output/", folder, "/occurrence.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)
write.table(mof, paste0("../output/", folder, "/measurementorfact.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)

