source("lib.R")

filename <- "../data/africa/hab_Southern Africa - updated 18.05.2020.xlsx"
folder <- "africa"

hab <- read.xlsx(filename, sheet = 1, startRow = 1)
aphia <- read.xlsx(filename, sheet = 3, startRow = 1)

hab <- hab[2:nrow(hab),]
hab <- prepData(hab)

# taxonomy

hab <- processTaxonomy(hab, aphia)

# fix dates

hab$eventDate <- hab$verbatimEventDate
hab$eventDate[hab$eventDate == "not given"] <- NA

# generate

occurrence <- generateOccurrence(hab)
mof <- generateMof(hab)

write.table(occurrence, paste0("../output/", folder, "/occurrence.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)
write.table(mof, paste0("../output/", folder, "/measurementorfact.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)

