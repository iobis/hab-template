source("lib.R")

filename <- "../data/mediterranean/HABtemplate  Mediterranean 10 December 2019.xlsx"
folder <- "mediterranean"

hab <- read.xlsx(filename, sheet = 1, startRow = 2)
aphia <- read.xlsx(filename, sheet = 3, startRow = 1)
hab <- prepData(hab)

# taxonomy

hab[hab$scientificName == "Amphidinium cf. carterae",]$curatedName <- "Amphidinium"
hab[hab$scientificName == "Fukuyoa spp.",]$curatedName <- "Fukuyoa"
hab[hab$scientificName == "Ostreopsis cf. ovata",]$curatedName <- "Ostreopsis" # also reported as Ostreopsis ovata
hab[hab$scientificName == "Ostreopsis cf. siamensis",]$curatedName <- "Ostreopsis"
ps <- which(hab$scientificName == "Pseufo-nitzschia subfraudulenta")
hab[ps,]$scientificName <- "Pseudo-nitzschia subfraudulenta"
hab[ps,]$curatedName <- "Pseudo-nitzschia subfraudulenta"
hab[hab$scientificName == "Pseudo-nitzschia pseudodelicatissima/ cuspidata",]$curatedName <- "Pseudo-nitzschia"

hab <- processTaxonomy(hab, aphia)
View(hab %>% select(scientificName, scientificNameID, curatedName) %>% filter(is.na(scientificNameID)))

# dates

hab$eventDate <- as.Date(as.numeric(hab$eventDate), origin = "1899-12-30")

# generate

occurrence <- generateOccurrence(hab)
mof <- generateMof(hab)

write.table(occurrence, paste0("../output/", folder, "/occurrence.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)
write.table(mof, paste0("../output/", folder, "/measurementorfact.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)

