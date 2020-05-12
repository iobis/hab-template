source("lib.R")

filename <- "../data/region_6/habtemplate_Region_6_SEAsia_Azanza_2.xlsx"
folder <- "region_6"

hab <- read.xlsx(filename, sheet = 1, startRow = 2)
aphia <- read.xlsx(filename, sheet = 3, startRow = 1)
hab <- prepData(hab, colnames = c("scientificName", "identificationVerificationStatus", "references", "additionalReferences", "eventRemarks", "modified", "eventDate", "verbatimEventDate", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "footprintWKT", "locality", "minimumDepthInMeters", "maximumDepthInMeters", "quantityValue", "quantityUnit", "toxin", "toxinValue", "toxinUnit", "occurrenceRemarks"))
hab <- processTaxonomy(hab, aphia)

occurrence <- generateOccurrence(hab)
mof <- generateMof(hab)

write.table(occurrence, paste0("../output/", folder, "/occurrence.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)
write.table(mof, paste0("../output/", folder, "/measurementorfact.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)

