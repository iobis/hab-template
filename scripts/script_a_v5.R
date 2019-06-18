Sys.setlocale("LC_ALL", "en_US.UTF-8") 
library("openxlsx")
library("uuid")
library("worrms")
library("dplyr")
library("stringr")
library("tidyr") # dev version!
source("lib.R")

#filename <- "../data/Feb 2018 HAB list_fensin_corrected.xlsx"
#filename <- "../data/habtemplate_a_v5 FANSA 2018-01-08.xlsx" # removed first header row
#filename <- "../data/201811/habtemplate_a_v5 Region_3_Ines Sunesen July 2018.xlsx"
filename <- "../data/region_5/OBIS  9 Feb region 5 NZ highlighted April 2018_convertedtov5.xlsx"
folder <- "region_5"

hab <- read.xlsx(filename, sheet = 1, startRow = 2)
names(hab) <- c("scientificName", "original", "identificationVerificationStatus", "references", "additionalReferences", "eventRemarks", "modified", "eventDate", "verbatimEventDate", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "footprintWKT", "locality", "minimumDepthInMeters", "maximumDepthInMeters", "quantityValue", "quantityUnit", "toxin", "toxinValue", "toxinUnit", "occurrenceRemarks")

hab <- cleanDf(hab)

hab$decimalLatitude <- as.numeric(hab$decimalLatitude)
hab$decimalLongitude <- as.numeric(hab$decimalLongitude)
obistools::plot_map_leaflet(hab)
hab <- hab %>% unite(associatedReferences, references, additionalReferences, sep = ";", remove = TRUE, na.rm = TRUE)

# fixed fields

hab$basisOfRecord <- "HumanObservation"

# occurrenceID

hab$occurrenceID <- sapply(hab$scientificName, UUIDgenerate)

# scientificNameID

hab$curatedName <- hab$scientificName
hab$scientificName[!is.na(hab$original)] <- hab$original[!is.na(hab$original)]

hab$scientificNameID <- NA

for (i in 1:nrow(hab)) {
  name <- hab$curatedName[i]
  tryCatch({
      hab$scientificNameID[i] <- fetchLSID(name)
    }, warning = function(w) {
      message(sprintf("Warning for: %s", name))
    }, error = function(e) {
      message(sprintf("Error for: %s", name))
    }
  )
}

### taxonomic fixes

hab[hab$scientificName == "Amphidinium operculatum",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:109745"
hab[hab$scientificName == "Gonyaulax spinifera",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:110041"
hab[hab$scientificName == "Dinophysis mitra",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:109635"
hab[hab$scientificName == "Dinophysis ovum",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:109642"
hab[hab$scientificName == "Microcystis aeruginosa",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:146557"
hab[hab$scientificName == "Microcystis wesenbergii",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:146557"
hab[hab$scientificName == "Microcystis botrys",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:146557"

# presence/absence

hab$occurrenceStatus <- "present"
hab$occurrenceStatus[hab$quantityValue == 0] <- "absent"

# generate

occurrence <- hab %>% select("occurrenceID", "scientificName", "scientificNameID", "eventDate", "verbatimEventDate", "decimalLatitude", "decimalLongitude", "locality", "minimumDepthInMeters", "maximumDepthInMeters", "coordinateUncertaintyInMeters", "basisOfRecord", "occurrenceStatus", "identificationVerificationStatus", "associatedReferences", "modified", "footprintWKT", "occurrenceRemarks", "eventRemarks")

### measurements

quantityMof <- hab %>%
  select(id = "occurrenceID", measurementValue = "quantityValue", measurementUnit = "quantityUnit")  %>%
  mutate(measurementType = "quantity", measurementValue = as.character(measurementValue), measurementUnit = as.character(measurementUnit)) %>%
  filter(!is.na(measurementValue))

toxinMof <- hab %>%
  select(id = "occurrenceID", measurementType = "toxin", measurementValue = "toxinValue", measurementUnit = "toxinUnit")  %>%
  mutate(measurementValue = as.character(measurementValue), measurementUnit = as.character(measurementUnit)) %>%
  filter(!is.na(measurementType))

mof <- bind_rows(toxinMof, quantityMof)

### output

write.table(occurrence, paste0("../output/", folder, "/occurrence.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)
write.table(mof, paste0("../output/", folder, "/measurementorfact.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)

