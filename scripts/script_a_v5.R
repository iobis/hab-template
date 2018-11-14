Sys.setlocale("LC_ALL", "en_US.UTF-8") 
library("openxlsx")
library("uuid")
library("worrms")
library("dplyr")
library("stringr")

#filename <- "../data/Feb 2018 HAB list_fensin_corrected.xlsx"
#filename <- "../data/habtemplate_a_v5 FANSA 2018-01-08.xlsx" # removed first header row
filename <- "../data/201811/habtemplate_a_v5 Region_3_Ines Sunesen July 2018.xlsx"

hab <- read.xlsx(filename, sheet = 1)
names(hab) <- c("scientificName", "original", "identificationVerificationStatus", "references", "additionalReferences", "eventRemarks", "modified", "eventDate", "verbatimEventDate", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "footprintWKT", "locality", "minimumDepthInMeters", "maximumDepthInMeters", "quantityValue", "quantityUnit", "toxin", "toxinValue", "toxinUnit", "occurrenceRemarks")
#obistools::plot_map_leaflet(hab)

### functions

cleanDf <- function(df) {
  if (!is.null(df)) {
    for (col in 1:ncol(df)) {
      if (class(df[,col])[1] == "character") {
        df[,col] <- sapply(df[,col], function(x) {
          x <- gsub("[\r\n]", " ", x)
          x <- gsub("  ", " ", x)
          x <- str_trim(x)
          if (!is.na(x) & x == "") {
            x <- NA
          }
          return(x)
        })
      }
    }
  }
  return(df)
}

### occurrence

hab <- cleanDf(hab)

# fixed fields

hab$basisOfRecord <- "HumanObservation"

# occurrenceID

hab$occurrenceID <- sapply(hab$scientificName, UUIDgenerate)

# scientificNameID

hab$curatedName <- hab$scientificName
hab$scientificName[!is.na(hab$original)] <- hab$original[!is.na(hab$original)]

fetchLSID <- function(name) {
  res <- wm_records_names(name)[[1]]
  for (j in 1:nrow(res)) {
    if (res$match_type == "exact") {
      return(res$lsid[j])
    }
  }
  return(NA)
}

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

# presence/absence

hab$occurrenceStatus <- "present"
hab$occurrenceStatus[hab$quantityValue == 0] <- "absent"

# generate

occurrence <- hab %>% select("occurrenceID", "scientificName", "eventDate", "verbatimEventDate", "decimalLatitude", "decimalLongitude", "locality", "minimumDepthInMeters", "maximumDepthInMeters", "coordinateUncertaintyInMeters", "basisOfRecord", "occurrenceStatus", "identificationVerificationStatus", "references", "additionalReferences", "modified", "footprintWKT", "occurrenceRemarks", "eventRemarks")

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

write.table(occurrence, "../output/occurrence.txt", quote=FALSE, sep="\t", na="", row.names=FALSE)
write.table(mof, "../output/measurementorfact.txt", quote=FALSE, sep="\t", na="", row.names=FALSE)

