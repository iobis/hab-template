Sys.setlocale("LC_ALL", "en_US.UTF-8") 

library("openxlsx")
library("uuid")
library("worrms")
library("dplyr")
library("stringr")
library("tidyr") # dev version!

generateMof <- function(hab) {
  quantityMof <- generateQuantityMof(hab)
  toxinMof <- generateToxinMof(hab)
  mof <- bind_rows(toxinMof, quantityMof)
  return(mof)
}

generateToxinMof <- function(hab) {
  toxinMof <- hab %>%
    select(id = "occurrenceID", measurementType = "toxin", measurementValue = "toxinValue", measurementUnit = "toxinUnit")  %>%
    mutate(measurementValue = as.character(measurementValue), measurementUnit = as.character(measurementUnit)) %>%
    filter(!is.na(measurementType))
  return(toxinMof)
}

generateQuantityMof <- function(hab) {
  quantityMof <- hab %>%
    select(id = "occurrenceID", measurementValue = "quantityValue", measurementUnit = "quantityUnit")  %>%
    mutate(measurementType = "quantity", measurementValue = as.character(measurementValue), measurementUnit = as.character(measurementUnit)) %>%
    filter(!is.na(measurementValue))
  return(quantityMof)
}

generateOccurrence <- function(hab) {
  occurrence <- hab %>%
    select("occurrenceID", "scientificName", "scientificNameID", "eventDate", "verbatimEventDate", "decimalLatitude", "decimalLongitude", "locality", "minimumDepthInMeters", "maximumDepthInMeters", "coordinateUncertaintyInMeters", "basisOfRecord", "occurrenceStatus", "identificationVerificationStatus", "associatedReferences", "modified", "footprintWKT", "occurrenceRemarks", "eventRemarks")
  return(occurrence)
}

processTaxonomy <- function(hab) {
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
  hab <- taxonomyFixes(hab)
  return(hab)
}

prepData <- function(hab) {
  names(hab) <- c("scientificName", "original", "identificationVerificationStatus", "references", "additionalReferences", "eventRemarks", "modified", "eventDate", "verbatimEventDate", "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters", "footprintWKT", "locality", "minimumDepthInMeters", "maximumDepthInMeters", "quantityValue", "quantityUnit", "toxin", "toxinValue", "toxinUnit", "occurrenceRemarks")
  hab <- cleanDf(hab)
  hab$decimalLatitude <- as.numeric(hab$decimalLatitude)
  hab$decimalLongitude <- as.numeric(hab$decimalLongitude)
  hab <- hab %>% unite(associatedReferences, references, additionalReferences, sep = ";", remove = TRUE, na.rm = TRUE)
  hab$basisOfRecord <- "HumanObservation"
  hab$occurrenceID <- sapply(hab$scientificName, UUIDgenerate)
  hab$curatedName <- hab$scientificName
  hab$occurrenceStatus <- "present"
  hab$occurrenceStatus[hab$quantityValue == 0] <- "absent"
  return(hab)
}

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

fetchLSID <- function(name) {
  res <- wm_records_names(name)[[1]]
  for (j in 1:nrow(res)) {
    if (res$match_type == "exact") {
      return(res$lsid[j])
    }
  }
  return(NA)
}

taxonomyFixes <- function(hab) {
  hab[hab$scientificName == "Amphidinium operculatum",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:109745"
  hab[hab$scientificName == "Gonyaulax spinifera",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:110041"
  hab[hab$scientificName == "Dinophysis mitra",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:109635"
  hab[hab$scientificName == "Dinophysis ovum",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:109642"
  hab[hab$scientificName == "Microcystis aeruginosa",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:146557"
  hab[hab$scientificName == "Microcystis wesenbergii",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:146557"
  hab[hab$scientificName == "Microcystis botrys",]$scientificNameID <- "urn:lsid:marinespecies.org:taxname:146557"
  return(hab)
}
