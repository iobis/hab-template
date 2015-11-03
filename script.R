require(xlsx)

# config

country <- "Colombia"

fixed <- list(
  language="en",
  license="",
  rightsHolder="",
  institutionID="",
  institutionCode="",
  collectionCode="",
  datasetID=""
)

filename <- paste0("HABData_", toupper(country), ".xls")
headerrow <- 3
skiprows <- 2

# functions

writeDwc <- function(data, file) {
  write.table(data, file=file, quote=FALSE, sep="\t", na="", row.names=FALSE)
}

addMof <- function(mof, occurrence, id=NA, type=NA, value=NA, accuracy=NA, unit=NA, date=NA, determinedby=NA, determineddate=NA, method=NA, remarks=NA) {
  if (!is.na(occurrence) & !is.na(value) & value != "ND") {
    mof <- rbind(mof, data.frame(occurrenceID=occurrence, measurementID=id, measurementType=type, measurementValue=value, measurementAccuracy=accuracy, meadurementUnit=unit, measurementDate=date, measurementDeterminedBy=determinedby, measurementDeterminedDate=determineddate, measurementMethod=method, measurementRemarks=remarks))
  }
  return(mof)
}

addRemark <- function(remarks, remark=NA, key=NA) {
  if (!is.na(remark)) {
    if (!is.na(key)) {
      remark <- paste0(key, ": ", remark)
    }
    remarks <- c(remarks, remark)
  }
  return(remarks)
}

addEffect <- function(effects, key, value=NA) {
  if (!is.na(value) & value != "ND") {
    effects <- c(effects, paste0(key, ": ", value))
  }
  return(effects)
}

# read sheets

data <- read.xlsx(filename, sheetName="template", startRow=headerrow, stringsAsFactors=FALSE, header=TRUE)
regions <- read.xlsx(filename, sheetName="regions", stringsAsFactors=FALSE, header=FALSE)
aphia <- read.xlsx(filename, sheetName="aphia", stringsAsFactors=FALSE, header=TRUE)
toxins <- read.xlsx(filename, sheetName="toxins", stringsAsFactors=FALSE, header=FALSE)
syndromes <- read.xlsx(filename, sheetName="syndromes", stringsAsFactors=FALSE, header=FALSE)

# clean sheets

data <- data[skiprows+1:nrow(data),]

# parse records

mof <- data.frame(occurrenceID=character(), measurementID=character(), measurementType=character(), measurementValue=character(), measurementAccuracy=character(), measurementUnit=character(), measurementDeterminedDate=character(), measurementDeterminedBy=character(), measurementMethod=character(), measurementRemarks=character())
occurrence <- NULL

numbers <- unique(data$Record.number)[!is.na(unique(data$Record.number))]

for (n in numbers) {
  
  rdata <- data[data$Record.number == n & !is.na(data$Record.number),]
  
  # process first line
  
  i <- 1
  result <- list()
  
  occurrenceRemarks <- NULL
  eventRemarks <- NULL
  effects <- NULL
  
  result$catalogNumber <- rdata[i, "Record.number"]
  result$occurrenceID <- paste0("HAB_", country, "_", rdata[i, "Record.number"])
  result$recordedBy <- paste0("HAB region ", rdata[i, "HAB.region"], ";", rdata[i, "Recorded.by"])
  result$modified <- as.POSIXct((as.numeric(rdata[i, "Modified"])-25569)*86400, tz="GMT", origin="1970-01-01")
  result$associatedReferences <- paste0(rdata[i, c("Main.reference", "Additional.references")], collapse=" | ")
  result$references <- rdata[i, "HADEDAT.event.URL"]
  result$eventDate <- rdata[i, "Event.date"]
  result$verbatimEventDate <- rdata[i, "VerbatimDate"]
  result$scientificName <- rdata[i, "Scientific.name"]
  result$organismQuantity <- rdata[i, "Organism.quantity"]
  result$organismQuantityType <- rdata[i, "Organism.quantity.type"]
  result$sampleSizeValue <- rdata[i, "Sample.size.value"]
  result$sampleSizeUnit <- rdata[i, "Sample.size.unit"]
  result$samplingProtocol <- rdata[i, "Sampling.protocol"]
  result$samplingEffort <- rdata[i, "Sampling.effort"]
  eventRemarks <- addRemark(eventRemarks, rdata[i, "Sampling.Event.Remarks"])
  if (!is.na(rdata[i, "Macroalgal.species"])) {
    result$associatedTaxa <- paste0("macroalgal species: ", rdata[i, "Macroalgal.species"])
  }
  occurrenceRemarks <- addRemark(occurrenceRemarks, rdata[i, "Occurrence.remarks"])
  mof <- addMof(mof, result$occurrenceID, type="water discoloration", value=rdata[i, "Water.discoloration"], remarks=rdata[i, "Water.discoloration.remarks"])
  mof <- addMof(mof, result$occurrenceID, type="mucus", value=rdata[i, "Mucus"])
  mof <- addMof(mof, result$occurrenceID, type="HAB related mass mortalities", value=rdata[i, "Mass.mortalities"])
  occurrenceRemarks <- addRemark(occurrenceRemarks, rdata[i, "High.phyto.concentrations"], "High phyto concentrations")
  mof <- addMof(mof, result$occurrenceID, type="foam/mucilage on the coast", value=rdata[i, "Foam.mucilage.on.the.coast"])
  eventRemarks <-  addRemark(eventRemarks, rdata[i, "Additional.remarks"])
  eventRemarks <-  addRemark(eventRemarks, rdata[i, "Additional.remarks.1"])
  mof <- addMof(mof, result$occurrenceID, type="toxicity detected", value=rdata[i, "Toxicity.detected"])
  mof <- addMof(mof, result$occurrenceID, type="toxin name", value=rdata[i, "ToxinName"])
  mof <- addMof(mof, result$occurrenceID, type="toxicity syndrome", value=rdata[i, "Syndrome"])
  mof <- addMof(mof, result$occurrenceID, type="intoxication transvector", value=rdata[i, "Intoxication..transvector"])
  mof <- addMof(mof, result$occurrenceID, type="economic effect", value=rdata[i, "Economic.losses"])
  effects <- addEffect(effects, "planktonic life", rdata[i, "Planktonic.life"])
  effects <- addEffect(effects, "benthic life", rdata[i, "Benthic.life"])
  effects <- addEffect(effects, "shellfish", rdata[i, "Shellfish"])
  effects <- addEffect(effects, "natural fish", rdata[i, "Natural.fish"])
  effects <- addEffect(effects, "aquaculture fish", rdata[i, "Aquaculture.fish"])
  effects <- addEffect(effects, "aquatic mammals", rdata[i, "Aquatic.mammals"])
  effects <- addEffect(effects, "birds", rdata[i, "Birds"])
  effects <- addEffect(effects, "other terrestrial", rdata[i, "Other.terrestrial"])
  effects <- addEffect(effects, "humans", rdata[i, "Humans"])
  effects <- addEffect(effects, "other", rdata[i, "Other"])
  result$verbatimLocality <- rdata[i, "Place.of.event"]
  result$decimalLongitude <- rdata[i, "Longitude"]
  result$decimalLatitude <- rdata[i, "Latitude"]
  result$verbatimLongitude <- rdata[i, "VerbatimLongitude"]
  result$verbatimLatitude <- rdata[i, "VerbatimLatitude"]
  result$coordinateUncertaintyInMeters <- rdata[i, "Coordinate.uncertainty"]
  result$coordinatePrecision <- rdata[i, "coordinatePrecision"]
  result$footprintWKT <- rdata[i, "Footprint.WKT"]
  result$waterBody <- rdata[i, "Water.body"]
  result$country <- rdata[i, "Country"]
  result$stateProvince <- rdata[i, "State.province"]
  result$county <- rdata[i, "County"]
  result$municipality <- rdata[i, "Municipality"]
  result$island <- rdata[i, "Island"]
  result$islandGroup <- rdata[i, "Island.group"]
  result$locality <- rdata[i, "Locality"]
  result$locationID <- rdata[i, "Location.ID"]
  result$locationAccordingTo <- rdata[i, "Location.according.to"]
  result$habitat <- rdata[i, "Habitat"]
  result$fieldNotes <- rdata[i, "Oligotrophic.Eutrophic"]
  mof <- addMof(mof, result$occurrenceID, type="substrate type", value=rdata[i, "Substrate.type"])
  result$minimumDepthInMeters <- rdata[i, "Minimum.depth"]
  result$maximumDepthInMeters <- rdata[i, "Maximum.depth"]
  result$verbatimDepth <- rdata[i, "Surface.subsurface.whole.water.column"]
  result$locationRemarks <- rdata[i, "Location.remarks"]
  mof <- addMof(mof, result$occurrenceID, type="temperature", value=rdata[i, "Temperature.value"], method=rdata[i, "Temperatrue.method"])
  mof <- addMof(mof, result$occurrenceID, type="salinity", value=rdata[i, "Salinity.value"], method=rdata[i, "Salinity.method"])
  
  if (length(effects) > 0) {
    mof <- addMof(mof, result$occurrenceID, type="environmental effect", value=paste0(effects, collapse=";"))
  }
  result$eventRemarks <- paste0(eventRemarks, collapse=";")
  result$occurrenceRemarks <- paste0(occurrenceRemarks, collapse=";")

  if (!is.na(result$scientificName)) {
    occurrence <- rbind(occurrence, data.frame(result))
  }
  
  # repeated lines

  for (i in seq(1, nrow(rdata))) {
    mof <- addMof(mof, result$occurrenceID, type=rdata[i, "Measurement.Type"], value=rdata[i, "Measurement.Value"], unit=rdata[i, "Measurement.Unit"], method=rdata[i, "Measurement.method"], remarks=rdata[i, "Additional.remarks.1"],)
    mof <- addMof(mof, result$occurrenceID, type=rdata[i, "MeasurementType"], value=rdata[i, "Measurement.value"], unit=rdata[i, "Measurement.unit"], method=rdata[i, "Measurement.method.1"])
    mof <- addMof(mof, result$occurrenceID, type=rdata[i, "MeasurementType.1"], value=rdata[i, "Measurement.value.1"], unit=rdata[i, "Measurement.unit.1"], method=rdata[i, "Measurement.method.2"])
  }
  
}

# add taxonomy

occurrence$kingdom <- NA
occurrence$phylum <- NA
occurrence$class <- NA
occurrence$order <- NA
occurrence$family <- NA
occurrence$genus <- NA
occurrence$species <- NA

for (i in 1:nrow(occurrence)) {
  taxon <- aphia[aphia$ScientificName==occurrence$scientificName[i],]
  occurrence$scientificNameID <- paste0("urn:lsid:marinespecies.org:taxname:", taxon$AphiaID)
  occurrence$scientificNameAuthorship[i] <- taxon$Authority
  occurrence$kingdom[i] <- taxon$Kingdom
  occurrence$phylum[i] <- taxon$Phylum
  occurrence$class[i] <- taxon$Class
  occurrence$order[i] <- taxon$Order
  occurrence$family[i] <- taxon$Family
  occurrence$genus[i] <- taxon$Genus
  if (!is.na(taxon$Species)) {
    occurrence$species[i] <- paste0(taxon$Genus, " ", taxon$Species)
  }
}

# fixed fields

for (name in names(fixed)) {
  occurrence[[name]] <- fixed[[name]]
}

# measurements id

mof$measurementID <- seq(1, nrow(mof))

# output

writeDwc(occurrence, "occurrence.txt")
writeDwc(mof, "measurementorfact.txt")