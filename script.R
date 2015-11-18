#devtools::install_github("pieterprovoost/worms")
require(xlsx)
require(worms)

# config

#country <- "Jamaica"
#filename <- "HAB_Jamaica.xls"
#sheetname <- "reviewed"
#datasetid <- "HAB_Jamaica"
#headerrow <- 1
#skiprows <- 1

#country <- "Cuba"
#filename <- "HAB_Cuba.xlsx"
#sheetname <- "reviewed"
#datasetid <- "HAB_Cuba"
#headerrow <- 1
#skiprows <- 1

#country <- "Colombia"
#filename <- "HAB_Colombia.xls"
#sheetname <- "reviewed"
#datasetid <- "HAB_Colombia"
#headerrow <- 1
#skiprows <- 1

#country <- "ElSalvador"
#filename <- "HAB_ElSalvador.xls"
#sheetname <- "reviewed"
#datasetid <- "HAB_ElSalvador"
#headerrow <- 1
#skiprows <- 1

#country <- "Guatemala"
#filename <- "HAB_Guatemala.xls"
#sheetname <- "reviewed"
#datasetid <- "HAB_Guatemala"
#headerrow <- 1
#skiprows <- 1

country <- "Panama"
filename <- "HAB_Panama2.xls"
sheetname <- "reviewed"
datasetid <- "HAB_Panama"
headerrow <- 1
skiprows <- 1

#country <- "Nicaragua"
#filename <- "HAB_Nicaragua.xls"
#sheetname <- "reviewed"
#datasetid <- "HAB_Nicaragua"
#headerrow <- 1
#skiprows <- 1

#country <- "CostaRica"
#filename <- "HAB_CostaRica.xls"
#sheetname <- "reviewed"
#datasetid <- "HAB_CostaRica"
#headerrow <- 1
#skiprows <- 1

# fixed fields

fixed <- list(
  language="en",
  license="CC0",
  rightsHolder="UNESCO Intergovernmental Oceanographic Commission",
  basisOfRecord="HumanObservation",
  institutionID="",
  institutionCode="",
  collectionCode="",
  datasetID=datasetid
)

# functions

cleanDf <- function(df) {
  if (!is.null(df)) {
    for (col in 1:ncol(df)) {
      if (class(df[,col])[1] == "character") {
        df[,col] <- sapply(df[,col], function(x) {
          x <- gsub("[\r\n]", " ", x)
          x <- gsub("  ", " ", x)
          return(x)
        })
      }
    }
  }
  return(df)
}

writeDwc <- function(data, file) {
  if (!is.null(data)) {
    write.table(data, file=file, quote=FALSE, sep="\t", na="", row.names=FALSE)
  }
}

addMof <- function(mof, occurrence, id=NA, type=NA, value=NA, accuracy=NA, unit=NA, determinedby=NA, determineddate=NA, method=NA, remarks=NA) {
  if (!is.na(value) & value != "ND") {
    mof <- rbind(mof, data.frame(occurrenceID=occurrence, measurementID=id, measurementType=type, measurementValue=value, measurementAccuracy=accuracy, measurementUnit=unit, measurementDeterminedBy=determinedby, measurementDeterminedDate=determineddate, measurementMethod=method, measurementRemarks=remarks, stringsAsFactors=FALSE))
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

addReference <- function(references, reference=NA) {
  if (!is.na(reference)) {
    references <- c(references, reference)
  }
  return(references)
}

addEffect <- function(effects, key, value=NA) {
  if (!is.na(value) & value != "ND") {
    effects <- c(effects, paste0(key, ": ", value))
  }
  return(effects)
}

# read sheet

data <- read.xlsx(filename, sheetName=sheetname, startRow=headerrow, stringsAsFactors=FALSE, header=TRUE)
data <- data[skiprows+1:nrow(data),]

# process records

measurements <- NULL
numbers <- unique(data$eventid)[!is.na(unique(data$eventid))]
occurrence <- NULL

# process records event by event

for (n in numbers) {
  
  rdata <- data[data$eventid == n & !is.na(data$eventid),]

  # create dataframe for measurements
  
  mof <- data.frame(occurrenceID=character(), measurementID=character(), measurementType=character(), measurementValue=character(), measurementAccuracy=character(), measurementUnit=character(), measurementDeterminedDate=character(), measurementDeterminedBy=character(), measurementMethod=character(), measurementRemarks=character(), stringsAsFactors=FALSE)
  
  # process event related fields on first line
  
  i <- 1
  result <- list()
  
  eventRemarks <- NULL
  effects <- NULL
  references <- NULL
  
  result$eventID <- rdata[i, "eventid"]
  result$recordedBy <- paste0("HAB region ", rdata[i, "habregion"], ";", rdata[i, "recordedby"])
  result$modified <- as.POSIXct((as.numeric(rdata[i, "modified"])-25569)*86400, tz="GMT", origin="1970-01-01")
  references <- addReference(references, rdata[i, "mainreference"])
  references <- addReference(references, rdata[i, "additionalreferences"])
  result$references <- rdata[i, "eventurl"]
  # todo: add event date validation
  eventdate <- rdata[i, "eventdate"]
  startyear <- rdata[i, "startyear"]
  startmonth <- rdata[i, "startmonth"]
  startday <- rdata[i, "startday"]
  endyear <- rdata[i, "endyear"]
  endmonth <- rdata[i, "endmonth"]
  enday <- rdata[i, "endday"]
  result$eventDate <- eventdate
  result$verbatimEventDate <- rdata[i, "verbatimdate"]
  result$sampleSizeValue <- rdata[i, "samplesizevalue"]
  result$sampleSizeUnit <- rdata[i, "samplesizeunit"]
  result$samplingProtocol <- rdata[i, "samplingprotocol"]
  result$samplingEffort <- rdata[i, "samplingeffort"]
  eventRemarks <- addRemark(eventRemarks, rdata[i, "eventremarks"])
  if (!is.na(rdata[i, "associatedtaxa"])) {
    result$associatedTaxa <- paste0("macroalgal species: ", rdata[i, "associatedtaxa"])
  }
  mof <- addMof(mof, NA, type="water discoloration", value=rdata[i, "waterdiscoloration"], remarks=rdata[i, "waterdiscolorationremarks"])
  mof <- addMof(mof, NA, type="mucus", value=rdata[i, "mucus"])
  mof <- addMof(mof, NA, type="HAB related mass mortalities", value=rdata[i, "massmortalities"])
  mof <- addMof(mof, NA, type="High phyto concentrations", value=rdata[i, "highphytoconcentrations"])
  mof <- addMof(mof, NA, type="foam/mucilage on the coast", value=rdata[i, "foammucilagecoast"])
  eventRemarks <-  addRemark(eventRemarks, rdata[i, "bloomremarks"])
  eventRemarks <-  addRemark(eventRemarks, rdata[i, "additionalinfo"])
  mof <- addMof(mof, NA, type="toxicity detected", value=rdata[i, "toxicity"])
  mof <- addMof(mof, NA, type="toxin name", value=rdata[i, "toxin"])
  mof <- addMof(mof, NA, type="toxicity syndrome", value=rdata[i, "syndrome"])
  mof <- addMof(mof, NA, type="intoxication transvector", value=rdata[i, "transvector"])
  mof <- addMof(mof, NA, type="economic effect", value=rdata[i, "economiclosses"])
  effects <- addEffect(effects, "planktonic life", rdata[i, "planktoniclife"])
  effects <- addEffect(effects, "benthic life", rdata[i, "benthiclife"])
  effects <- addEffect(effects, "shellfish", rdata[i, "shellfish"])
  effects <- addEffect(effects, "natural fish", rdata[i, "naturalfish"])
  effects <- addEffect(effects, "aquaculture fish", rdata[i, "aquaculturefish"])
  effects <- addEffect(effects, "aquatic mammals", rdata[i, "aquaticmammals"])
  effects <- addEffect(effects, "birds", rdata[i, "birds"])
  effects <- addEffect(effects, "other terrestrial", rdata[i, "otherterrestrial"])
  effects <- addEffect(effects, "humans", rdata[i, "humans"])
  effects <- addEffect(effects, "other", rdata[i, "other"])
  result$verbatimLocality <- rdata[i, "place"]
  result$decimalLongitude <- rdata[i, "longitude"]
  result$decimalLatitude <- rdata[i, "latitude"]
  result$verbatimLongitude <- rdata[i, "Verbatimlongitude"]
  result$verbatimLatitude <- rdata[i, "Verbatimlatitude"]
  result$coordinateUncertaintyInMeters <- rdata[i, "coordinateuncertainty"]
  result$coordinatePrecision <- rdata[i, "coordinateprecision"]
  result$footprintWKT <- rdata[i, "footprint"]
  result$waterBody <- rdata[i, "waterbody"]
  result$country <- rdata[i, "country"]
  result$stateProvince <- rdata[i, "stateprovince"]
  result$county <- rdata[i, "county"]
  result$municipality <- rdata[i, "municipality"]
  result$island <- rdata[i, "island"]
  result$islandGroup <- rdata[i, "islandgroup"]
  result$locality <- rdata[i, "locality"]
  result$locationID <- rdata[i, "locationid"]
  result$locationAccordingTo <- rdata[i, "locationaccordingto"]
  result$habitat <- rdata[i, "habitat"]
  result$fieldNotes <- rdata[i, "oligotrophiceutrophic"]
  mof <- addMof(mof, NA, type="substrate type", value=rdata[i, "substrate"])
  result$verbatimDepth <- rdata[i, "verbatimdepth"]
  result$minimumDepthInMeters <- rdata[i, "minimumdepth"]
  result$maximumDepthInMeters <- rdata[i, "maximumdepth"]
  result$locationRemarks <- rdata[i, "locationremarks"]
  mof <- addMof(mof, NA, type="temperature", value=rdata[i, "temperature"], method=rdata[i, "temperaturemethod"], unit=rdata[i, "temperatureunits"])
  mof <- addMof(mof, NA, type="salinity", value=rdata[i, "salinity"], method=rdata[i, "salinitymethod"], unit=rdata[i, "salinityunits"])
  
  if (length(effects) > 0) {
    mof <- addMof(mof, NA, type="environmental effect", value=paste0(effects, collapse=";"))
  }
  result$associatedReferences <- paste0(references, collapse=" | ")
  result$eventRemarks <- paste0(eventRemarks, collapse=";")
  
  # process repeated lines (measurements)

  for (i in seq(1, nrow(rdata))) {
    mof <- addMof(mof, NA, type=rdata[i, "measurementtype"], value=rdata[i, "measurementvalue"], unit=rdata[i, "measurementunit"], method=rdata[i, "measurementmethod"], remarks=rdata[i, "measurementremarks"])
    mof <- addMof(mof, NA, type=rdata[i, "nutrientmeasurementtype"], value=rdata[i, "nutrientmeasurementvalue"], unit=rdata[i, "nutrientmeasurementunit"], method=rdata[i, "nutrientmeasurementmethod"])
    mof <- addMof(mof, NA, type=rdata[i, "othermeasurementtype"], value=rdata[i, "othermeasurementvalue"], unit=rdata[i, "othermeasurementunit"], method=rdata[i, "othermeasurementmethod"])
  }
  
  # process repeated lines (occurrences)
  
  for (i in seq(1, nrow(rdata))) {

    # clone result to create new occurrence
    
    occresult <- result
    
    occresult$catalogNumber <- paste0(rdata[i, "eventid"], "_", i)
    occresult$occurrenceID <- paste0("HAB_", country, "_", rdata[i, "eventid"], "_", i)
    occresult$scientificName <- rdata[i, "scientificname"]
    occresult$organismQuantity <- rdata[i, "organismquantity"]
    occresult$organismQuantityType <- rdata[i, "organismquantitytype"]
    
    occurrenceRemarks <- NULL
    occurrenceRemarks <- addRemark(occurrenceRemarks, rdata[i, "occurrenceremarks"])
    occresult$occurrenceRemarks <- paste0(occurrenceRemarks, collapse=";")

    # append occurrence if scientific name is not NA
        
    if (!is.na(occresult$scientificName)) {
      
      occurrence <- rbind(occurrence, data.frame(occresult, stringsAsFactors=FALSE))

      # duplicate measurements  
      
      if (nrow(mof) > 0) {
        occmof <- mof
        occmof$occurrenceID <- occresult$occurrenceID
        measurements <- rbind(measurements, occmof)
      }
      
    }
    
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
  match <- matchAphiaRecordsByNames(occurrence$scientificName[i])
  occurrence$scientificName[i] <- match$scientificname[1]
  occurrence$scientificNameID[i] <- match$lsid[1]
  occurrence$scientificNameAuthorship[i] <- match$authority[1]
  occurrence$kingdom[i] <- match$kingdom[1]
  occurrence$phylum[i] <- match$phylum[1]
  occurrence$class[i] <- match$class[1]
  occurrence$order[i] <- match$order[1]
  occurrence$family[i] <- match$family[1]
  occurrence$genus[i] <- match$genus[1]
  if (match$rank[1] == "Species") {
    occurrence$species[i] <- match$scientificname[1]
  }
}

# fixed fields

for (name in names(fixed)) {
  occurrence[[name]] <- fixed[[name]]
}

# measurements id

if (!is.null(measurements)) {
  measurements$measurementID <- seq(1, nrow(measurements))
}

# clean up strings

occurrence <- cleanDf(occurrence)
measurements <- cleanDf(measurements)

# output

writeDwc(occurrence, "occurrence.txt")
writeDwc(measurements, "measurementorfact.txt")

