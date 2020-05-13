source("lib.R")

filename <- "../data/mediterranean/HABtemplate  Mediterranean 12 MAy 2020 OBIS.xlsx"
folder <- "mediterranean"

hab <- read.xlsx(filename, sheet = 1, startRow = 1)
aphia <- read.xlsx(filename, sheet = 3, startRow = 1)

# remove second row

hab <- hab[2:nrow(hab),]

# remove added columns

df_added <- hab[,1:3]
df_country <- hab[,18]

hab <- hab[,c(4:17, 19:ncol(hab))]

hab <- prepData(hab)

# taxonomy

#hab[hab$scientificName == "Amphidinium cf. carterae",]$curatedName <- "Amphidinium"
hab[hab$scientificName == "Fukuyoa spp.",]$curatedName <- "Fukuyoa"
hab[hab$scientificName == "Ostreopsis cf. ovata",]$curatedName <- "Ostreopsis" # also reported as Ostreopsis ovata
hab[hab$scientificName == "Ostreopsis cf. siamensis",]$curatedName <- "Ostreopsis"
#ps <- which(hab$scientificName == "Pseufo-nitzschia subfraudulenta")
#hab[ps,]$scientificName <- "Pseudo-nitzschia subfraudulenta"
#hab[ps,]$curatedName <- "Pseudo-nitzschia subfraudulenta"
hab[hab$scientificName == "Pseudo-nitzschia pseudodelicatissima/ cuspidata",]$curatedName <- "Pseudo-nitzschia"
hab[hab$scientificName == "Prorocentrum rhathymun",]$curatedName <- "Prorocentrum rhathymum"
hab[hab$scientificName == "Pseudo-nitzschia multristriata",]$curatedName <- "Pseudo-nitzschia multistriata"

hab <- processTaxonomy(hab, aphia)
View(hab %>% select(scientificName, scientificNameID, curatedName) %>% filter(is.na(scientificNameID)))

# convert dates

hab$eventDate <- as.Date(as.numeric(hab$eventDate), origin = "1899-12-30")

# fix dates

hab$verbatimEventDate <- recode(hab$verbatimEventDate,
  "1981 - 2000" = "1981/2000",
  "August - September 1996" = "1996-08/1996-09",
  "2001 - 2011" = "2001/2011",
  "1983 - 1985" = "1983/1985",
  "1984 - 2006" = "1984/2006",
  "1989 - 1990" = "1989/1990",
  "1990 - 1991" = "1990/1991",
  "1992 - 2001" = "1992/2001",
  "1994 - 1996" = "1994/1996",
  "1995 - 2010" = "1995/2010",
  "1995 -1999" = "1995/1999",
  "1996 - 1999" = "1996/1999",
  "1997 -1999" = "1997/1999",
  "1998 - 1999" = "1998/1999",
  "1998 - 2005" = "1998/2005",
  "2001 - 2002" = "2001/2002",
  "2001-2003" = "2001/2003",
  "2002-2003" = "2002/2003",
  "2002-2004" = "2002/2004",
  "2003 - 2008" = "2003/2008",
  "2004 - 2008" = "2004/2008",
  "2004-2008" = "2004/2008",
  "2005 - 2008" = "2005/2008",
  "2006 - 2007" = "2006/2007",
  "2006 - 2008" = "2006/2008",
  "2007 - 2008" = "2007/2008",
  "2008 - 2008" = "2008",
  "2008 - 2009" = "2008/2009",
  "2008-2009 " = "2008/2009",
  "2009 - 2008" = "2008/2009",
  "April 1984 - 2006" = "1984-2006",
  "Autumn 1991" = "1991",
  "December 1984 - 2006" = "1984-2006",
  "February - June 1986 - 2006" = "1986/2006", 
  "February - Mach 1984 - 2006" = "1984/2006",
  "January 1984 - 2006" = "1984/2006",
  "January 2000 - December 2004" = "2000/2004",
  "January 2002 - December 2003" = "2002/2003",
  "July - August 1994" = "1994-07/08",
  "July - August 2007" = "2007-07/08",
  "July - August 2008" = "2008-07/08",
  "July - August 2010 " = "2010-07/08",
  "July - September 2010" = "2020-07/09",
  "July 1984 - 2006" = "1984/2006",
  "July 2007" = "2007-07",
  "June 1984 - 2006" = "1984/2006", 
  "June 2013 - April 2014" = "2013-06/2014-04",
  "March - August 1984 - 2006" = "1984/2006",
  "March 2002 - July 2008" = "2002/2008",
  "May - December 1995-2003" = "1995/2003",
  "May - July 1984 - 2006" = "1984/2006",
  "May - July 1986 - 2006" = "1986/2006",
  "May - November 1986 - 2006" = "1986/2006",
  "May - October 1984 - 2006" = "1984/2006",
  "May 1984 - 2006" = "1984/2006",
  "May 2010" = "2010-05",
  "November 2000" = "2000-11",
  "October 2006 - February 2007" = "2006-10/2007-02",
  "September 2010" = "2010-09",
  "Spring 1998" = "1998-03/06",
  "Spring 1999" = "1999-03/06",
  "Spring 2004" = "2004-03/06",
  "Summer 1994" = "1994-06/09",
  "Summer 1999" = "1999-06/09",
  "Summer 2000" = "2000-06/09",
  "Summer 2002" = "2002-06/09",
  "Summer 2009" = "2009-06/09",
  "Winter - Spring 1984 - 2006" = "1984-2006",
  "2008-2009" = "2008/2009",
  "July - August 2010" = "2010-07/08"
)

table(hab$verbatimEventDate[is.na(hab$eventDate)])

# generate

occurrence <- generateOccurrence(hab)
mof <- generateMof(hab)

# handle added columns

occurrence$country <- df_country
occurrence$dynamicProperties <- paste0("syndrome=", df_added$SYNDROME)

write.table(occurrence, paste0("../output/", folder, "/occurrence.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)
write.table(mof, paste0("../output/", folder, "/measurementorfact.txt"), quote=FALSE, sep="\t", na="", row.names=FALSE)

