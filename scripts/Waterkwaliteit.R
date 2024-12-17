# Re-arrange surface water quality data of waterkwaliteitportaal.nl
# the original script was made by Kees (and Sven?)(Github NMI-Bodemschat\preprocessing\ppr_waterkwaliteit.R)

# The script was adjusted so that new data of water quality (2016-2019) can be properly read.
# The median values of Ntot and Ptot (for all points in NL) is saved as:  paste0(onedrive, "project/NMI_bodemschat/products/wk_interpolation/mtp_n_2010-2019.gpkg"))

# Major changes made by YF are:
# 1. Chemistry data of 2017-2019 and location data of 2016-2019 are added. 
# 2. median value of summer (march - september) is calculated, instead of whole year
# 3. In original script, a point was removed when they have only 1 record. In this version, a point was removed only when they have 0 record.
# 4. In this version, extreme outliers (<1Q-3*IQR or >3Q+3*IQR of box-cox-transformed values) were exluded from the calculation of median 

# Clean memory
rm(list = ls() )

getwd()

# Load packages
require(gstat); require(data.table); require(sf); require(readxl); require(MASS)

## Make a data table of location and chemical data --------------

## Grondwaterkwaliteit locaties. Every location file has another header, so make a new dt with consistent headers


# Read the Excel file
loc.gw <- read_excel("meetlocatiebestand_WKP_koppeling_meetgegevens_20181207-corr20210519_aanvulling_provincies.xlsx", sheet = 1)

# Convert to data.table
loc.gw <- as.data.table(loc.gw)
View(loc.gw)

# Rename columns
colnames(loc.gw) <- tolower(colnames(loc.gw))
setnames(loc.gw, old = c("nitgnummer + filter", "krwlocatienaam", "x", "y"),
         new = c("mtp_naam", "mtp_id", "mtp_x", "mtp_y"))

gw.loc <- data.table(
  mtp_naam = loc.gw[["mtp_naam"]],
  mtp_id   = loc.gw[["mtp_id"]],
  mtp_x    = loc.gw[["mtp_x"]],
  mtp_y    = loc.gw[["mtp_y"]]
)

View(gw.loc)

# remove characters from coordinate, and convert to numeric
gw.loc[, mtp_x := as.numeric((gsub("[^0-9\\.]", "", mtp_x)))]
gw.loc[, mtp_y := as.numeric(gsub("[^0-9\\.]", "", mtp_y))]

# remove records for which x- and/or y-coordinate is empty or 0
gw.loc <- gw.loc[!is.na(mtp_x) & mtp_x > 0 & mtp_x != "",]
gw.loc <- gw.loc[!is.na(mtp_y) & mtp_y > 0 & mtp_y != "",]

# remove records which largely exceeds the RD range of NL
gw.loc <- gw.loc[mtp_x > 10000 & mtp_x < 300000, ]
gw.loc <- gw.loc[mtp_y > 300000 & mtp_y < 650000, ]

gw.loc$mtp_id <- gsub("'", '', gw.loc$mtp_id)
gw.loc$mtp_id <- gsub("\"", '', gw.loc$mtp_id)
gw.loc <- unique(gw.loc, by = "mtp_id")
View(gw.loc)

# Oppervlaktewater locaties. Every location file has another header, so make a new dt with consistent headers
loc.files <- list.files(path = "Water quality data/Meetlocaties/", pattern = ".csv$", full.names = TRUE)
locs <- list()
for (file in loc.files) {
  
  this.file <- fread(file, header = TRUE)
  colnames(this.file) <- tolower(colnames(this.file))
  
  # Find the indices for each column if they exist
  mtp_naam.index <- which(colnames(this.file) %in% c("mpn_mpnomsch", "mpnomsch", "meetpunt.omschrijving", "station_longname", "meetpuntomschrijving", "omschrijving"))
  mtp_id.index <- which(colnames(this.file) %in% c("mpn_mpnident", "mpnident", "meetpunt.identificatie", "station_no",  "identificatie", "meetobjectcode"))
  mtp_x.index <- which(colnames(this.file) %in% c("xcoor", "mrfxcoor", "mpn_mrfxcoor", "geometriepunt.x_rd", "x_rd", "xcoordinate", "carteasting"))
  mtp_y.index <- which(colnames(this.file) %in% c("ycoor", "mrfycoor", "mpn_ycoor", "geometriepunt.y_rd", "y_rd", "ycoordinate", "cartnorthing"))
  
  
  # Assign columns or NA if not found
  new.file <- data.table(
    mtp_naam = if (length(mtp_naam.index) > 0) this.file[[mtp_naam.index]] else NA,
    mtp_id   = if (length(mtp_id.index) > 0) this.file[[mtp_id.index]] else NA,
    mtp_x    = if (length(mtp_x.index) > 0) this.file[[mtp_x.index]] else NA,
    mtp_y    = if (length(mtp_y.index) > 0) this.file[[mtp_y.index]] else NA
  )
  
  # Add the result to the list
  locs[[file]] <- new.file
  
  # remove characters from coordinate, and convert to numeric
  new.file[, mtp_x := as.numeric((gsub("[^0-9\\.]", "", mtp_x)))]
  new.file[, mtp_y := as.numeric(gsub("[^0-9\\.]", "", mtp_y))]
  
  # remove records for which x- and/or y-coordinate is empty or 0
  new.file <- new.file[!is.na(mtp_x) & mtp_x > 0 & mtp_x != "",]
  new.file <- new.file[!is.na(mtp_y) & mtp_y > 0 & mtp_y != "",]
  
  # remove records which largely exceeds the RD range of NL
  new.file <- new.file[mtp_x > 10000 & mtp_x < 300000, ]
  new.file <- new.file[mtp_y > 300000 & mtp_y < 650000, ]
  
  locs[[file]] <- new.file
}

# Combine the years into a dt
dt.loc <- rbindlist(locs)
dt.loc$mtp_id <- gsub("'", '', dt.loc$mtp_id)
dt.loc$mtp_id <- gsub("\"", '', dt.loc$mtp_id)
dt.loc <- unique(dt.loc, by = "mtp_id")

# Transform coordinates to sf
sw.loc <- copy(dt.loc)

lgw <- st_as_sf(gw.loc, coords = c("mtp_x", "mtp_y"), crs = 28992)
lsw <- st_as_sf(sw.loc, coords = c("mtp_x", "mtp_y"), crs = 28992)

st_write(lgw, "gw.coords.gpkg")
st_write(lsw, "sw.coords.gpkg")

fwrite(dt.loc, file= "sw.coordinates.csv")
fwrite(gw.loc, file = "gw.coordinates.csv")

View(dt.loc)

# Grondwater chemie. Every chemie file has another header, so make new dt with consistent headers

chemie.files.gw <- list.files(path = "Water quality data/Grondwater", pattern = '.csv$', full.names = TRUE)
chemie.gw <- list()
for (file in chemie.files.gw) {
  
  this.file <- fread(file, fill = TRUE)
  
  colnames(this.file) <- tolower(colnames(this.file))
  
  mtp_naam.index <- which(colnames(this.file) %in% c("meetobject.namespace", "namespace")) [1]
  mtp_id.index <- which(colnames(this.file) %in% c("meetpunt.lokaalid", "monsteridentificatie")) [1]
  mtp_compartiment.index  <- which(colnames(this.file) %in% c("analysecompartiment.omschrijving", "compartimentcode")) [1]
  mtp_parameter.index <- which(colnames(this.file) %in% c("parameter.omschrijving", "parametercode")) [1]
  mtp_waarde.index <- which(colnames(this.file) %in% c("numeriekewaarde")) [1]
  mtp_eenheid.index <- which(colnames(this.file) %in% c("eenheid.code", "eenheidcode")) [1]
  mtp_datum.index <- which(colnames(this.file) %in% c("begindatum")) [1]
  mtp_tijd.index <- which(colnames(this.file) %in% c("begintijd")) [1]
  
  
  new.file <- data.table(mtp_naam = this.file[, ..mtp_naam.index],
                         mtp_compartiment = this.file[, ..mtp_compartiment.index],
                         mtp_id = this.file[, ..mtp_id.index],
                         mtp_parameter = this.file[, ..mtp_parameter.index],
                         mtp_waarde = this.file[, ..mtp_waarde.index],
                         mtp_eenheid = this.file[, ..mtp_eenheid.index],
                         mtp_datum = this.file[, ..mtp_datum.index],
                         mtp_tijd = this.file[, ..mtp_tijd.index])
  setnames(new.file,
           colnames(new.file),
           c("mtp_naam", "mtp_compartiment", "mtp_id", "mtp_parameter", "mtp_waarde", "mtp_eenheid", "mtp_datum", "mtp_tijd"))
  
  chemie.gw[[file]] <- new.file
  
}

dt.chemie.gw <- rbindlist(chemie.gw, fill = TRUE)
# dt.chemie.gw <- dt.chemie.gw[grep("opp|Opp", mtp_compartiment)][, mtp_compartiment := NULL]
dt.chemie.gw$mtp_id <- gsub("'", '', dt.chemie.gw$mtp_id)
dt.chemie.gw$mtp_id <- gsub("\"", '', dt.chemie.gw$mtp_id)
dt.chemie.gw$mtp_datum <- as.Date(dt.chemie.gw$mtp_datum)
View(dt.chemie.gw)

# merge XY coordinate into chemistry data table
dt.gw <- merge(dt.chemie.gw, gw.loc, by.x = "mtp_id", by.y = "mtp_naam")
dt.gw$mtp_naam.y <- NULL
setnames(dt.gw, c("mtp_naam.x"), "mtp_naam")
View(dt.gw)

# temp # check number of unique ID (with valid xy coordinate) per year
dt.gw[, lapply(.SD, function(x) length(unique(x))), .SDcols = "mtp_id", by = year(mtp_datum)]



# Oppervlaktewater chemie. Every chemie file has another header, so make new dt with consistent headers
chemie.files <- list.files(path = "Water quality data/Oppervlaktewater", pattern = '.csv$', full.names = TRUE)
chemie <- list()
for (file in chemie.files) {
  
  if(grepl("2014-1.csv", file)){
    # The R session crushes when reading this file with fread. 
    this.file <- as.data.table(read.csv(file, sep=";"))
    this.file[, Begindatum := as.IDate(Begindatum)]
  } else {
    
    this.file <- fread(file, fill = TRUE)
    
  }
  
  colnames(this.file) <- tolower(colnames(this.file))
  
  mtp_naam.index <- which(colnames(this.file) %in% c("mpn_mpnomsch", "meetpunt.omschrijving", "meetobject.lokaalid"))
  mtp_id.index <- which(colnames(this.file) %in% c("mpn_mpnident", "meetpunt.identificatie", "meetobject.lokaalid"))
  mtp_compartiment.index  <- which(colnames(this.file) %in% c("mco_domomsch", "compartiment.omschrijving", "analysecompartiment.omschrijving"))[1]
  mtp_parameter.index <- which(colnames(this.file) %in% c("mps_domomsch", "parameter.omschrijving"))
  mtp_waarde.index <- which(colnames(this.file) %in% c("mwa_mwawrden", "numeriekewaarde"))
  mtp_eenheid.index <- which(colnames(this.file) %in% c("mep_domgwcod", "eenheid.code"))
  mtp_datum.index <- which(colnames(this.file) %in% c("mwa_mwadtmb", "begindatum"))
  #mtp_tijd.index <- which(colnames(this.file) %in% c("mwa_mwadtijdb", "mwa_mwatijdb", "begintijd"))
  
  new.file <- data.table(mtp_naam = this.file[, ..mtp_naam.index],
                         mtp_compartiment = this.file[, ..mtp_compartiment.index],
                         mtp_id = this.file[, ..mtp_id.index],
                         mtp_parameter = this.file[, ..mtp_parameter.index],
                         mtp_waarde = this.file[, ..mtp_waarde.index],
                         mtp_eenheid = this.file[, ..mtp_eenheid.index],
                         mtp_datum = this.file[, ..mtp_datum.index])
                         #mtp_tijd = this.file[, ..mtp_tijd.index])
  setnames(new.file,
           colnames(new.file),
           c("mtp_naam", "mtp_compartiment", "mtp_id", "mtp_parameter", "mtp_waarde", "mtp_eenheid", "mtp_datum"))
  
  chemie[[file]] <- new.file
  
}

dt.chemie <- rbindlist(chemie, fill = TRUE)
dt.chemie <- dt.chemie[grep("opp|Opp", mtp_compartiment)][, mtp_compartiment := NULL]
dt.chemie$mtp_id <- gsub("'", '', dt.chemie$mtp_id)
dt.chemie$mtp_id <- gsub("\"", '', dt.chemie$mtp_id)
dt.chemie$mtp_datum <- as.Date(dt.chemie$mtp_datum)

# merge XY coordinate into chemistry data table
dt <- merge(dt.chemie, dt.loc, by = c("mtp_id"))
dt$mtp_naam.y <- NULL
setnames(dt, c("mtp_naam.x"), "mtp_naam")

# temp # check number of unique ID (with valid xy coordinate) per year
dt[, lapply(.SD, function(x) length(unique(x))), .SDcols = "mtp_id", by = year(mtp_datum)]




# Pre-processing data table for N -------------------
poi.n <- c("stikstof Kjeldahl", "stikstof", "nitriet", "nitraat","ammonium", "stikstof totaal",
           "NKj", "NH3", "Ntot", "NO3", "NH4", "NO2")
dt.n <- dt[mtp_parameter %in% poi.n & mtp_eenheid == "mg/l"]
dt.n[, mtp_waarde := as.numeric(mtp_waarde)]
dt.n[mtp_parameter == "stikstof Kjeldahl", mtp_parameter := "NKj"]
dt.n[mtp_parameter == "stikstof", mtp_parameter := "Ntot"]
dt.n[mtp_parameter == "nitriet", mtp_parameter := "NO2"]
dt.n[mtp_parameter == "nitraat", mtp_parameter := "NO3"]
dt.n[mtp_parameter == "ammonium", mtp_parameter := "NH4"]
dt.n[mtp_parameter == "stikstof totaal", mtp_parameter := "Ntot"]
dt.n <- unique(dt.n, by = c("mtp_id", "mtp_parameter", "mtp_datum"))

#'Compute lamda for box-cox transformation by likelihood maximization
#'
#'@import MASS
calc_lamda <- function(value){
  # exclude 0, because it works only for positive response variables
  value <- value[value > 0]
  bc <- MASS::boxcox(value ~ 1,
                     plotit = F,
                     lambda = seq(-1, 2, 1/100)) # same range as the default values of forecast::BoxCox.lambda
  lambda <- bc$x[which.max(bc$y)]
  return(lambda)
}


#'box-cox transformation
boxcoxTrans <- function(x, lambda) {  
  y  <- fifelse(lambda == 0,  log(x), ((x^lambda) - 1) / lambda)
  return(y)
}

## Remove outlier (outside Q1 – 3*IQR and Q3 + 3*IQR, after box-cox transformation)
# remove <0 values
dt.n <- dt.n[mtp_waarde>=0,]
# box-cox transformation
dt.n[, lamda := calc_lamda(mtp_waarde), by = mtp_parameter]
dt.n[, mtp_waarde_bc := boxcoxTrans(mtp_waarde, lamda)]
dt.n[, Q1_bc := quantile(mtp_waarde_bc, probs = 0.25, na.rm=T),by = mtp_parameter]
dt.n[, Q3_bc := quantile(mtp_waarde_bc, probs = 0.75, na.rm=T), by = mtp_parameter]
dt.n[, IQR_bc := Q3_bc - Q1_bc]
dt.n[, ol_up_bc := Q3_bc + 3*IQR_bc]
dt.n[, ol_low_bc := Q1_bc - 3*IQR_bc]
# Exclude extreme outlier
dt.n <- dt.n[(mtp_waarde_bc > ol_low_bc & mtp_waarde_bc < ol_up_bc)|mtp_waarde == 0, ]

# temp 
dt.n_ori <- copy(dt.n)

dt.n <- dcast(dt.n, mtp_id + mtp_x + mtp_y + mtp_datum ~ mtp_parameter, value.var = "mtp_waarde", fun.aggregate = median)
dt.n <- dt.n[mtp_x != "" & mtp_x != 0]
dt.n[is.na(Ntot), Ntot := NKj + NO2 + NO3]
dt.n[, Ntot := NKj + NO2 + NO3]
dt.n[, maand := month(mtp_datum)]
dt.n[, jaar := year(mtp_datum)]
View(dt.n)


dt.n <- dt.n[jaar >= 2000 & jaar <= 2019]

unique(dt.p$jaar)

# Calculate median, sd, n, and n >3Q for NH4, NKj, NO2, NO3, and Ntot per month per year (force all rows in merge with all.x = TRUE)
dt.NH4.median <- dt.n[, .(NH4_median =  median(NH4, na.rm = TRUE)), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NH4.median, by = c("maand", "jaar"), all.x = TRUE)
dt.NH4.sd <- dt.n[, .(NH4sd =  sd(NH4, na.rm = TRUE)), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NH4.sd, by = c("maand", "jaar"), all.x = TRUE)
dt.NH4.n <- dt.n[!is.na(NH4), .(NH4.n = .N), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NH4.n, by = c("maand", "jaar"), all.x = TRUE)

dt.NKj.median <- dt.n[, .(NKjmedian =  median(NKj, na.rm = TRUE)), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NKj.median, by = c("maand", "jaar"), all.x = TRUE)
dt.NKj.sd <- dt.n[, .(NKjsd =  sd(NKj, na.rm = TRUE)), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NKj.sd, by = c("maand", "jaar"), all.x = TRUE)
dt.NKj.n <- dt.n[!is.na(NKj), .(NKj.n = .N), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NKj.n, by = c("maand", "jaar"), all.x = TRUE)

dt.NO2.median <- dt.n[, .(NO2median =  median(NO2, na.rm = TRUE)), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NO2.median, by = c("maand", "jaar"), all.x = TRUE)
dt.NO2.sd <- dt.n[, .(NO2sd =  sd(NO2, na.rm = TRUE)), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NO2.sd, by = c("maand", "jaar"), all.x = TRUE)
dt.NO2.n <- dt.n[!is.na(NO2), .(NO2.n = .N), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NO2.n, by = c("maand", "jaar"), all.x = TRUE)

dt.NO3.median <- dt.n[, .(NO3median =  median(NO3, na.rm = TRUE)), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NO3.median, by = c("maand", "jaar"), all.x = TRUE)
dt.NO3.sd <- dt.n[, .(NO3sd =  sd(NO3, na.rm = TRUE)), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NO3.sd, by = c("maand", "jaar"), all.x = TRUE)
dt.NO3.n <- dt.n[!is.na(NO3), .(NO3.n = .N), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.NO3.n, by = c("maand", "jaar"), all.x = TRUE)

dt.Ntot.median <- dt.n[, .(Ntotmedian = median(Ntot, na.rm = TRUE)), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.Ntot.median, by = c("maand", "jaar"), all.x = TRUE)
dt.Ntot.sd <- dt.n[, .(Ntot.sd = sd(Ntot, na.rm = TRUE)), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.Ntot.sd, by = c("maand", "jaar"), all.x = TRUE)
dt.Ntot.n <- dt.n[!is.na(Ntot), .(Ntot.n = .N), by = .(jaar, maand)]
dt.n <- merge(dt.n, dt.Ntot.n, by = c("maand", "jaar"), all.x = TRUE)

unique(dt.n$jaar)

# Calculate the 3rd quantile for PO4 and Ptot per month and year, and add count columns
dt.n[, `:=`(
  NH4_3rd_quantile = quantile(NH4, 0.75, na.rm = TRUE),
  NKj_3rd_quantile = quantile(NKj, 0.75, na.rm = TRUE),
  NO2_3rd_quantile = quantile(NO2, 0.75, na.rm = TRUE),
  NO3_3rd_quantile = quantile(NO3, 0.75, na.rm = TRUE),
  Ntot_3rd_quantile = quantile(Ntot, 0.75, na.rm = TRUE)
), by = .(jaar, maand)]

dt.n[, `:=`(
  NH4_exceeds = sum(NH4 > NH4_3rd_quantile, na.rm = TRUE),
  NKj_exceeds = sum(NKj > NKj_3rd_quantile, na.rm = TRUE),
  NO2_exceeds = sum(NO2 > NO2_3rd_quantile, na.rm = TRUE),
  NO3_exceeds = sum(NO3 > NO3_3rd_quantile, na.rm = TRUE),
  Ntot_exceeds = sum(Ntot > Ntot_3rd_quantile, na.rm = TRUE)
), by = .(jaar, maand)]

unique(dt.p$jaar)

dt.n[, NH4_3rd_quantile := NULL]
dt.n[, NKj_3rd_quantile := NULL]
dt.n[, NO2_3rd_quantile := NULL]
dt.n[, NO3_3rd_quantile := NULL]
dt.n[, Ntot_3rd_quantile := NULL]


# Pre-processing data table for P -------------------
poi.p <- c("orthofosfaat", "totaal fosfaat", "fosfaat", "fosfor totaal", "Ptot", "PO4")

dt.p <- dt[mtp_parameter %in% poi.p & mtp_eenheid %in% c("mg/l", "ug/l")]
dt.p[, mtp_waarde := as.numeric(mtp_waarde)]
dt.p[mtp_eenheid == "ug/l", mtp_waarde := mtp_waarde / 1000]
dt.p[mtp_eenheid == "ug/l", mtp_eenheid := "mg/l"]
dt.p[mtp_parameter == "totaal fosfaat", mtp_parameter := "PO4"]
dt.p[mtp_parameter == "fosfaat", mtp_parameter := "PO4"]
dt.p[mtp_parameter == "fosfor totaal", mtp_parameter := "Ptot"]
dt.p[mtp_parameter == "orthofosfaat", mtp_parameter := "PO4"]
dt.p <- unique(dt.p, by = c("mtp_id", "mtp_parameter", "mtp_datum"))

## Remove outlier (outside Q1 – 3*IQR and Q3 + 3*IQR, after box-cox transformation)
# remove <0 values
dt.p <- dt.p[mtp_waarde>=0,]
# box-cox transformation
dt.p[, lamda := calc_lamda(mtp_waarde), by = mtp_parameter]
dt.p[, mtp_waarde_bc := boxcoxTrans(mtp_waarde, lamda)]
dt.p[, Q1_bc := quantile(mtp_waarde_bc, probs = 0.25, na.rm=T),by = mtp_parameter]
dt.p[, Q3_bc := quantile(mtp_waarde_bc, probs = 0.75, na.rm=T), by = mtp_parameter]
dt.p[, IQR_bc := Q3_bc - Q1_bc]
dt.p[, ol_up_bc := Q3_bc + 3*IQR_bc]
dt.p[, ol_low_bc := Q1_bc - 3*IQR_bc]
# Exclude extreme outlier
dt.p <- dt.p[(mtp_waarde_bc > ol_low_bc & mtp_waarde_bc < ol_up_bc)|mtp_waarde == 0, ]

# exclude very high values (1e+12 and 1e+09) of PO4 (which was not excluded with the outlier exclusion above, as they are quite many (N=99))
dt.p <- dt.p[!(mtp_parameter == "PO4" & mtp_waarde > 1000000), ]

# temp 
dt.p_ori <- copy(dt.p)
gw.p <- copy(dt.p_ori)
 
dt.p <- gw.p

dt.p <- dcast(gw.p, mtp_id + mtp_x + mtp_y + mtp_datum ~ mtp_parameter, value.var = "mtp_waarde", fun.aggregate = median)
dt.p <- dt.p[mtp_x != "" & mtp_x != 0]
dt.p[, maand := month(mtp_datum)]
dt.p[, jaar := year(mtp_datum)]
View(dt.p)

dt.p <- dt.p[jaar >= 2000 & jaar <= 2019]

unique(dt.p$jaar)

# Ptot & PO4 median per month per year, merge by month and year
dt.Ptot.median <- dt.p[, .(Ptotmedian = median(Ptot, na.rm = TRUE)), by = .(jaar, maand)]
dt.p <- merge(dt.p, dt.Ptot.median, by = c("maand", "jaar"), all.x = TRUE)
dt.Ptot.sd <- dt.p[, .(Ptot.sd = sd(Ptot, na.rm = TRUE)), by = .(jaar, maand)]
dt.p <- merge(dt.p, dt.Ptot.sd, by = c("maand", "jaar"), all.x = TRUE)
dt.Ptot.n <- dt.p[!is.na(Ptot), .(Ptot.n = .N), by = .(jaar, maand)]
dt.p <- merge(dt.p, dt.Ptot.n, by = c("maand", "jaar"), all.x = TRUE)
dt.PO4.median <- dt.p[, .(PO4median =  median(PO4, na.rm = TRUE)), by = .(jaar, maand)]
dt.p <- merge(dt.p, dt.PO4.median, by = c("maand", "jaar"), all.x = TRUE)
dt.PO4.sd <- dt.p[, .(PO4sd =  sd(PO4, na.rm = TRUE)), by = .(jaar, maand)]
dt.p <- merge(dt.p, dt.PO4.sd, by = c("maand", "jaar"), all.x = TRUE)
dt.PO4.n <- dt.p[!is.na(PO4), .(PO4.n = .N), by = .(jaar, maand)]
dt.p <- merge(dt.p, dt.PO4.n, by = c("maand", "jaar"), all.x = TRUE)
unique(dt.p$jaar)
View(dt.Ptot.median)

# Calculate the 3rd quantile for PO4 and Ptot per month and year, and add count columns
dt.p[, `:=`(
  PO4_3rd_quantile = quantile(PO4, 0.75, na.rm = TRUE),
  Ptot_3rd_quantile = quantile(Ptot, 0.75, na.rm = TRUE)
), by = .(jaar, maand)]

dt.p[, `:=`(
  PO4_exceeds = sum(PO4 > PO4_3rd_quantile, na.rm = TRUE),
  Ptot_exceeds = sum(Ptot > Ptot_3rd_quantile, na.rm = TRUE)
), by = .(jaar, maand)]
unique(dt.p$jaar)

dt.p[, PO4_3rd_quantile := NULL]
dt.p[, Ptot_3rd_quantile := NULL]

# Merge dt.n and dt.p into final groundwater dataset
final.dt.gw <- merge(dt.n, dt.p, by = c("mtp_id", "mtp_x", "mtp_y", "mtp_datum", "maand", "jaar"), all.x = TRUE)
final.dt.sw <- merge(dt.n, dt.p, by = c("mtp_id", "mtp_x", "mtp_y", "mtp_datum", "maand", "jaar"), all.x = TRUE)

#Identify the season
final.dt.gw[, season := fifelse(maand %in% c(10, 11, 12, 1, 2), "winter", "summer")]
View(final.dt.gw)
final.dt.sw[, season := fifelse(maand %in% c(10, 11, 12, 1, 2), "winter", "summer")]
View(final.dt.sw)

# Save final datasets:
saveRDS(final.dt.gw, "final_dt_gw.rds")
final.chemie.gw <- readRDS("final_dt_gw.rds")

saveRDS(final.dt.sw, "final_dt_sw.rds")
final.chemie.sw <- readRDS("final_dt_sw.rds")


### Notes for calculations and plotting:
#
#
# # Ntot median per month over all years
# dt.n.monthly <- dt.n[, .(Ntot.median = median(Ntot, na.rm = TRUE)), by = .(jaar, maand)]
# 
# # Create a date column for plotting
# dt.n.monthly[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]
# 
# require(ggplot2)
# 
# # Plot the time series for monthly median Ntot
# ggplot(dt.n.monthly, aes(x = date, y = Ntot.median)) +
#   geom_line() + 
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(title = "Monthly Median Total Nitrogen", x = "Date", y = "Median Ntot (mg/L)") + 
#   theme_minimal() + theme(text = element_text(size=20))
# 
# 
# # Ntot sd per month over all years
# dt.n.sd.monthly <- dt.n[, .(Ntot.sd = sd(Ntot, na.rm = TRUE)), by = .(jaar, maand)]
# 
# # Create a date column for plotting
# dt.n.sd.monthly[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]
# 
# # Plot the time series for montly sd Ntot
# ggplot(dt.n.sd.monthly, aes(x = date, y = Ntot.sd)) +
#   geom_line() + 
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(title = "Monthly Sd Total Nitrogen", x = "Date", y = "Sd Ntot (mg/L)") + 
#   theme_minimal() + theme(text = element_text(size=20))
# 
# # calculate median value per month per year
# dt.n.m.y.median <- dt.n[, .(Ntot = median(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand, jaar)]
# # calculate monthly median
# dt.n.m.median <- dt.n[, .(Ntot = median(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand)]
# # calculate summer median (march - september)
# dt.n.s.median <- dt.n[maand >= 3 & maand <= 9, .(Ntot.median = median(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
# dt.n.s.sd <- dt.n[maand >= 3 & maand <= 9, .(Ntot.sd = sd(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
# dt.n.s.n <- dt.n[!is.na(Ntot) & maand >= 3 & maand <= 9, .(Ntot.n = .N), by = list(mtp_id, mtp_x, mtp_y)]
# 
# dt.new <- merge(dt.n.s.median, dt.n.s.sd, by = c("mtp_id", "mtp_x", "mtp_y"))
# dt.new <- merge(dt.new, dt.n.s.n, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T)
# dt.new[is.na(Ntot.n), Ntot.n := 0]
# dt.new <- dt.new[!is.na(Ntot.median),]

# # Create a date column for plotting
# dt.p.monthly[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]
# 
# 
# # Plot the time series for monthly median Ptot
# ggplot(dt.p.monthly, aes(x = date, y = Ptotmedian)) +
#   geom_line() + 
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(title = "Monthly Median Total Phosphorous", x = "Date", y = "Median Ptot (mg/L)") + 
#   theme_minimal() + theme(text = element_text(size=20))

# Plot the time series for monthly median PO4
# dt.PO4 <- dt.p[, .(PO4median =  median(PO4, na.rm = TRUE)), by = .(jaar, maand)]
# dt.PO4[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]
# ggplot(dt.PO4, aes(x = date, y = PO4median)) +
#   geom_line() + 
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(title = "Monthly Median Phosphate", x = "Date", y = "Median PO4 (mg/L)") + 
#   theme_minimal() + theme(text = element_text(size=20))

# Plot the time series for sd median PO4
# dt.PO4.sd <- dt.p[, .(PO4sd =  sd(PO4, na.rm = TRUE)), by = .(jaar, maand)]
# 
# 
# dt.PO4.sd[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]
# ggplot(dt.PO4.sd, aes(x = date, y = PO4sd)) +
#   geom_line() + 
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(title = "Monthly Sd Phosphate", x = "Date", y = "Sd PO4 (mg/L)") + 
#   theme_minimal() + theme(text = element_text(size=20))

# Ntot sd per month over all years
# dt.p.sd.monthly <- dt.p[, .(Ptot.sd = sd(Ptot, na.rm = TRUE)), by = .(jaar, maand)]
# 
# # Create a date column for plotting
# dt.p.sd.monthly[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]
# 
# # Plot the time series for montly sd Ntot
# ggplot(dt.p.sd.monthly, aes(x = date, y = Ptot.sd)) +
#   geom_line() + 
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(title = "Monthly Sd Total Phosphorous", x = "Date", y = "Sd Ptot (mg/L)") + 
#   theme_minimal() + theme(text = element_text(size=20))

#Groundwater timeseries

# 
# nitrogen_ts_gw <- ts(dt.n.monthly$Ntot.median, start = c(2010, 1), frequency = 12)
# 
# nitrogen_diff <- diff(nitrogen_ts, lag = 12)
# 
# # Plot the differenced data
# plot(nitrogen_diff, main = "Differenced Monthly Nitrogen Levels",
#      xlab = "Year", ylab = "Differenced Nitrogen (mg/L)")
# 
# 
# # Plot the time series for montly Ntot
# nitrogen_ts <- ts(dt.n.monthly$Ntot.median, start = c(2010, 1), frequency = 12)
# 
# nitrogen_diff <- diff(nitrogen_ts, lag = 12)
# 
# # Plot the differenced data
# plot(nitrogen_diff, main = "Differenced Monthly Nitrogen Levels",
#      xlab = "Year", ylab = "Differenced Nitrogen (mg/L)")

# calculate average PO4 value per month per year
# dt.p <- dt.p[, .(PO4 = mean(PO4, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand, jaar)]
# # calculate monthly average PO4
# dt.p <- dt.p[, .(PO4 = mean(PO4, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand)]
# calculate summer average PO4 (march - september)
# dt.p.median <- dt.p[maand >= 3 & maand <= 9, .(PO4.median = median(PO4, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
# dt.p.sd <- dt.p[maand >= 3 & maand <= 9, .(PO4.sd = sd(PO4, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
# dt.p.n <- dt.p[!is.na(PO4) & maand >= 3 & maand <= 9, .(PO4.n = .N), by = list(mtp_id, mtp_x, mtp_y)]
# dt.p.values<- merge(dt.p.median, dt.p.sd, by = c("mtp_id", "mtp_x", "mtp_y"))
# dt.pp <- merge(dt.p, dt.p.values, by = c("mtp_id", "mtp_x", "mtp_y"))
# dt.p <- merge(dt.pp, dt.p.n, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T)
# dt.p[is.na(PO4.n), PO4.n := 0]

# calculate average Ptot value per month per year
# dt.p2 <- copy(dt.p)
# dt.p2 <- dt.p2[, .(Ptot = median(Ptot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand, jaar)]
# # calculate monthly average Ptot
# dt.p2 <- dt.p2[, .(Ptot = median(Ptot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand)]
# # calculate summer average Ptot (march - september)
# dt.p2.median <- dt.p2[maand >= 3 & maand <= 9, .(Ptot.median = median(Ptot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
# dt.p2.sd <- dt.p2[maand >= 3 & maand <= 9, .(Ptot.sd = sd(Ptot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
# dt.p2.n <- dt.p2[!is.na(Ptot) & maand >= 3 & maand <= 9, .(Ptot.n = .N), by = list(mtp_id, mtp_x, mtp_y)]
# 
# dt.p <- merge(dt.p, dt.p2.median, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T, all.y = T)
# dt.p <- merge(dt.p, dt.p2.sd, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T, all.y = T)
# dt.p <- merge(dt.p, dt.p2.n, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T)
# dt.p[is.na(Ptot.n), Ptot.n := 0]
# 
# dt.p <- dt.p[!is.na(PO4.median) | !is.na(Ptot.median),]


##### Aggregate water quality data --------------------------------

### nitrogen ----------------------

# # calculate montly median, sd, n
# dt.n.median <- dt.n.gw[, .(Ntot.median = median(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand)]
# dt.p2[, .(Ptot = median(Ptot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand)]
# merge(dt.n, dt.n.gw, by = c("mtp_id", "mtp_x", "mtp_y"))
# dt.n.sd <- dt.n.gw[, .(Ntot.sd = sd(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand)]
# dt.n.n <- dt.n.gw[!is.na(Ntot), .(Ntot.n = .N), by = list(mtp_id, mtp_x, mtp_y, maand)]
# dt.n <- merge(dt.n.median, dt.n.sd, by = c("mtp_id", "mtp_x", "mtp_y"))
# dt.n <- merge(dt.n.median, dt.n.sd, by = c("mtp_id", "mtp_x", "mtp_y"), allow.cartesian = TRUE)
# dt.n <- merge(dt.n, dt.n.n, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T , allow.cartesian = TRUE)
# dt.n[is.na(Ntot.n), Ntot.n := 0]
# dt.n <- dt.n[!is.na(Ntot.median),]
# dt.n.year <- dt.n[year(mtp_datum)]
# dt.n.month <- dt.n[month(mtp_datum)]
# 
# 
# dt.n.wintermedian <- dt.n[maand >= 10 & maand <= 2, .(Ntot.median = median(Ntot, na.rm = TRUE)), by = .(jaar, maand)]
# 
# # Create a date column for plotting
# dt.n.wintermedian[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]
# 
# # Plot the time series for montly sd Ntot
# ggplot(dt.n.wintermedian, aes(x = date, y = dt.n.wintermedian)) +
#   geom_line() + 
#   scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#   labs(title = "Monthly Sd Total Phosphorous", x = "Date", y = "Sd Ptot (mg/L)") + 
#   theme_minimal() + theme(text = element_text(size=20))

