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

# # merge XY coordinate into chemistry data table
# dt.gw <- merge(dt.chemie.gw, gw.loc, by = c("mtp_id"))
# dt.gw$mtp_naam.y <- NULL
# setnames(dt.gw, c("mtp_naam.x"), "mtp_naam")
# View(dt.gw)
# 
# # temp # check number of unique ID (with valid xy coordinate) per year
# dt.gw[, lapply(.SD, function(x) length(unique(x))), .SDcols = "mtp_id", by = year(mtp_datum)]



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
  mtp_tijd.index <- which(colnames(this.file) %in% c("mwa_mwadtijdb", "mwa_mwatijdb", "begintijd"))
  
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
dt.n[, maand := month(mtp_datum)]
dt.n[, jaar := year(mtp_datum)]
View(dt.n)

# Ntot median per month over all years
dt.n.monthly <- dt.n[, .(Ntot.median = median(Ntot, na.rm = TRUE)), by = .(jaar, maand)]

# Create a date column for plotting
dt.n.monthly[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]

require(ggplot2)

# Plot the time series for monthly median Ntot
ggplot(dt.n.monthly, aes(x = date, y = Ntot.median)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly Median Total Nitrogen", x = "Date", y = "Median Ntot (mg/L)") + 
  theme_minimal() + theme(text = element_text(size=20))


# Ntot sd per month over all years
dt.n.sd.monthly <- dt.n[, .(Ntot.sd = sd(Ntot, na.rm = TRUE)), by = .(jaar, maand)]

# Create a date column for plotting
dt.n.sd.monthly[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]

# Plot the time series for montly sd Ntot
ggplot(dt.n.sd.monthly, aes(x = date, y = Ntot.sd)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly Sd Total Nitrogen", x = "Date", y = "Sd Ntot (mg/L)") + 
  theme_minimal() + theme(text = element_text(size=20))





# calculate median value per month per year
dt.n.m.y.median <- dt.n[, .(Ntot = median(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand, jaar)]
# calculate monthly median
dt.n.m.median <- dt.n[, .(Ntot = median(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand)]
# calculate summer median (march - september)
dt.n.s.median <- dt.n[maand >= 3 & maand <= 9, .(Ntot.median = median(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
dt.n.s.sd <- dt.n[maand >= 3 & maand <= 9, .(Ntot.sd = sd(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
dt.n.s.n <- dt.n[!is.na(Ntot) & maand >= 3 & maand <= 9, .(Ntot.n = .N), by = list(mtp_id, mtp_x, mtp_y)]

dt.n <- merge(dt.n.s.median, dt.n.s.sd, by = c("mtp_id", "mtp_x", "mtp_y"))
dt.n <- merge(dt.n, dt.n.s.n, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T)
dt.n[is.na(Ntot.n), Ntot.n := 0]
dt.n <- dt.n[!is.na(Ntot.median),]


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


dt.p <- dcast(dt.p, mtp_id + mtp_x + mtp_y + mtp_datum ~ mtp_parameter, value.var = "mtp_waarde", fun.aggregate = median)
dt.p <- dt.p[mtp_x != "" & mtp_x != 0]
dt.p[, maand := month(mtp_datum)]
dt.p[, jaar := year(mtp_datum)]
View(dt.p)

# Ptot median per month over all years
dt.p.monthly <- dt.p[, .(Ptotmedian = median(Ptot, na.rm = TRUE)), by = .(jaar, maand)]

# Create a date column for plotting
dt.p.monthly[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]


# Plot the time series for monthly median Ptot
ggplot(dt.p.monthly, aes(x = date, y = Ptotmedian)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly Median Total Phosphorous", x = "Date", y = "Median Ptot (mg/L)") + 
  theme_minimal() + theme(text = element_text(size=20))

# Plot the time series for monthly median PO4
dt.PO4 <- dt.p[, .(PO4median =  median(PO4, na.rm = TRUE)), by = .(jaar, maand)]
dt.PO4[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]
ggplot(dt.PO4, aes(x = date, y = PO4median)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly Median Phosphate", x = "Date", y = "Median PO4 (mg/L)") + 
  theme_minimal() + theme(text = element_text(size=20))

# Plot the time series for sd median PO4
dt.PO4.sd <- dt.p[, .(PO4sd =  sd(PO4, na.rm = TRUE)), by = .(jaar, maand)]


dt.PO4.sd[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]
ggplot(dt.PO4.sd, aes(x = date, y = PO4sd)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly Sd Phosphate", x = "Date", y = "Sd PO4 (mg/L)") + 
  theme_minimal() + theme(text = element_text(size=20))

# Ntot sd per month over all years
dt.p.sd.monthly <- dt.p[, .(Ptot.sd = sd(Ptot, na.rm = TRUE)), by = .(jaar, maand)]

# Create a date column for plotting
dt.p.sd.monthly[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]

# Plot the time series for montly sd Ntot
ggplot(dt.p.sd.monthly, aes(x = date, y = Ptot.sd)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly Sd Total Phosphorous", x = "Date", y = "Sd Ptot (mg/L)") + 
  theme_minimal() + theme(text = element_text(size=20))



#Groundwater timeseries


nitrogen_ts_gw <- ts(dt.n.monthly$Ntot.median, start = c(2010, 1), frequency = 12)

nitrogen_diff <- diff(nitrogen_ts, lag = 12)

# Plot the differenced data
plot(nitrogen_diff, main = "Differenced Monthly Nitrogen Levels",
     xlab = "Year", ylab = "Differenced Nitrogen (mg/L)")


# Plot the time series for montly Ntot
nitrogen_ts <- ts(dt.n.monthly$Ntot.median, start = c(2010, 1), frequency = 12)

nitrogen_diff <- diff(nitrogen_ts, lag = 12)

# Plot the differenced data
plot(nitrogen_diff, main = "Differenced Monthly Nitrogen Levels",
     xlab = "Year", ylab = "Differenced Nitrogen (mg/L)")

# calculate average PO4 value per month per year
dt.p <- dt.p[, .(PO4 = mean(PO4, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand, jaar)]
# calculate monthly average PO4
dt.p <- dt.p[, .(PO4 = mean(PO4, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand)]
# calculate summer average PO4 (march - september)
dt.p.median <- dt.p[maand >= 3 & maand <= 9, .(PO4.median = median(PO4, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
dt.p.sd <- dt.p[maand >= 3 & maand <= 9, .(PO4.sd = sd(PO4, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
dt.p.n <- dt.p[!is.na(PO4) & maand >= 3 & maand <= 9, .(PO4.n = .N), by = list(mtp_id, mtp_x, mtp_y)]
dt.p <- merge(dt.p.median, dt.p.sd, by = c("mtp_id", "mtp_x", "mtp_y"))
dt.p <- merge(dt.p, dt.p.n, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T)
dt.p[is.na(PO4.n), PO4.n := 0]

# calculate average Ptot value per month per year
dt.p2 <- dt.p2[, .(Ptot = median(Ptot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand, jaar)]
# calculate monthly average Ptot
dt.p2 <- dt.p2[, .(Ptot = median(Ptot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y, maand)]
# calculate summer average Ptot (march - september)
dt.p2.median <- dt.p2[maand >= 3 & maand <= 9, .(Ptot.median = median(Ptot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
dt.p2.sd <- dt.p2[maand >= 3 & maand <= 9, .(Ptot.sd = sd(Ptot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
dt.p2.n <- dt.p2[!is.na(Ptot) & maand >= 3 & maand <= 9, .(Ptot.n = .N), by = list(mtp_id, mtp_x, mtp_y)]

dt.p <- merge(dt.p, dt.p2.median, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T, all.y = T)
dt.p <- merge(dt.p, dt.p2.sd, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T, all.y = T)
dt.p <- merge(dt.p, dt.p2.n, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T)
dt.p[is.na(Ptot.n), Ptot.n := 0]

dt.p <- dt.p[!is.na(PO4.median) | !is.na(Ptot.median),]


##### Aggregate water quality data --------------------------------

### nitrogen ----------------------

# calculate winter median (October - February)
dt.n.wintermedian <- dt.n[maand >= 10 & maand <= 2, .(Ntot.median = median(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
dt.n.wintersd <- dt.n[maand >= 10 & maand <= 2, .(Ntot.sd = sd(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]
dt.n.wintern <- dt.n[!is.na(Ntot) & maand >= 10 & maand <= 2, .(Ntot.n = .N), by = list(mtp_id, mtp_x, mtp_y)]
dt.n <- merge(dt.n.median, dt.n.sd, by = c("mtp_id", "mtp_x", "mtp_y"))
dt.n <- merge(dt.n, dt.n.n, by = c("mtp_id", "mtp_x", "mtp_y"), all.x = T)
dt.n[is.na(Ntot.n), Ntot.n := 0]
dt.n <- dt.n[!is.na(Ntot.median),]
dt.n.year <- dt.n[year(mtp_datum)]
dt.n.month <- dt.n[month(mtp_datum)]


dt.n.wintermedian <- dt.n[maand >= 10 & maand <= 2, .(Ntot.median = median(Ntot, na.rm = TRUE)), by = .(jaar, maand)]

# Create a date column for plotting
dt.n.wintermedian[, date := as.Date(paste(jaar, maand, "01", sep = "-"))]

# Plot the time series for montly sd Ntot
ggplot(dt.n.wintermedian, aes(x = date, y = dt.n.wintermedian)) +
  geom_line() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Monthly Sd Total Phosphorous", x = "Date", y = "Sd Ptot (mg/L)") + 
  theme_minimal() + theme(text = element_text(size=20))

# Calculate winter sd 
# dt.n.sd <- dt.n[maand >= 12 & maand <= 2, .(Ntot.sd = sd(Ntot, na.rm = TRUE)), by = list(mtp_id, mtp_x, mtp_y)]

# Calculate winter n event > Q3


## Calculate summer median (June-August)

# Calculate summer sd

# Calculate summer n events > Q3


### Phosphorous ----------------------

## calculate winter median (December - February)

# Calculate winter sd 

# Calculate winter n event > Q3


## Calculate summer median (June-August)

# Calculate summer sd

# Calculate summer n events > Q3




# ## Map water quality measurements
# library(sf)
# library(tmap)
# 
# ## SF
# pointSF <- sf:: st_as_sf(dt, coords = c("mtp_x", "mtp_y"), crs = 4326)
# class(pointSF)
# tmap::qtm(pointSF)
# View(pointSF)
# 
# #write out the file
# sf::st_write(pointSF, paste0(baseDir, "/outputs/Surfacewatercoords.shp"))
# head(dt)


require(sf)
require(dplyr)

# Convert data to sf object (assuming coordinates are in columns "mtp_x.index" and "mtp_y.index")
loc.gw_clean <- loc.gw[!is.na(mtp_x) & !is.na(mtp_y)]

subset_loc.gw <- loc.gw_clean
subset_loc.gw <- subset_loc.gw[,.(mtp_x,mtp_y,mtp_naam)]
subset_loc.gw <- subset_loc.gw[!duplicated(mtp_x)]
subset_loc.gw[,value := rnorm(.N,mean=10,sd=2.5)]

# Convert to sf object
points_sf <- st_as_sf(subset_loc.gw, coords = c("mtp_x", "mtp_y"), crs = 28992)


View(subset_loc.gw)
# # Convert to SF object -------------------
# sf.n <- st_as_sf(dt.n, coords = c("mtp_x", "mtp_y"), crs = 28992)
# sf.p <- st_as_sf(dt.p, coords = c("mtp_x", "mtp_y"), crs = 28992)
# st_write(sf.n, "C:/Users/Tldek/Documents/MES/MSc Thesis/Outputs/Interpolation/Nitrogen_winter.gpkg")
# st_write(sf.p, "C:/Users/Tldek/Documents/MES/MSc Thesis/Outputs/Phosphorous_winter.gpkg")
# 
# # Create rasters for interesting values -----------------------------------
# st.mask <- st_as_stars(mask)
# st.mask <- st_warp(st.mask, cellsize = 250, crs = 28992)
# 
# idw.n.median <- gstat::idw(Ntot.median ~ 1, sf.n, newdata = st.mask, idp = 2.0, nmax = 10, debug.level = -1)
# write_stars(idw.n.median, paste0(onedrive, "project/NMI_bodemschat/products/wk_interpolation/wk_ntot_median.tif"))
# 
# idw.n.sd <- gstat::idw(Ntot.sd ~ 1, sf.n, newdata = st.mask, idp = 2.0, nmax = 10, debug.level = -1)
# write_stars(idw.n.sd, paste0(onedrive, "project/NMI_bodemschat/products/wk_interpolation/wk_ntot_sd.tif"))
# 
# idw.p.median <- gstat::idw(PO4.median ~ 1, sf.p, newdata = st.mask, idp = 2.0, nmax = 10, debug.level = -1)
# write_stars(idw.p.median, paste0(onedrive, "project/NMI_bodemschat/products/wk_interpolation/wk_po4_median.tif"))
# 
# idw.p.sd <- gstat::idw(PO4.sd ~ 1, sf.p, newdata = st.mask, idp = 2.0, nmax = 10, debug.level = -1)
# write_stars(idw.p.sd, paste0(onedrive, "project/NMI_bodemschat/products/wk_interpolation/wk_po4_sd.tif"))
# 
# #}
# 
# addWaterKwaliteit <- function (fields, onedrive) {
#   
#   # Drop irrelevant columns for this function
#   fields <- fields[, "id"]
#   
#   # Create centroids of fields
#   points <- st_centroid(fields, of_largest_polygon = TRUE)
#   
#   # Setup table to list results
#   dt.wk <- data.table(id = fields$id)
#   
#   # Extract Ntot.median and insert into table
#   r.wk.ntot.median <- raster(paste0(onedrive, "project/NMI_Bodemschat/products/wk_interpolation/wk_ntot_median.tif"))
#   this.wk.ntot.median <- extract(r.wk.ntot.median , points, df = TRUE)
#   setnames(this.wk.ntot.median , c("ID", "wk_ntot_median"), c("id", "wk.ntot.median"))
#   dt.wk <- merge(dt.wk, this.wk.ntot.median, by = "id", all.x = TRUE)
#   
#   # Extract Ntot.sd and insert into table
#   r.wk.ntot.sd <- raster(paste0(onedrive, "project/NMI_Bodemschat/products/wk_interpolation/wk_ntot_sd.tif"))
#   this.wk.ntot.sd <- extract(r.wk.ntot.sd , points, df = TRUE)
#   setnames(this.wk.ntot.sd , c("ID", "wk_ntot_sd"), c("id", "wk.ntot.sd"))
#   dt.wk <- merge(dt.wk, this.wk.ntot.sd, by = "id", all.x = TRUE)
#   
#   # Extract po4.median and insert into table
#   r.wk.po4.median <- raster(paste0(onedrive, "project/NMI_Bodemschat/products/wk_interpolation/wk_po4_median.tif"))
#   this.wk.po4.median <- extract(r.wk.po4.median , points, df = TRUE)
#   setnames(this.wk.po4.median , c("ID", "wk_po4_median"), c("id", "wk.po4.median"))
#   dt.wk <- merge(dt.wk, this.wk.po4.median, by = "id", all.x = TRUE)
#   
#   # Extract po4.sd and insert into table
#   r.wk.po4.sd <- raster(paste0(onedrive, "project/NMI_Bodemschat/products/wk_interpolation/wk_po4_sd.tif"))
#   this.wk.po4.sd <- extract(r.wk.po4.sd , points, df = TRUE)
#   setnames(this.wk.po4.sd , c("ID", "wk_po4_sd"), c("id", "wk.po4.sd"))
#   dt.wk <- merge(dt.wk, this.wk.po4.sd, by = "id", all.x = TRUE)
#   
#   # Replace outliers
#   dt.wk[wk.ntot.sd > 15, wk.ntot.sd := 15]
#   dt.wk[wk.po4.sd > 4, wk.po4.sd := 4]
#   
#   # Merge waterkwaliteit data to the fields
#   fields <- merge(fields, dt.wk, by = "id", all.x = TRUE)
#   
#   return(fields)
# }