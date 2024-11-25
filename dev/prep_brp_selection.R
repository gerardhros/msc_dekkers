# extract soil property input data for the selection water regions

# require packages
require(data.table)
require(sf)

# location of the bln data
loc.bln <- 'D:/DATA/18 bln/'

# location of the water regions selected by Tessa
s1 <- st_read('../grouped_polygons.gpkg')

# load in the BLN database field locations
d1 <- readRDS(paste0(loc.bln,'brp21_s1sel.rds'))

# load in the BLN input database
d1.bln.input <- readRDS(paste0(loc.bln,'brp21_bln_before_scoring.rds'))

# load in the BLN database for 10-years field history

# select only the relevant fields
d2 <- st_intersection(s1,d1)

# select the unique fields located in the water regions
u_ref_id <- unique(d2$ref_id)

# select the original field ids
d2 <- d1[d1$ref_id %in% u_ref_id,]

# select the input BLN file
d3 <- d1.bln.input[id %in% d2$id]

# save file
saveRDS(d3,'../brp_selected.rds')
saveRDS(d2,'../brp_selected_sf.rds')
