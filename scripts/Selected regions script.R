### This script is created to spatially join the representative regions and the water quality data points

# Load required packages
require(sf)
require(dplyr)

# Read the spatial layers with polygon regions 
wur_regions <- st_read("ws_wur_selection_waterlichaamgebieden (1).gpkg")
agv_regions <- st_read("ws_agv_selection (1).gpkg")

## Make columns ready before merging the layers
# Make column headings lowercase
colnames(wur_regions) <- tolower(colnames(wur_regions))
colnames(agv_regions) <- tolower(colnames(agv_regions))

# Give same columns same headers
wur_regions <- wur_regions %>% rename(name = wlgname)
agv_regions <- agv_regions %>% rename(name = naam, id = objectid)

# Find all headings
all_columns <- union(names(wur_regions), names(agv_regions))

# Set missing heading to NA (skip geometry column to prevent errors)
for (col in setdiff(all_columns, names(wur_regions))) {
  if (col != "geom") {
    wur_regions[[col]] <- NA
  }
}

for (col in setdiff(all_columns, names(agv_regions))) {
  if (col != "geom") {
    agv_regions[[col]] <- NA
  }
}

# Add geometry column 
wur_regions <- wur_regions[, c(setdiff(all_columns, "geom"), "geom")]
agv_regions <- agv_regions[, c(setdiff(all_columns, "geom"), "geom")]

# merge the 2 polygon layers
merged_layer <- rbind(wur_regions, agv_regions)

# save result as geopackage
st_write(merged_layer, "merged_layer.gpkg")


## Group small polygons together based on their names
grouped_polygons <- merged_layer %>%
  mutate(grouped_name = case_when(
    name %in% c("Muyeveld, Loosdrechtsche Plassen", "Polder de Rondehoep", "Polder Zevenhoven", "Aetsveldse Polder Oost, bemalen", "Bovenkerkerpolder, Landelijk", "Polder Groot Mijdrecht  en Polder de Eerste Bedijking (oost)") ~ "Amstelland",
    name %in% c("Bolscherbeek", "Hagmolenbeek", "Twenthekanalen", "Azelerbeek", "Boven Regge", "Poelsbeek") ~ "Twente",
    name %in% c("Oploosche Molenbeek, Oeffeltsche Raam ea", "Sambeekse Uitwatering + Lactariabeek", "St Jansbeek", "Loobeek en Molenbeek", "Oostrumsche Beek", "Groote Molenbeek") ~ "Noord Limburg",
    TRUE ~ name                                         
  )) %>%
  group_by(grouped_name) %>%
  summarise(geom = st_union(geom))  

# Save result as geopackage
st_write(grouped_polygons, "grouped_polygons.gpkg")

## Merge the coordinates with the selected regions
# Check crs
st_crs(grouped_polygons) == st_crs(lsw)
st_crs(grouped_polygons) == st_crs(lgw)

# Perform spatial join/intersection to select data points within the regions
swpoints_in_regions <- st_join(lsw, grouped_polygons, join = st_within)
gwpoints_in_regions <- st_join(lgw, grouped_polygons, join = st_within)

sw_in_regions <- st_intersection(lsw, grouped_polygons)
gw_in_regions <- st_intersection(lgw, grouped_polygons)

# save results as geopackage
st_write(sw_in_regions, "swcoords_filtered.gpkg", delete_dsn = TRUE)
st_write(gw_in_regions, "gwcoords_filtered.gpkg", delete_dsn = TRUE)

# count number of points
sw_counts <- sw_in_regions %>%
  group_by(grouped_name) %>%  
  summarise(count = n())
View(sw_counts)

gw_counts <- gw_in_regions %>%
  group_by(grouped_name) %>%
  summarise(count = n())
View(gw_counts)

# Read province polygons


### Make plots for visualization

# Plot the grouped polygon regions
plot(grouped_polygons)

## Plot the selected measurement points (within the regions)
# Surface water points
ggplot(data = sw_in_regions) +
  geom_sf(size = 1.5, color = "darkcyan", fill = "darkcyan") +
  theme_bw() + ggtitle("Selected Surface Water Measurements") 

# Groundwater points
ggplot(data = gw_in_regions) +
  geom_sf(size = 1.5, color = "darkcyan", fill = "darkcyan") +
  theme_bw() + ggtitle("Selected Groundwater Measurements")


## Plot selected measurement points with the grouped polygon regions
# Surface water points
ggplot() +
  geom_sf(data = grouped_polygons, color = "black") +
  geom_sf(data = sw_in_regions, color = "darkcyan", , fill = "darkcyan",  size = 1) +
  theme_bw() + ggtitle("Selected Surface Water Measurement Points")

# Ground water points
ggplot() +
  geom_sf(data = grouped_polygons, color = "black") +
  geom_sf(data = gw_in_regions, color = "darkcyan", , fill = "darkcyan",  size = 1) +
  theme_bw() + ggtitle("Selected Groundwater Measurement Points")

# # Notes
# ggplot() + geom_sf(data = merged_regions, aes(fill = NA, color = "pink"))
# 
# waterlichaam <- 1 # dit is dan een polygoon van een waterlichaam
# require(ggplot2)
# 
# ggplot() + geom_sf(data=s1,aes(fill=value,color = value))  + theme_bw()  
# 
# geom_sf(data = regions, aes(color='black',fill=NA))
# 
# plot(st_geometry(s1))
# 
# 
# ggplot() + geom_sf(data = lsw, aes(fill = TRUE, color = "pink")) + theme_bw()
# ggplot() + geom_sf(data=lgw, aes(fill = TRUE, color = "blue"))  + theme_bw() 
# 
# ggplot() + geom_sf(data = merged_regions, aes(color='black',fill=NA))
# 
# plot(st_geometry(gw.regions))
# 
# plot(st_geometry(merged_regions))
