
# require packages
require(data.table)
require(sf)

# provincies NL
#prov <- st_read(paste0(Sys.getenv('NMI_DATA'),'topo/provincies/2018-Imergis_provinciegrenzen_kustlijn.shp'))
prov <- st_read('provinciegrenzen/2018-Imergis_provinciegrenzen_kustlijn.shp')

# location of the water regions selected by Tessa
s1 <- st_read('grouped_polygons.gpkg')

# convert water regions from MULTYPOLOGYON TO POLYGON
s1.impr <- st_cast(s1,'POLYGON')
s1.impr$area <- round(as.numeric(st_area(s1.impr)/10000))
s1.impr <- s1.impr[s1.impr$area>100,]
s1.impr <- as.data.table(s1.impr)
s1.impr[,groep := 1:.N,by='grouped_name']
s1.impr[,grouped_name := paste0(grouped_name,groep)]
s1.impr <- st_as_sf(s1.impr)

# spatial features object for agricultural fields
s1.fields <- readRDS('Field data/brp_selected_sf.rds')

# spatial features dataset
s1.fields.data <- readRDS('Field data/brp_selected.rds')

# for now i select only one year per field (later this will be the BBWP score or mean inputs)
s1.fields.data <- s1.fields.data[,.SD[1],by='id']

# water quality measurement points for nitrogen
d1.wq.n <- readRDS('sw_nitrogen_mp.rds')


# make water quality points spatial from the multi-annual averaged N concentration
d1.wq.n <- d1.wq.n[,list(Ntot = median(Ntot)),by=c('mtp_id','mtp_x','mtp_y','season')]

# make een analyse alleen voor summer
d1.wq.n.sum <- d1.wq.n[season=='summer']
d1.wq.n.win <- d1.wq.n[season == 'winter']

# make the object spatial
s1.sw.n.sum <- st_as_sf(d1.wq.n.sum,coords = c('mtp_x','mtp_y'),crs = 28992)
s1.sw.n.win <- st_as_sf(d1.wq.n.win, coords = c('mtp_x', 'mtp_y'), crs = 28992)

# filter the spatial object using the polygons for regions
s1.sw.n.sum <- st_intersection(s1.sw.n.sum,s1.impr)
s1.sw.n.win <- st_intersection(s1.sw.n.win,s1.impr)

# inladen waterlichamen en select only those in water regions
# s1.water <- st_read(paste0(Sys.getenv('NMI_DATA'),'topo/top10NL/TOP10NL_gpkg/top10nl_Waterdeel.gpkg'),layer = 'top10nl_waterdeel_lijn')
s1.water <- st_read('top1nl_waterdeel_selectie.gpkg')
s1.water <- st_intersection(s1.water,s1.impr)
# plot(st_geometry(s1.water))

# for Tessa only: make subsets per region
regions <- unique(s1.impr$grouped_name)#[2]
# plot(st_geometry(regions))

# fields_regions <- st_intersection(s1.fields, regions)
# make a for loop to do this for all regions separately (given RAM limitations of TD laptop)

  # output object to store the results
  out.list <- list()
  
  # do the data coupling per regions
  for(i in regions){
    
    # subset all datasets for the specific region
    s2.region <- s1.impr[s1.impr$grouped_name == i,]
    s2.water <- s1.water[s1.water$grouped_name == i,]
    s2.fields <- st_intersection(s1.fields,s1.impr[s1.impr$grouped_name == i,])
    s2.sw.n <- s1.sw.n.sum[s1.sw.n.sum$grouped_name == i,]
    
    # make data.table (used for merging later)
    d2.sw.n <- as.data.table(s2.sw.n)
    
    # add a buffer of 100m around the ditches
    s2.water.buf <- st_buffer(s2.water,dist=20)
    
    # add a buffer of 5 km around measurement points
    s2.sw.n.buf <- st_buffer(s2.sw.n,dist=5000)
    
    # select only fields that are within 100m distance of a ditch and within the 5 km zone
    s3.fields.sw <- st_join(s2.fields,s2.water.buf,join= st_touches)
    s3.fields.sw <- st_join(s3.fields.sw, s2.sw.n.buf,join = st_within)
    
    # select only the measurement points within distance of a ditch
    s3.sw.n <- st_join(s2.sw.n,s2.water.buf,join= st_touches)
    
    # calculate minimum distance between selected fields and measurement points
    test <-  round(st_distance(s3.fields.sw, s3.sw.n),1)
    test <- as.data.table(test)
    setnames(test,s3.sw.n$mtp_id)
    test[,fieldid := s3.fields.sw$id]
    test <- melt(test,id.vars='fieldid',variable.name = 'mtp_id',value.name = 'DTM')
    
    # save output in a list
    out.list[[i]] <- copy(test)
    
    saveRDS(out.list, "out_list_sw_winter.rds")
    
    # print i
    print(i)
  }
  


#s2.water.buf <- st_union(s2.water.buf)
#s2.water.buf <- st_cast(s2.water.buf,'MULTYPOLYGON')


peil <- st_read("peilbesluitgebied.gpkg")
plot(st_geometry(peil))

# toevoegen van meetgegevens
# waterdelen duingebied zuid:
for (i in out.list[1:3]) {
  out <- merge(i,s1.fields.data,all.x=TRUE,by.x='fieldid',by.y='id')
  out <- merge(i,d2.sw.n[,.(mtp_id,regio = grouped_name,Ntot)],all.x=TRUE,by = 'mtp_id')
  out <- unique(i, by = "fieldid")
  print(i)
}

merged_list <- list()

# Loop over the regions in out.list (9 & 20 have no data point, 12, 13, 19 too big/crashing)
for (region_name in names(out.list[24])) {
  i <- out.list[[region_name]]  # Get the data.table for the current region
  
  # Check if required columns exist
  if (!"fieldid" %in% colnames(i)) stop(paste("Column 'fieldid' missing in region:", region_name))
  if (!"id" %in% colnames(s1.fields.data)) stop("Column 'id' missing in s1.fields.data.")
  
  # Merge with s1.fields.data
  out <- merge(i, s1.fields.data, all.x = TRUE, by.x = 'fieldid', by.y = 'id')
  
  # Merge with d2.sw.n
  out <- merge(out, d2.sw.n[, .(mtp_id, regio = grouped_name, Ntot)], all.x = TRUE, by = 'mtp_id')
  
  # Keep unique rows based on 'fieldid'
  out <- unique(out, by = "fieldid")
  
  # Store the result in the merged_list with the region name as the key
  merged_list[[region_name]] <- out
  
  # check result
  print(region_name)
  print(names(merged_list))  
}

# Check the resulting list
print(names(merged_list))  # Shows the names of regions in the list

# save results
# Combine all regions into a single data.table
combined <- rbindlist(merged_list, idcol = "region")

# Save the combined data to a single CSV
fwrite(combined, "merged_sw_n_winter.csv")
fwrite(all_regions_combined, "merged_sw_n_summer.csv")

# read
merged_sw_n_winter <- fread("merged_sw_n_winter.csv")
merged_sw_n_summer <- fread("all_regions_combined.csv")


all_regions_combined <- fread("all_regions_combined.csv")


# for (region_name in names(merged_list)) {
#   fwrite(merged_list[[region_name]], paste0(region_name, "_merged.csv"))
# }
# 
# out <- merge(out,s1.fields.data,all.x=TRUE,by.x='fieldid',by.y='id')
# out <- merge(out,d2.sw.n[,.(mtp_id,regio = grouped_name,Ntot)],all.x=TRUE,by = 'mtp_id')
# out <- unique(out, by = "fieldid")
# Duingebied_Zuid <- merge(out, dt.p, by = "mtp_id")
# 
# for (i in out.list) {
#   assign(paste0("df_", region_name), as.data.frame(out[[region_name]]))
# }
# merged_out <- lapply(out, function(out.list) {
#   merge(out.list, s1.fields.data, by.x = "fieldid", by.y = "id", all.x = TRUE)
# })

# require(OBIC)
# crops.obic <- OBIC::crops.obic
# crops.obic[1,]
# d1[1]
# OBIC::calc_rotation_fraction(d1$ref_id,d1$B_LU_BRP,crop='cereals')
# ?OBIC::calc_rotation_fraction
# ?calc_rotation_fraction
# 
# crops.obic[1,]
# brp_selected[1]
# 
# plot(brp_selected_sf$geom)

# plot for testing during the process
require(ggplot2)
require(patchwork)
p1 <- ggplot() + 
  geom_sf(data=s2.water,color='blue',fill=NA)


for (i in regions[1:6]) {
  
  filename_current <- paste0("products/",i,".png")
  title_current <- paste0("Locations in ", i)
  print(filename_current)
  print(title_current)
  
  # subset all datasets for the specific region
  s2.region <- s1.impr[s1.impr$grouped_name == i,]
  s2.water <- s1.water[s1.water$grouped_name == i,]
  s2.fields <- st_intersection(s1.fields,s1.impr[s1.impr$grouped_name == i,])
  s2.sw.n <- s1.sw.n.sum[s1.sw.n.sum$grouped_name == i,]
  
  p1 <- ggplot() +
    geom_sf(data = s2.water, color = 'blue', fill = 'blue', alpha = 0.6) +
    geom_sf(data = s2.fields, color = 'gray75', fill = NA, alpha = 0.5) +
    geom_sf(data=s2.sw.n, color='red',size=2.5) +
    geom_sf(data=s2.region, color='black',fill=NA)  +
    theme_bw() + ggtitle(title_current)
  
  ggsave(plot = p1, filename = filename_current, width = 13,height = 10)

}
  





p1 <- ggplot() + 
      geom_sf(data=s1.water,color='blue',fill=NA)  + 
      #geom_sf(data=s2.water.buf,color='green',fill=NA)  + 
      geom_sf(data = s3.fields.sw,color='gray75',fill='NA',alpha=0.3)+
      geom_sf(data=s3.sw.n,color='red',size=5) + 
      #geom_sf(data=s2.sw.n.buf,color='orange',fill=NA,size=2) + 
      geom_sf(data=s2.region,color='black',fill=NA)  + 
      theme_bw() + ggtitle('waterbodies')
ggsave(plot = p1,filename = 'products/waterbodies_all_regions.png',width = 13,height = 10)

p2 <- p1 + p1

p1 <- ggplot() + 
  geom_sf(data=s2.water,color='blue',fill=NA)  + 
  geom_sf(data=s2.water.buf,color='green',fill=NA)  + 
  geom_sf(data = s3.fields.sw,color='gray75',fill='NA',alpha=0.3)+
  geom_sf(data=s3.sw.n,color='red',size=5) + 
  #geom_sf(data=s2.sw.n.buf,color='orange',fill=NA,size=2) + 
  geom_sf(data=s2.region,color='black',fill=NA)  + 
  theme_bw() + ggtitle('locations in Twente(3)')
ggsave(plot = p1,filename = 'products/waterbodies_all_regions.png',width = 13,height = 10)



p1 <- ggplot() + 
  geom_sf(data=s2.water,color='blue',fill=NA)  + 
  geom_sf(data=s2.water.buf,color='green',fill=NA)  + 
  geom_sf(data = s3.fields.sw,color='gray75',fill='gray75',alpha=0.3)+
  geom_sf(data=s3.sw.n,color='red',size=5) + 
  #geom_sf(data=s2.sw.n.buf,color='orange',fill=NA,size=2) + 
  geom_sf(data=s2.region,color='black',fill=NA)  + 
  theme_bw() + ggtitle('locations in Twente(3)')
ggsave(plot = p1,filename = 'products/waterbodies_all_regions.png',width = 13,height = 10)

dt.n.merged <- fread("dt_n_sw_merged.csv")

f2(merge(f1, ))