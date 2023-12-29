library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
library(terra)
library(dplyr)
library(grDevices)

#----------------------------------------------------------------------------------------------------------------------------------------
#1. Power Plant Map
#Load shp and plot geometry
wd = "C:/Users/Lenovo/Documents/Assessment Spatial"
indo <- read_sf(paste0(wd,"/idn_admbnda_adm0_bps_20200401.shp"))
indo_province <- read_sf(paste0(wd,"/idn_admbnda_adm1_bps_20200401.shp"))

#Load power plant locations and filter missing longitude and latitude values
indo_plant <- readxl::read_excel("C:/Users/Lenovo/Documents/Assessment Spatial/BENV0093_2ndAssignment_powerplants.xlsx", sheet = "Sheet1") %>%
  na.omit()

#Convert longitude and langitude values to points
indo_plant_point = st_as_sf(indo_plant, coords = c("longitude", "latitude"), crs = st_crs(4326))

#Recategorize capacity size range
indo_plant_point$size_range <- cut(indo_plant_point$capacity_mw, breaks = c(0, 10, 100, 1000, 9000), labels = FALSE)

#Transform type and status column to factor
indo_plant_point$type <- as.factor(indo_plant_point$type)
indo_plant_point$status <- as.factor(indo_plant_point$status)

#Create the map
transparent_theme <- theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill='transparent', colour=NA),
  plot.background = element_rect(fill='transparent', colour=NA),
  axis.line = element_line(colour = "black"))

ggplot()+
  geom_sf(data=indo_province, fill='lightgray')+
  geom_sf(data=indo_plant_point, aes(fill = type, size=size_range, shape=status))+
  labs(x = "Longitude", y = "Latitude", fill = "Type", size = "Capacity", shape = "Status")+
  scale_fill_manual(values=c("#6f006f","blue","orange","cyan"))+
  scale_shape_manual(labels = c("Construction", "Existing", "Planned"), values=c(21,22,23))+
  scale_size_continuous(labels = c("0-10 MW", "10-100 MW", "100-1000 MW", "> 1000 MW"), range = c(2,6))+
  guides(shape=guide_legend(override.aes=list(size=6)), fill=guide_legend(override.aes=list(size=6,shape=21)))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#----------------------------------------------------------------------------------------------------------------------------------------

#2. Parameter maps
#2.1. Protected Areas
#Import and combine SHP files
protected0 <- read_sf(paste0(wd,"/Protected Area/WDPA_WDOECM_Apr2023_Public_IDN_shp/WDPA_WDOECM_Apr2023_Public_IDN_shp_0/WDPA_WDOECM_Apr2023_Public_IDN_shp-polygons.shp"))
protected1 <- read_sf(paste0(wd,"/Protected Area/WDPA_WDOECM_Apr2023_Public_IDN_shp/WDPA_WDOECM_Apr2023_Public_IDN_shp_1/WDPA_WDOECM_Apr2023_Public_IDN_shp-polygons.shp"))
protected2 <- read_sf(paste0(wd,"/Protected Area/WDPA_WDOECM_Apr2023_Public_IDN_shp/WDPA_WDOECM_Apr2023_Public_IDN_shp_2/WDPA_WDOECM_Apr2023_Public_IDN_shp-polygons.shp"))
protected <- rbind(protected0, protected1, protected2)
rm(protected0)
rm(protected1)
rm(protected2)

#Create map
ggplot()+
  geom_sf(data=indo_province, fill=NA)+
  geom_sf(data=protected, aes(fill = MARINE))+
  labs(x = "Longitude", y = "Latitude", fill = "Protected Area")+
  scale_fill_manual(values=c("darkgreen", "#9B870C", "darkblue"), labels = c("Terrestrial", "Coastal", "Marine"))+
  guides(fill=guide_legend(override.aes=list(size=6,shape=21)))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme


#2.2. Population
#Import data and resample
indo_population <- rast(paste0(wd, "/idn_bsgme_v0a_100m_2020.tif"))
indo_population_500 <- aggregate(indo_population,5) 

#Create data frame and plot
indo_population_500.df <- as.data.frame(indo_population_500,xy=TRUE) %>%
  mutate(built_yes = case_when(idn_bsgme_v0a_100m_2020 > 0 ~ 1, idn_bsgme_v0a_100m_2020 == 0 ~ 0))
indo_population_500.df$built_yes = as.factor(indo_population_500.df$built_yes)

ggplot()+
  geom_sf(data=indo_province, fill=NA)+
  geom_tile(data=subset(indo_population_500.df, built_yes == "1"), aes(x=x,y=y,fill=built_yes))+
  labs(x = "Longitude", y = "Latitude", fill = "Populated area (500 m)")+
  scale_fill_manual(values=c("darkgreen"), labels = "Populated area")+
  guides(fill=guide_legend(override.aes=list(size=6,shape=21)))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#2.3. Elevation and Slope
#Import data and calculate slope
indo_elevation <- rast(paste0(wd, "/Altitude/IDN_alt.vrt"))
indo_elevation <- mask(indo_elevation, indo)
indo_slope = terrain(indo_elevation,v='slope',unit='degrees')

#Create data frame and plot
indo_elevation.df <- as.data.frame(indo_elevation,xy=TRUE)
indo_slope.df <- as.data.frame(indo_slope,xy=TRUE)

ggplot()+
  geom_tile(data=indo_elevation.df, aes(x=x,y=y,fill=IDN_alt))+
  geom_sf(data=indo_province, fill=NA)+
  labs(x = "Longitude", y = "Latitude", fill = "Altitude (m)")+
  scale_fill_gradientn(colors=terrain.colors(10))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

ggplot()+
  geom_tile(data=indo_slope.df, aes(x=x,y=y,fill=slope))+
  geom_sf(data=indo_province, fill=NA)+
  labs(x = "Longitude", y = "Latitude", fill = "Slope (degs)")+
  scale_fill_gradientn(colors=terrain.colors(10))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#2.4. Land Cover
#Import file and extract Indonesia land cover
indo_land_cover <- read_sf(paste0(wd,"/Landcover 2017/Landcover 2017/idn_land_cover.shp"))
indo_land_cover <- na.omit(indo_land_cover)

#Create map
ggplot()+
  geom_sf(data=indo_land_cover, colour=NA,aes(fill = desc_en))+
  geom_sf(data=indo_province, fill=NA)+
  labs(x = "Longitude", y = "Latitude", fill = "Land Cover")+
  guides(fill=guide_legend(override.aes=list(size=6,shape=21)))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#2.5. Road
#Search features of interests
indo_road <- read_sf(paste0(wd,"/Roads/IDN_roads.shp")) %>%
  filter(F_CODE_DES == "Road")
indo_road <- st_transform(indo_road, st_crs(indo))

ggplot()+
  geom_sf(data=indo, fill=NA)+
  geom_sf(data=indo_road, aes(colour = F_CODE_DES), show.legend = "line")+
  labs(x = "Longitude", y = "Latitude", colour = "Road Network")+
  scale_colour_discrete(labels = "Road")+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#--------------------------------------------------------------------------------------------------
#3. Constraint mapping
#3.1. Protected area
#Min. 2 km distance from protected areas (https://www.mdpi.com/1996-1073/9/8/643)
protected_2km <- st_buffer(protected, 2000)
protected_2km <- mutate(protected_2km, suitability = 0)

ggplot()+
  geom_sf(data=indo_province, aes(fill="ADM0_EN"))+
  geom_sf(data=protected_2km, aes(fill="Suitability"))+
  labs(x = "Longitude", y = "Latitude", fill = "Suitability")+
  scale_fill_manual(values = c("green","darkred"), labels = c("Suitable","Unsuitable"))+
  guides(fill=guide_legend(override.aes=list(size=6,shape=21)))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#3.2. Population
#500 m distance from populated area (https://gll.urk.edu.pl/zasoby/74/GLL-4-4-2017.pdf)
ggplot()+
  geom_raster(data=indo_population_500.df, aes(x=x,y=y,fill=built_yes))+
  scale_fill_manual(values = c("green","darkred"), labels = c("Suitable","Unsuitable"))+
  geom_sf(data=indo_province, fill=NA)+
  labs(x = "Longitude", y = "Latitude", fill = "Suitability")+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#3.3. Elevation and Slope
#Max. 2220 m above sea level, slope of 5 degrees (https://www.mdpi.com/1996-1073/9/8/643, https://www.sciencedirect.com/science/article/pii/S030626191731437X)
#Elevation matrix and reclassification
indo_elevation_m<- matrix(c(-30, 2200,  1,
                        2200, 4650, 0), ncol=3, byrow=TRUE) 
indo_elevation_rc <- classify(indo_elevation, indo_elevation_m, include.lowest=TRUE)
indo_elevation_rc.df <- as.data.frame(indo_elevation_rc,xy=TRUE)
indo_elevation_rc.df$IDN_alt <- as.factor(indo_elevation_rc.df$IDN_alt)

ggplot()+
  geom_tile(data=indo_elevation_rc.df, aes(x=x,y=y,fill=IDN_alt))+
  scale_fill_manual(values = c("darkred","green"), labels = c("Unsuitable","Suitable"))+
  geom_sf(data=indo_province, fill=NA)+
  labs(x = "Longitude", y = "Latitude", fill = "Suitability")+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#Slope matrix and reclassification
indo_slope_m<- matrix(c(0, 5,  1,
                   5, 46, 0), ncol=3, byrow=TRUE) 
indo_slope_rc <- classify(indo_slope, indo_slope_m, include.lowest=TRUE )
indo_slope_rc.df <- as.data.frame(indo_slope_rc,xy=TRUE)
indo_slope_rc.df$slope <- as.factor(indo_slope_rc.df$slope)

ggplot()+
  geom_tile(data=indo_slope_rc.df, aes(x=x,y=y,fill=slope))+
  scale_fill_manual(values = c("darkred","green"), labels = c("Unsuitable","Suitable"))+
  geom_sf(data=indo_province, fill=NA)+
  labs(x = "Longitude", y = "Latitude", fill = "Suitability")+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#3.4. Land Cover
#No built-up infrastructures, forestry, peatland, water bodies, wetlands, and agricultural land (https://d1wqtxts1xzle7.cloudfront.net/89398433/j.solener.2017.05.07520220808-1-1fdc4xu-libre.pdf?1659986416=&response-content-disposition=inline%3B+filename%3DA_GIS_based_Fuzzy_AHP_method_for_the_eva.pdf&Expires=1681732203&Signature=J9-9KDUuwjtBK4xJxSuQiVtx4pdQjjRpwtQPByL85r-VINiTEB9GeitT0Ay-1guFsXlGwXO1uR7xhj5N6EzBPslYgwQPuVl6kIdhVE~jrKy6hmBSrmCcPeFGoSYf3LiA4nZMqVoWlyvmxYWMlV479DyI7gms9yTjj68Y6NmECRG2oP-Z-P1LCWJsD2Lj5mXEEJo4kQJ~7sZDiKQOjVrdJAb13plvjKSACRa2-vq5GMJthKP4BKFkqkW55EuhADnekw1qaZ7AJSGUgiAoz8k60I84L7s9e9QHUlqOcbne61k4M326fcHZtHjZZ18W8sGuAao91UFuAAIzqcrziIfFAA__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA)
indo_land_cover <- indo_land_cover %>%
  mutate(Suitability = ifelse(desc_en == "Bare Land" | desc_en == "Savannah", 1, 0)) %>%
  filter(Suitability == 0)

ggplot()+
  geom_sf(data=indo_province, aes(fill="ADM0_EN"))+
  geom_sf(data=indo_land_cover, colour = NA, aes(fill="Suitability"))+
  labs(x = "Longitude", y = "Latitude", fill = "Suitability")+
  scale_fill_manual(values = c("green","darkred"), labels = c("Suitable","Unsuitable"))+
  guides(fill=guide_legend(override.aes=list(size=6,shape=21)))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#3.5 Road network
ggplot()+
  geom_sf(data=indo, aes(fill="ADM0_EN"))+
  geom_sf(data=indo_road, colour = "darkred", aes(fill = F_CODE_DES), key_glyph = "rect")+
  labs(x = "Longitude", y = "Latitude", fill = "Suitability")+
  scale_fill_manual(values = c("green","darkred"), labels = c("Suitable","Unsuitable"))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#3.7. Rasterize and compile all constraints
#Resolution 12 km2 per grid
raster_box <- rast(xmin = 95.01079, xmax= 141.01940, ymin = -11.00762, ymax = 6.07693, crs=st_crs(indo)$wkt, resolution = 0.0288675123)
extension <- ext(95.01079, 141.01940,-11.00762, 6.07693)

protected_raster <- rasterize(protected_2km, raster_box)
crs(protected_raster) <- crs(indo) 
writeRaster(protected_raster, "protected.tiff", overwrite = TRUE)

indo_population_500 <- resample(indo_population_500,protected_raster)
indo_population_500 <- extend(indo_population_500,extension)
crs(indo_population_500) <- crs(indo)

indo_elevation_rc <- resample(indo_elevation_rc, protected_raster)
crs(indo_elevation_rc) <- crs(indo)
indo_slope_rc <- resample(indo_slope_rc, protected_raster)
crs(indo_slope_rc) <- crs(indo)

lc_raster <- rasterize(indo_land_cover, raster_box)
crs(lc_raster) <- crs(indo) 
writeRaster(lc_raster, "lc.tiff", overwrite = TRUE)

road_raster <- rasterize(indo_road, raster_box)
crs(road_raster) <- crs(indo) 
writeRaster(road_raster, "road.tiff", overwrite = TRUE)

constrain = c(protected_raster, indo_population_500, indo_elevation_rc, indo_slope_rc, lc_raster, road_raster)
names(constrain) <- c("protected","population","elevation","slope","land_cover", "road")
constrain <- mask(constrain,indo)
crs(constrain) <- crs(indo)
constrain.df = as.data.frame(constrain, xy = TRUE)
id = which(is.na(constrain.df$protected) & constrain.df$population == 0 & constrain.df$elevation == 1 & constrain.df$slope == 1 & is.na(constrain.df$land_cover) 
           & is.na(constrain.df$road))
constrain.df$suitability=NA
constrain.df$suitability[id] = 1
constrain.df$suitability[constrain.df$suitability==0] <- NA

rm(protected)
rm(indo_population)
rm(indo_elevation)
rm(indo_slope)
rm(indo_elevation_m)
rm(indo_slope_m)

ggplot()+
  geom_tile(data=subset(constrain.df, suitability==1), aes(x=x,y=y,fill=suitability))+
  geom_sf(data=indo_province, fill=NA)+
  labs(x = "Longitude", y = "Latitude", fill = "Suitability")+
  scale_fill_gradientn(colours=topo.colors(1), labels = "Suitable")+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme
#----------------------------------------------------------------------------------------------
#4. Spatial interpolation for solar irradiance data
library(ncdf4)
library(chron)
library(lattice)
library(rgdal)
library(tmap)

era <- nc_open(paste0(wd,"/era5.nc" ))

#Extract dimensions
lon <- ncvar_get(era, "longitude")
lat <- ncvar_get(era, "latitude")
time <- ncvar_get(era, "time")

#Convert time from string to date-time
tunits <- ncatt_get(era,"time","units")
tustr <- strsplit(tunits$value, " ") 
tdstr <- strsplit(unlist(tustr)[3], "-")
tyear <- as.integer(unlist(tdstr)[1]) 
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])

#Extract variables
ssrd_array <- ncvar_get(era,"ssrd")
dlname <- ncatt_get(era,"ssrd","long_name")
dunits <- ncatt_get(era,"ssrd","units")
fillvalue <- ncatt_get(era,"ssrd","_FillValue")

#Slice data for all 08.00 to 16.00
lonlat <- as.matrix( (expand.grid(lon, lat)))
 for(x in 1:8){
   array <- ssrd_array[,,x]
   vec <- as.vector(array)
   df <- data.frame(cbind(lonlat,vec))
   if(x==1){
     ssrd_df_combo <- rbind(df)
   }
   else if (x==4 | x ==8){
        }
   else {
     ssrd_df_combo <- rbind(ssrd_df_combo, df)
   }
 }

colnames(ssrd_df_combo) <- c("lon", "lat", "ssrd")
ssrd_df_combo <- na.omit(ssrd_df_combo)

#Create spatial object
ssrd_sf_combo<- st_as_sf(ssrd_df_combo, coords = c("lon", "lat"))
st_crs(ssrd_sf_combo) <- 4326 
ssrd_sf_combo <- st_transform(ssrd_sf_combo, 4326)

#Calculate power from solar irradiance
radiation_to_power <- function(G, A=1, r=0.175, p=0.6, hours=1){
  kWh <- G * A * r * p * (hours/3600) / 1000
  return(kWh)
}

ssrd_kwh <- as.data.frame (radiation_to_power(ssrd_df_combo))
ssrd_df_combo <- cbind(ssrd_df_combo,ssrd_kwh$ssrd)
colnames(ssrd_df_combo) [4] <- 'ssrd_kwh'
ssrd_sf_combo$ssrd_kwh = ssrd_kwh$ssrd
ssrd_sf_combo <- ssrd_sf_combo %>%
  group_by(geometry) %>%
  summarise(ssrd_kwh = mean(ssrd_kwh), ssrd = mean(ssrd))

#Initial plot
ggplot()+
  geom_sf(data=ssrd_sf_combo, aes(colour=ssrd), size = 0.01)+
  geom_sf(data=indo, fill=NA)+
  geom_sf(data=indo_province, fill=NA)+
  labs(x = "Longitude", y = "Latitude", colour = "Surface Solar\nRadiation Downwards\n(W/m\u00b2)")+
  scale_colour_gradientn(colours=rev(heat.colors(10)))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

ggplot()+
  geom_sf(data=ssrd_sf_combo, aes(colour=ssrd_kwh), size = 0.01)+
  geom_sf(data=indo, fill=NA)+
  geom_sf(data=indo_province, fill=NA)+
  labs(x = "Longitude", y = "Latitude", colour = "Energy\nGeneration\n(kWh/m\u00b2)")+
  scale_colour_gradientn(colours=rev(heat.colors(10)))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#Conduct IDW for interpolation
#Remove NA values and geometry
coor = as.data.frame(st_coordinates(ssrd_sf_combo))
ssrd_sf_combo$x = coor$X
ssrd_sf_combo$y = coor$Y
ssrd_combo_nogeom = st_drop_geometry(ssrd_sf_combo) %>%
  na.omit()
  
rm(coor)
rm(dlname)
rm(dunits)
rm(era)
rm(fillvalue)
rm(lonlat)
rm(ssrd_df_combo)
rm(ssrd_kwh)
rm(ssrd_sf_combo)
rm(tdstr)
rm(tunits)
rm(tustr)

#Cross-validation for IDP value
n_idp = 20 #examine power ranging from 1 to 20
n_fold =10

RMSE <- function(observed, predicted) {
  sqrt(mean((predicted - observed)^2, na.rm=TRUE))
}

rmse <- rep(NA, n_fold) #generate 10 NA
set.seed(3456)
kf <- sample(1:n_fold, nrow(ssrd_combo_nogeom), replace=TRUE)
va = data.frame( c(1:n_idp), NA)
colnames(va) =c("idp","rmse") 

for (j in 1:n_idp) 
{
  for (i in 1:n_fold) {
    test <- ssrd_combo_nogeom[kf == 1, ]
    train <- ssrd_combo_nogeom[kf != 1, ]
    gs <- gstat::gstat(formula=ssrd_kwh~1, locations=~x+y, data=train, nmax=Inf, set=list(idp=j))
    pre = predict(gs, test, debug.level=0 )
    rmse[i] <- RMSE(test$ssrd_kwh, pre$var1.pred)
  }
  va[j,2] = (mean(rmse) )
}

#3 as the IDP
va[which(va$rmse==min(va)),] 

#Interpolate
raster_small <- rast(xmin = 95.01079, xmax= 141.01940, ymin = -11.00762, ymax = 6.07693, crs=st_crs(indo)$wkt, resolution = 0.08333333)
gs <- gstat::gstat(formula=ssrd_kwh~1, locations=~x+y, data=ssrd_combo_nogeom, nmax=Inf, set=list(idp=3))
idw <- interpolate(raster_small,gs,debug.level=0)
idw_mask <- mask(idw, indo)
idw_mask <- resample(idw_mask, raster_box)
names(idw_mask)=c("Predicted","Observed")

#Create data frame and plot
idw_mask.df <- as.data.frame(idw_mask,xy=TRUE) %>%
  mutate(Predicted_Annual = Predicted*8760,
         weight = case_when(Predicted > 0.08 & Predicted <= 0.29 ~ 0.3, Predicted > 0.29 & Predicted <= 0.49 ~ 0.6, Predicted > 0.49 ~ 1, .default = 0), 
         category = case_when(weight == 1 ~ "Highly Suitable", weight == 0.6 ~ "Moderately Suitable", weight == 0.3 ~ "Less Suitable", .default = "Not Suitable"))

cat_level = c("Highly Suitable", "Moderately Suitable", "Less Suitable", "Not Suitable")
idw_mask.df$category <- factor(idw_mask.df$category, levels = cat_level)

#Interpolation plot
ggplot()+
  geom_tile(data=idw_mask.df, aes(x=x,y=y,fill=Predicted))+
  geom_sf(data=indo_province, fill=NA)+
  labs(x = "Longitude", y = "Latitude", fill = "Predicted Generation\n(kWh/m\u00b2)")+
  scale_fill_gradientn(colours=rev(heat.colors(10)))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#Categorized plot
ggplot()+
  geom_tile(data=idw_mask.df,aes(x,y,fill=category), lwd = 0.01)+
  geom_sf(data=indo, fill=NA)+
  scale_fill_manual(values = c("darkgreen", "cyan", "orange", "gray"))+
  labs(x = "Longitude", y = "Latitude", fill = "Energy Generation\nSuitability")+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme
#-----------------------------------------------------------------------------------------------
#5. Distance from electricity network & roads
#Electricity network
#Import file and extract Indonesia electrical network
indo_powerline <- read_sf(paste0(wd,"/grid.geojson")) %>%
  st_transform(crs=st_crs(indo))
indo_powerline_intersect <- st_intersects(indo_powerline, indo, sparse = FALSE)
indo_powerline <- indo_powerline[which(indo_powerline_intersect==TRUE),]

#Calculate distance of cell grids from electricity network
indo_gridded <- st_make_grid(indo,cellsize=0.08333333,what="centers")
indo_gridded_intersect <- st_intersects(indo_gridded, indo, sparse = FALSE)
indo_gridded <- indo_gridded[which(indo_gridded_intersect==TRUE)]

elec_distance <- st_distance(indo_gridded, indo_powerline)
elec_distance.df <- data.frame(st_coordinates(indo_gridded), dist=as.vector(elec_distance)/1000) %>%
  group_by(X,Y) %>%
  summarise_at(vars(dist), list(min=min))

elec_distance_raster <- rast(elec_distance.df, type = 'xyz')
elec_distance_raster <- resample(elec_distance_raster, raster_box)
elec_distance.df <- as.data.frame(elec_distance_raster, xy = TRUE) %>%
  mutate(weight = case_when(min >= 0 & min <= 5 ~ 1, min > 5 & min <= 10 ~ 0.6, min > 10 & min <= 20 ~ 0.3, .default = 0), category = case_when(weight == 1 ~ "Highly Suitable", weight == 0.6 ~ "Moderately Suitable", weight == 0.3 ~ "Less Suitable", .default = "Not Suitable"))

elec_distance.df$category <- factor(elec_distance.df$category, levels = cat_level)

ggplot()+
  geom_tile(data=elec_distance.df,aes(x,y,fill=category))+
  geom_sf(data=indo, fill=NA)+
  geom_sf(data=indo_powerline, colour="black", lwd = 0.01)+
  scale_fill_manual(values = c("darkgreen", "cyan", "orange", "gray"))+
  labs(x = "Longitude", y = "Latitude", fill = "Electricity Network\nSuitability")+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme

#Road network
road_distance <- st_distance(indo_gridded, indo_road)
road_distance.df <- data.frame(dist=as.vector(road_distance)/1000,
                               st_coordinates(indo_gridded)) %>%
  group_by(X,Y) %>%
  summarise_at(vars(dist), list(min=min))

road_distance_raster <- rast(road_distance.df, type = 'xyz')
road_distance_raster <- resample(road_distance_raster, raster_box)
road_distance.df <- as.data.frame(road_distance_raster, xy = TRUE) %>%
  mutate(weight = case_when(min > 0.1 & min <= 5 ~ 1, min > 5 & min <= 20 ~ 0.6, min >= 0 & min <= 0.1 ~ 0.3, .default = 0), category = case_when(weight == 1 ~ "Highly Suitable", weight == 0.6 ~ "Moderately Suitable", weight == 0.3 ~ "Less Suitable", .default = "Not Suitable"))

road_distance.df$category <- factor(road_distance.df$category, levels = cat_level)

rm(indo_gridded_intersect)
rm(elec_distance)
rm(road_distance)

ggplot()+
  geom_tile(data=road_distance.df,aes(x,y,fill=category))+
  geom_sf(data=indo, fill=NA)+
  geom_sf(data=indo_road, colour="black", lwd = 0.01)+
  scale_fill_manual(values = c("darkgreen", "cyan", "orange", "gray"))+
  labs(x = "Longitude", y = "Latitude", fill = "Road Network\nSuitability")+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme
#-----------------------------------------------------------------------------------------------
#6. AHP
#Create AHP matrix
ahp_matrix <- matrix(c(1, 3,  4,
                       0.33, 1, 2,
                       0.25, 0.5, 1), ncol=3, byrow=TRUE) 
colnames(ahp_matrix) <- c("C1", "C2", "C3")

norm_matrix <- sweep(ahp_matrix,2,colSums(ahp_matrix),'/')
norm_weight <- matrix(rowMeans(norm_matrix[1:3,]),ncol=1)

#Check consistency ratio
#n = 3, random_index = 0.58
eigen_matrix <- (ahp_matrix %*% norm_weight) / norm_weight
consistency_index <- (max(eigen_matrix)-nrow(ahp_matrix))/(nrow(ahp_matrix)-1)
consistency_ratio <- consistency_index/0.58 #0.026 <= 0.1, satisfactory

#Extract values from criteria rasters
constrain_sf <- st_as_sf(constrain.df, coords = c("x","y"), crs=crs(indo))
solar_extract <- extract(idw_mask, vect(constrain_sf))
constrain_sf$solar <- solar_extract$Predicted
constrain_sf$solar_category <- case_when(constrain_sf$solar >- 0.08 & constrain_sf$solar <= 0.29 ~ 0.3, constrain_sf$solar > 0.29 & constrain_sf$solar <= 0.49 ~ 0.6, constrain_sf$solar > 0.49 ~ 1, .default = 0)
constrain_sf$solar_weight <- norm_weight[1]

elec_extract <- extract(elec_distance_raster, vect(constrain_sf))
constrain_sf$elec_distance <- elec_extract$min
constrain_sf$elec_category <- case_when(constrain_sf$elec_distance >= 0 & constrain_sf$elec_distance <= 5 ~ 1, constrain_sf$elec_distance > 5 & constrain_sf$elec_distance <= 10 ~ 0.6, constrain_sf$elec_distance > 10 & constrain_sf$elec_distance <= 20 ~ 0.3, .default = 0)
constrain_sf$elec_weight <- norm_weight[2]

road_extract <- extract(road_distance_raster, vect(constrain_sf))
constrain_sf$road_distance <- road_extract$min
constrain_sf$road_category <- case_when(constrain_sf$road_distance > 0.1 & constrain_sf$road_distance <= 5 ~ 1, constrain_sf$road_distance > 5 & constrain_sf$road_distance <= 20 ~ 0.6, constrain_sf$road_distance >= 0 & constrain_sf$road_distance <= 0.1 ~ 0.3, .default = 0)
constrain_sf$road_weight <- norm_weight[3]

#Calculate weighted criteria values
constrain_sf <- constrain_sf %>%
  mutate(ahp_weight = solar_category*solar_weight + elec_category*elec_weight + road_category*road_weight,
         ahp_range = case_when(ahp_weight >= 0 & ahp_weight <= 0.20 ~ "0-20%", ahp_weight > 0.2 & ahp_weight <= 0.4 ~ "20-40%", ahp_weight > 0.4 & ahp_weight <= 0.6 ~ "40-60%", ahp_weight > 0.6 & ahp_weight <= 0.8 ~ "60-80%", ahp_weight > 0.8 & ahp_weight <= 1 ~ "80-100%"))

suitable_sf <- subset(constrain_sf, suitability == 1) %>%
  tidyr::drop_na(elec_distance) %>%
  tidyr::drop_na(road_distance)

suitable_raster <- rasterize(suitable_sf,raster_box)

#-----------------------------------------------------------------------------------------------
#7. Calculate number of solar plants
#Resolution is 0.0288675123, equivalent to 3.2 km
#Solar capacity factor = 0.19
#Revenue selling price is assumed to be 0.103 USD/kWh
suitable_sf$full_capacity_mw = suitable_sf$solar*8760*((3.2*10^3)^2)/(8760*10^3)
suitable_sf$capacity_mw = suitable_sf$full_capacity_mw*0.21
suitable_sf$generated_mw = suitable_sf$capacity_mw*0.19
suitable_sf$generated_kwh = suitable_sf$generated_mw*8760*10^3
suitable_sf$annual_revenue_Musd = suitable_sf$generated_kwh*0.103/10^6

suitable_sites <- nrow(suitable_sf)
suitable_capacity <- sum(suitable_sf$capacity_mw)
suitable_generation <- sum(suitable_sf$generated_mw)
suitable_powerline_length <- sum(suitable_sf$elec_distance)
suitable_area <- expanse(suitable_raster, "km")

#Optimize selection by omitting zero electricity and road distance categories
optimized_sf <- subset(suitable_sf, elec_category != 0 & road_category != 0)
optimized_raster <- rasterize(optimized_sf, raster_box)

optimized_sites <- nrow(optimized_sf)
optimized_capacity <- sum(optimized_sf$capacity_mw)
optimized_generation <- sum(optimized_sf$generated_mw)
optimized_powerline_length <- sum(optimized_sf$elec_distance)
optimized_area <- expanse(optimized_raster, "km")

 ggplot()+
   geom_sf(data=indo_province, fill=NA)+
   geom_sf(data=optimized_sf,size = 1.5, aes(colour=ahp_range), lwd = 0.001)+
   labs(x = "Longitude", y = "Latitude", colour = "AHP Suitability Index")+
   guides(colour=guide_legend(override.aes=list(size=6)))+
   annotation_scale(location="br")+
   annotation_north_arrow(location="tr",which_north=TRUE)+
   transparent_theme

#Plant selection
indo_province_OGR <- readOGR(paste0(wd,"/idn_admbnda_adm1_bps_20200401.shp"))
province_df <- as.data.frame(st_coordinates(optimized_sf)) %>%
  rename(x=X,y=Y)

coordinates(province_df) <- ~ x+y
proj4string(province_df) <- proj4string(indo_province_OGR)

plant_province <- over(province_df, indo_province_OGR)
plant_province <- mutate(plant_province, id = seq_len(nrow(plant_province)))

optimized_sf$id <- seq_len(optimized_sites)
optimized_sf <- left_join(optimized_sf, plant_province[,c("id","ADM1_EN", "ADM1_PCODE")], by = "id") %>%
  mutate(island = case_when(grepl("ID1", ADM1_PCODE) | grepl("ID2", ADM1_PCODE) ~ "Sumatra", grepl("ID3", ADM1_PCODE) ~ "Java", grepl("ID5", ADM1_PCODE) ~ "Bali and Sumba", grepl("ID6", ADM1_PCODE) ~ "Kalimantan", grepl("ID7", ADM1_PCODE) ~ "Sulawesi", grepl("ID8", ADM1_PCODE) ~ "Maluku", grepl("ID9", ADM1_PCODE) ~ "Papua"))

#It can be seen that Sumatera and Kalimantan dominates compared to other islands, therefore we will select
#all sites from other islands
optimized_sf %>% count(island)
selected_sf <- subset(optimized_sf, island == "Sulawesi" | island == "Bali and Sumba")


#Sort table by AHP values and distances from grid and road
optimized_sf <- optimized_sf[order(-optimized_sf$ahp_weight,optimized_sf$elec_distance, optimized_sf$road_distance),]

#Loop until generated power reaches shortfall of 16.25 GW
for(x in 1:nrow(optimized_sf)){
    gen = sum(selected_sf$generated_mw)
    if (gen >= 16.25*10^3){
      break
    }
    selected_sf[1+nrow(selected_sf),] <- optimized_sf[x,]
  }

#Calculate capital expenditure, NPV, and LCOE
capex_solar <- function(install_capacity, capacity_cost = 1.16, grid_distance, grid_cost = 590){
  capex <- capacity_cost * install_capacity + grid_cost * install_capacity * grid_distance / 10^6
  return(capex)
}

calc_NPV <- function(annual_revenue, i=0.05, lifetime_yrs = 25, CAPEX, OPEX=0){
  costs_op <- rep(OPEX, lifetime_yrs) #operating cost
  revenue <- rep(annual_revenue, lifetime_yrs) 
  t <- seq(1, lifetime_yrs, 1) #output: 1, 2, 3, ...25
  
  NPV <- sum( (revenue - costs_op)/(1 + i)**t ) - CAPEX
  return(round(NPV, 0))
}

Life_span_generation_kWH <- function (yearly_generation_kWH, discount = 0.08, lifetime_yrs = 25){
  kwh <- rep(yearly_generation_kWH, lifetime_yrs)
  t<- seq(1, lifetime_yrs, 1)
  L_S_G <- sum(kwh/(1+discount)**t)
  return (round(L_S_G,0))
}

LCOE <- function(NPV,Life_span_generation){
  lcoe <- NPV/Life_span_generation
  return(round(lcoe,2))
}

#OPEX assumption is $25/kWh https://emp.lbl.gov/publications/benchmarking-utility-scale-pv
selected_sf$capex_Musd <- capex_solar(install_capacity = selected_sf$capacity_mw, grid_distance = selected_sf$elec_distance)
selected_sf$opex_Musd <- selected_sf$capacity_mw*25*10^3/10^6
selected_sf$npv_Musd <- calc_NPV(annual_revenue = selected_sf$annual_revenue_Musd, CAPEX = selected_sf$capex_Musd, OPEX = selected_sf$opex_Musd)

selected_sf$lsg_kwh <- Life_span_generation_kWH(yearly_generation_kWH=selected_sf$generated_kwh)
selected_sf$lcoe_usdkwh <- LCOE(selected_sf$npv_Musd*10^6, selected_sf$lsg_kwh)

selected_raster <- rasterize(selected_sf, raster_box)
selected_sites <- nrow(selected_sf)
selected_capacity <- sum(selected_sf$capacity_mw)
selected_generation <- sum(selected_sf$generated_mw)
selected_powerline_length <- sum(selected_sf$elec_distance)
selected_area <- expanse(selected_raster, "km")

selected_summary <- selected_sf %>% 
                  select(island, ADM1_EN,elec_distance,capacity_mw,generated_mw) %>% 
                  group_by(island, ADM1_EN) %>%
                  summarise(sites = n(), area = round(sites/nrow(selected_sf)*selected_area[1,2],2), across(where(is.numeric), ~ round(sum(.x, na.rm = TRUE),2)))

ggplot()+
  geom_sf(data=indo_province, fill=NA)+
  geom_sf(data=indo_powerline, aes(colour = power), lwd = 0.01)+
  scale_colour_manual(values = "black", labels = "Powerline")+
  geom_sf(data=selected_sf,colour="black", size = 3, shape = 21, aes(fill=ahp_range))+
  labs(x = "Longitude", y = "Latitude", colour = "Electricity Network", fill = "AHP Suitability Index")+
  guides(fill=guide_legend(override.aes=list(size=6,shape=21)))+
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr",which_north=TRUE)+
  transparent_theme
