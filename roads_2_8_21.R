#### New attempt at managing the major roads question ###

### Started February 8 2021 #####

### Libraries ####

library(sp)
library(rgeos)
library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(sf)
library(mapview)
library(units)

#### IMPORT THE DATA ####
### TWO OPTIONS: one for the cluster and one for testing ###

PEGS3 <- read.csv("~/Desktop/PEGS_CensusTractAddresses.csv")

#PEGS3 <- read.csv("/ddn/gs1/group/shag/loweme/pegs_majorroads/PEGS_CensusTractAddresses.csv")

#roads <-rgdal::readOGR(dsn="/Volumes/shag/loweme/pegs_majorroads/roadtrl010g.shp_nt00920")

#roads <-rgdal::readOGR(dsn="/ddn/gs1/group/shag/loweme/pegs_majorroads/roadtrl010g.shp_nt00920")
roads2 <- read_sf('~/Desktop/roadtrl010g.shp_nt00920') #loads WAYYY Faster than rgdal

PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude))
#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
PEGS2$epr_number_TYPE <- ifelse(PEGS2$geo_study_event == "Exposome Part A - Current Address", paste0(PEGS2$epr_number, "_2"),paste0(PEGS2$epr_number, "_1"))


### Roads is coming in as a spatial lines dataframe.(sf object? how?)

### PEGS should be transformed into a spatial points dataframe. (or simpler? sf object)

#xy <- PEGS2[,c("geo_longitude","geo_latitude")] #longitude, then latitude

#spdf <- SpatialPointsDataFrame(coords = xy, data = PEGS2,
#                               proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))

projcrs <- "+proj=longlat +datum=NAD83 +no_defs"
df_sf <- st_as_sf(x = PEGS2,                         
               coords = c("geo_longitude", "geo_latitude"),
               crs = projcrs)

## In order to create buffers, we need to transform the crs to something flat. 
## Arguably, this should be dependent on the location of the item in PEGS. 


### What is the original CRS of roads?

proj4string(roads)
#[1] "+proj=longlat +datum=NAD83 +no_defs"
#Warning message:
#  In proj4string(roads) : CRS object has comment, which is lost in output

# there are a lot of changes happening to spatial R packages right now. the comment lost in output is probably part of that.


# we'll use albers equal area projection:
# "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#what if i try using sf only?

#convert to albers ea projection

sf2 <- df_sf %>% st_transform(crs = 5070)
crs(sf2)
crs(roads2)
roads3 <- roads2 %>% st_transform(crs = 5070)
crs(roads3)
density_1km <- NA
density_5km <- NA
density_10km <- NA
library(tictoc)
tic()
for ( i in 4880:12339){
  
sf2_buffer1 <-  st_buffer(sf2[i,], 1000)
sf2_buffer2 <-  st_buffer(sf2[i,], 5000)
sf2_buffer3 <-  st_buffer(sf2[i,], 10000)


#mapview(sf2_buffer2)

testcrop1 <- st_crop(roads3, sf2_buffer1)
testcrop2 <- st_crop(roads3, sf2_buffer2)
testcrop3 <- st_crop(roads3, sf2_buffer3)

#mapview(testcrop)

length_test1 <- sum(st_length(testcrop1))
length_test2 <- sum(st_length(testcrop2))
length_test3 <- sum(st_length(testcrop3))

density_1km <- c(density_1km, length_test1)
density_5km <- c(density_5km, length_test2)
density_10km <- c(density_10km, length_test3)
}
toc()


write.csv(density_10km, "~/Desktop/1_4879density10km.csv") #all addresses
write.csv(density_5km, "~/Desktop/1_4879density5km.csv") #all addresses
write.csv(density_1km, "~/Desktop/1_4879density1km.csv") #all addresses

