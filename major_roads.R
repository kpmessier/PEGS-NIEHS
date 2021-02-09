PEGS3 <- read.csv("~/Desktop/PEGS_CensusTractAddresses.csv")
PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude))


library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)

#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
PEGS2$epr_number_TYPE <- ifelse(PEGS2$geo_study_event == "Exposome Part A - Current Address", paste0(PEGS2$epr_number, "_2"),paste0(PEGS2$epr_number, "_1"))

xy <- PEGS2[,c("geo_longitude","geo_latitude")]
spdf <- SpatialPointsDataFrame(coords = xy, data = PEGS2, proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
                               #CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


spdf2 <- spTransform(spdf,  CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

xy[12314,]
library(sp)
library(rgeos)
library(nngeo)

roads <-rgdal::readOGR(dsn="~/Desktop/roadtrl010g.shp_nt00920")
roads@proj4string
roads <- spTransform(roads, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 


#miles of roads within a buffer

#what size buffer?

# 1 km buffer, 5km buffer, 10 km buffer

# break down road segment into points, points will be intersections or arbitrary locations
# where they're trying to create curves. might make sense to do something myself 
# convert it in QGIS: conver the line to a point file, converting things betwen different structures
# maybe make a point every kilometer or 500 meters. 
# then bringing the shapefile in to R
# sometimes spatial packages can be quite smart and not pull everything in at once.
# calculate distance to the points. every point within buffer represents a km or half a km of road. 


# another thing to look into: k nearest neighbors packages. it was a computational breakthrough 
# it's very efficient and very fast, might slow it down if you ask it to give the distance.

#there are a lot of spatial packages in r that have coalesced around a spatial data science group
# rspatial.org



  l <- split(spdf2, spdf2$epr_number_TYPE) #split the spatial dataframe by row into a list of dataframes
  lbuff_1km <- lapply(l, gBuffer, width = 1000) #apply gbuffer

  e <- NA
  rd_1kmfun <- function(i){
    tryCatch(raster::crop(roads, extent(lbuff_1km[[i]])), error = function(e) e) 
    #create a function to crop the roads dataset by the extent of the buffer
    #tryCatch deals with any error messages and leaves it as an Simple error object
  }



  rd_1km <- lapply(1:5, rd_1kmfun) # lapply statement with that function
  
  totrd_1kmfun <- function(i){
    
    ifelse(class(rd_1km[[i]])[1] == "simpleError", NA, gLength(rd_1km[[i]], byid=F))
    #find total road length in buffer if there are roads within the buffer
    #otherwise, NA
  }
  
  totrd_1km <- lapply(1:5, totrd_1kmfun) # lapply statement with that function
  

  mindist_1kmfun <- function(i){
    
    ifelse(class(rd_1km[[i]])[1] == "simpleError", NA, min(gDistance(unlist(l[[i]]), unlist(rd_1km[[i]]), byid=T)))
    #find minimum distance from point to road if there are roads within the buffer
    #otherwise, NA
  }
  mindist_1km<- lapply(1:5, mindist_1kmfun) # lapply statement with that function
  
  
  nearn_1km <- st_nn(st_as_sf(l[[i]]),ptsrd_1km, returnDist=T )#find k nearest neighbors + distance
  
  
buff_1 <-  function(l){
  # 1km buffer
  lbuff_1km <- gBuffer(l, width = 1000) #establish buffer around address
  rd_1km <- raster::crop(roads, extent(lbuff_1km)) # crop roads spatial data by extent of buffer
  mindist2rd_1km <- min(gDistance(l, rd_1km, byid=TRUE)) # find distance from address to lines 
  totrd_1km <- gLength(rd_1km, byid=F) #find total road length in buffer
  ptsrd_1km = st_as_sf(as(rd_1km, "SpatialPointsDataFrame")) # make roads into points
  l2 <- st_as_sf(l) #convert l to st
  nearn_1km <- st_nn(l2,ptsrd_1km, returnDist=T )#find k nearest neighbors + distance
  eprnumtype <- l@data$epr_number_TYPE
  return(c(eprnumtype, mindist2rd_1km, totrd_1km, unlist(nearn_1km)))
 
}  
#make an apply statement for each of the components. lapply working on a list is the most flexible - slowerish
#
test <- try(buff_1(l))
if(class(test)=="try-error") {next()}
dat1 <- as.data.frame(t(test))
names(dat1) <-  c("eprnumtype", "mindist2rd_1km", "totrd_1km", "nn","nearn_1km")
data_rds <- rbind(data_rds, dat1)

}









data_rds5 <- NULL

for (i in 1:length(spdf2)) {
  l<- spdf2[i,]
  
  buff_5 <-  function(l){
    # 5km buffer
    lbuff_5km <- gBuffer(l, width = 5000) #establish buffer around address
    rd_5km <- raster::crop(roads, extent(lbuff_5km)) # crop roads spatial data by extent of buffer
    mindist2rd_5km <- min(gDistance(l, rd_5km, byid=TRUE)) # find distance from address to lines 
    totrd_5km <- gLength(rd_5km, byid=F) #find total road leangth in buffer
    ptsrd_5km = st_as_sf(as(rd_5km, "SpatialPointsDataFrame")) # make roads into points
    l2 <- st_as_sf(l) #convert l to st
    nearn_5km <- st_nn(l2,ptsrd_5km, returnDist=T )#find k nearest neighbors + distance
    eprnumtype <- l@data$epr_number_TYPE
    return(c(eprnumtype, mindist2rd_5km, totrd_5km, unlist(nearn_5km)))
    
  }  
  
  test <- try(buff_5(l))
  if(class(test)=="try-error") {next()}
  dat1 <- as.data.frame(t(test))
  names(dat1) <-  c("eprnumtype", "mindist2rd_5km", "totrd_5km", "nn","nearn_5km")
  data_rds5 <- rbind(data_rds5, dat1)
  
}
 

data_rds10 <- NULL

for (i in 1:length(spdf2)) {
  l<- spdf2[i,]
  
  buff_10 <-  function(l){
    # 10km buffer
    lbuff_10km <- gBuffer(l, width = 10000) #establish buffer around address
    rd_10km <- raster::crop(roads, extent(lbuff_10km)) # crop roads spatial data by extent of buffer
    mindist2rd_10km <- min(gDistance(l, rd_10km, byid=TRUE)) # find distance from address to lines 
    totrd_10km <- gLength(rd_10km, byid=F) #find total road leangth in buffer
    ptsrd_10km = st_as_sf(as(rd_10km, "SpatialPointsDataFrame")) # make roads into points
    l2 <- st_as_sf(l) #convert l to st
    nearn_10km <- st_nn(l2,ptsrd_10km, returnDist=T )#find k nearest neighbors + distance
    eprnumtype <- l@data$epr_number_TYPE
    return(c(eprnumtype, mindist2rd_10km, totrd_10km, unlist(nearn_10km)))
    
  }  
  
  test <- try(buff_5(l))
  if(class(test)=="try-error") {next()}
  dat1 <- as.data.frame(t(test))
  names(dat1) <-  c("eprnumtype", "mindist2rd_10km", "totrd_10km", "nn","nearn_10km")
  data_rds10 <- rbind(data_rds10, dat1)
  
}

