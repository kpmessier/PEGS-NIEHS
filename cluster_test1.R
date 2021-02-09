### TRI for Submission to Cluster ###
#PEGS3 <- read.csv("~/Desktop/PEGS_CensusTractAddresses.csv")

PEGS3 <- read.csv("/ddn/gs1/group/shag/loweme/pegs_majorroads/PEGS_CensusTractAddresses.csv")
PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude))

library(sp)
library(rgeos)
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


#roads <-rgdal::readOGR(dsn="/Volumes/shag/loweme/pegs_majorroads/roadtrl010g.shp_nt00920")

roads <-rgdal::readOGR(dsn="/ddn/gs1/group/shag/loweme/pegs_majorroads/roadtrl010g.shp_nt00920")
roads@proj4string
roads <- spTransform(roads, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 



  l <- split(spdf2, spdf2$epr_number_TYPE) 
  
  #create 1km buffer around each address which is a unique component of the list l
  
  
  e <- NA #make an empty item
  rd_1kmfun <- function(i){
    lbuff_1km <- gBuffer(l[[i]], width = 1000)
    rd_1km <- tryCatch(raster::crop(roads, extent(lbuff_1km)), error = function(e) e)
    ## Mess with this to make it work in the lapply.
    ifelse(class(rd_1km)[1] == "simpleError", NA, gLength(rd_1km, byid=F))
    #find total road length in buffer if there are roads within the buffer
    #otherwise, NA. "simpleError" is produced by the trycatch when it fails
    
    #trycatch is used to manage the errors where there are no roads within 10km, "simpleError" is what is left
    #crop the roads around each address
  }
  

  totrd_1km<- lapply(1:length(spdf2), rd_1kmfun)
 # totrd_1km<- lapply(1:15, rd_1kmfun)
  
  save(totrd_1km,file = "/ddn/gs1/group/shag/loweme/pegs_majorroads/totrd_1kmtry.RData")
  
  #profiling: helps determine where the slowest part of the code is. shows how much time spent in each function
 