###libraries ###

library(rgdal)
library(sp)
library(sf)
library(rgeos)
library(tidyr)
library(dplyr)
library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(sp)
library(rgeos)
library(nngeo)

###import PEGS addresses ###
PEGS3 <- read.csv("~/Desktop/PEGS_CensusTractAddresses.csv")
PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude))


#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
PEGS2$epr_number_TYPE <- ifelse(PEGS2$geo_study_event == "Exposome Part A - Current Address", paste0(PEGS2$epr_number, "_2"),paste0(PEGS2$epr_number, "_1"))

xy <- PEGS2[,c("geo_longitude","geo_latitude")]
spdf <- SpatialPointsDataFrame(coords = xy, data = PEGS2, proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
#CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
spdf2 <- spTransform(spdf,  CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))



files.dir <- "~/Desktop/TRI/tri_val" 

filenames_TRI <- list.files(path=files.dir, full.names=T) 

TOLUENE <- NULL
BENZENE <- NULL
XYLENE <- NULL
EB <- NULL
for (i in 1:length(filenames_TRI)){
#for (i in 1:3){
  name1 <- (filenames_TRI[i])
  print(name1)
  TRI_a <- read.csv(name1)
  
  TOLUENE_a<- subset(TRI_a, TRI_a$X34..CHEMICAL == "TOLUENE" | TRI_a$X36..CAS...COMPOUND.ID == "108883")
  
  BENZENE_a <- subset(TRI_a, TRI_a$X34..CHEMICAL == "BENZENE"| TRI_a$X36..CAS...COMPOUND.ID == "71432")
  
  XYLENE_a<- subset(TRI_a, TRI_a$X34..CHEMICAL == "XYLENE (MIXED ISOMERS)"| TRI_a$X36..CAS...COMPOUND.ID == "108383" | TRI_a$X36..CAS...COMPOUND.ID == "1330207")
  
  EB_a <- subset(TRI_a, TRI_a$X34..CHEMICAL == "ETHYLBENZENE" | TRI_a$X36..CAS...COMPOUND.ID == "100414")
  
  TOLUENE <- as.data.frame(rbind(TOLUENE, TOLUENE_a))
  BENZENE <- as.data.frame(rbind(BENZENE, BENZENE_a))
  XYLENE <- as.data.frame(rbind(XYLENE, XYLENE_a))
  EB <- as.data.frame(rbind(EB, EB_a))
  
}




TOLUENE2 <- subset(TOLUENE, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR,X6..CITY,X7..COUNTY,X8..ST))
BENZENE2 <- subset(BENZENE, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR,X6..CITY,X7..COUNTY,X8..ST))
XYLENE2 <- subset(XYLENE, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR,X6..CITY,X7..COUNTY,X8..ST))
EB2 <- subset(EB, select= c(X12..LATITUDE, X13..LONGITUDE, X4..FACILITY.NAME, X45..5.1...FUGITIVE.AIR,X46..5.2...STACK.AIR,X6..CITY,X7..COUNTY,X8..ST))

states <- c("AL","AZ","AR","CA", "CO", "CT", "DE", "FL","GA","ID","IL","IN","IA", "KS","KY","LA","ME","MD","MA","MI",
          "MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX", "UT","VT", "VA","WA","WV","WI","WY","DC")
TOLUENE2 <- subset(TOLUENE2, TOLUENE2$X8..ST %in% states)
BENZENE2 <- subset(BENZENE2, BENZENE2$X8..ST %in% states)
XYLENE2 <- subset(XYLENE2, XYLENE2$X8..ST %in% states)
EB2 <- subset(EB2, EB2$X8..ST %in% states)

TOLUENE2$location <- paste0(TOLUENE2$X12..LATITUDE, TOLUENE2$X13..LONGITUDE)
length(unique(TOLUENE2$location))
BENZENE2$location <- paste0(BENZENE2$X12..LATITUDE, BENZENE2$X13..LONGITUDE)
length(unique(BENZENE2$location))
XYLENE2$location <- paste0(XYLENE2$X12..LATITUDE, XYLENE2$X13..LONGITUDE)
length(unique(XYLENE2$location))
EB2$location <- paste0(EB2$X12..LATITUDE, EB2$X13..LONGITUDE)
length(unique(EB2$location))



#write.csv(TOLUENE2, file="~/Desktop/TRI/tri_tot/TOLUENE_allyears.csv", row.names=F) #all addresses
#write.csv(BENZENE2, file="~/Desktop/TRI/tri_tot/BENZENE_allyears.csv", row.names=F) #all addresses
#write.csv(XYLENE2, file="~/Desktop/TRI/tri_tot/XYLENE_allyears.csv", row.names=F) #all addresses
#write.csv(EB2, file="~/Desktop/TRI/tri_tot/TOLUENE_allyears.csv", row.names=F) #all addresses



# TOLUENE2 <- read.csv("~/Desktop/TRI/tri_tot/TOLUENE_allyears.csv") #all addresses
# BENZENE2 <- read.csv("~/Desktop/TRI/tri_tot/BENZENE_allyears.csv") #all addresses
# XYLENE2 <- read.csv("~/Desktop/TRI/tri_tot/XYLENE_allyears.csv") #all addresses
# EB2 <- read.csv("~/Desktop/TRI/tri_tot/TOLUENE_allyears.csv") #all addresses

tloc <- unique(TOLUENE2$location) #3172
bloc <- unique(BENZENE2$location) # 1755
xloc <- unique(XYLENE2$location) #5568
ebloc <- unique(EB2$location) #3172
library(dplyr)
TOLUENE2.0 <- NULL
for (i in 1:3172)
{
   tol2 <- subset(TOLUENE2, TOLUENE2$location == tloc[i]) %>% summarise(stackair = mean(X46..5.2...STACK.AIR), fugair = mean(X45..5.1...FUGITIVE.AIR),loc2 = location) %>% unique()
   TOLUENE2.0 <- rbind(TOLUENE2.0, tol2)
}


BENZENE2.0 <- NULL
for (i in 1:1755)
{
  ben2 <- subset(BENZENE2, BENZENE2$location == bloc[i]) %>% summarise(stackair = mean(X46..5.2...STACK.AIR), fugair = mean(X45..5.1...FUGITIVE.AIR),loc2 = location) %>% unique()
  BENZENE2.0 <- rbind(BENZENE2.0, ben2)
}


XYLENE2.0 <- NULL
for (i in 1:5568)
{
  xol2 <- subset(XYLENE2, XYLENE2$location == xloc[i]) %>% summarise(stackair = mean(X46..5.2...STACK.AIR), fugair = mean(X45..5.1...FUGITIVE.AIR),loc2 = location) %>% unique()
  XYLENE2.0 <- rbind(XYLENE2.0, xol2)
}


EB2.0 <- NULL
for (i in 1:3172)
{
  eb2 <- subset(EB2, EB2$location == ebloc[i]) %>% summarise(stackair = mean(X46..5.2...STACK.AIR), fugair = mean(X45..5.1...FUGITIVE.AIR),loc2 = location) %>% unique()
  EB2.0 <- rbind(EB2.0, eb2)
}


#Turn these into spatial points dataframes


names(TOLUENE2) <- c("latitude","longitude","facility", "fugitive_air" ,"stack_air","city", "county", "state", "location")
names(BENZENE2) <- c("latitude","longitude","facility", "fugitive_air" ,"stack_air","city", "county", "state", "location")
names(XYLENE2) <- c("latitude","longitude","facility", "fugitive_air" ,"stack_air","city", "county", "state", "location")
names(EB2) <- c("latitude","longitude","facility", "fugitive_air" ,"stack_air","city", "county", "state", "location")

TOLUENE25 <- unique(subset(TOLUENE2, select=c("latitude","longitude", "facility", "location")))
BENZENE25 <- unique(subset(BENZENE2, select=c("latitude","longitude", "facility", "location")))
XYLENE25 <- unique(subset(XYLENE2, select=c("latitude","longitude", "facility", "location")))
EB25 <- unique(subset(EB2, select=c("latitude","longitude", "facility", "location")))


TOLUENE3 <- sf:::as_Spatial(st_as_sf(TOLUENE25, coords = c('longitude','latitude'), crs ="+proj=longlat +datum=NAD83 +no_defs"))
BENZENE3 <- sf:::as_Spatial(st_as_sf(BENZENE25, coords = c('longitude','latitude'), crs ="+proj=longlat +datum=NAD83 +no_defs"))
XYLENE3 <- sf:::as_Spatial(st_as_sf(XYLENE25, coords = c('longitude','latitude'), crs ="+proj=longlat +datum=NAD83 +no_defs"))
EB3 <- sf:::as_Spatial(st_as_sf(EB25, coords = c('longitude','latitude'), crs ="+proj=longlat +datum=NAD83 +no_defs"))

TOLUENE4 <- spTransform(TOLUENE3, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
BENZENE4 <- spTransform(BENZENE3, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
XYLENE4 <- spTransform(XYLENE3, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 
EB4 <- spTransform(EB3, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") ) 



matrixtest_t <- pointDistance(TOLUENE4, spdf2, lonlat=F, allpairs=T) #find distances between tri sites and addresses
matrixtest_t <- ifelse(matrixtest_t <= 0, NA, matrixtest_t) # make sure there aren't any 0 or negative values
colnames(matrixtest_t) <- spdf2$epr_number_TYPE #make sure we keep the names for addresses
rownames(matrixtest_t) <- TOLUENE4$location #and the TRI sites
matrixtest_t <- t(matrixtest_t) # transpose to match mxn structure


matrixtest_b <- pointDistance(BENZENE4, spdf2, lonlat=F, allpairs=T)
matrixtest_b <- ifelse(matrixtest_b <= 0, NA, matrixtest_b)
colnames(matrixtest_b) <- spdf2$epr_number_TYPE
rownames(matrixtest_b) <- BENZENE4$location
matrixtest_b <- t(matrixtest_b)


matrixtest_x <- pointDistance(XYLENE4, spdf2, lonlat=F, allpairs=T)
matrixtest_x <- ifelse(matrixtest_x <= 0, NA, matrixtest_x)
colnames(matrixtest_x) <- spdf2$epr_number_TYPE
rownames(matrixtest_x) <- XYLENE4$location
matrixtest_x <- t(matrixtest_x)


matrixtest_eb <- pointDistance(EB4, spdf2, lonlat=F, allpairs=T)
matrixtest_eb <- ifelse(matrixtest_eb <= 0, NA, matrixtest_eb)
colnames(matrixtest_eb) <- spdf2$epr_number_TYPE
rownames(matrixtest_eb) <- EB4$location
matrixtest_eb <- t(matrixtest_eb)


psEXP <- function(d,C0,decay) { 
  # d : distance matrix (m x n) m = # of monitoring sites, n = # of sources
  #  C0 : initial source concentrations
  # decay: vector of buffer values (presumably in the same units as d)
  X.ps <- matrix(NA,nrow = nrow(d),ncol = length(decay))
  
  decay.func <- function(x) sum(C0 * exp(-3 * x / decay[i]))
  
  for (i in 1:length(decay)){
    X.ps[,i] <- d %>% apply(1,decay.func)
  }
  
  return(X.ps)
  
}

#tloc <- unique(TOLUENE2$location) #3172
#bloc <- unique(BENZENE2$location) # 1755
#xloc <- unique(XYLENE2$location) #5568
#ebloc <- unique(EB2$location) #3172

#apply exponential decay function across the 12339 sites


decay2 <- c(1000,5000,10000) #decay distance options in meteres
test_benzene <- psEXP(matrixtest_b, BENZENE2.0$stackair, decay2) #apply decay function
test_benzene <- as.data.frame(cbind(spdf2$epr_number_TYPE, test_benzene))#add epr identifier
names(test_benzene) <-  c("epr_number_TYPE", "epdecay_1km", "epdecay_5km","epdecay_10km") #label columns

test_eb <- psEXP(matrixtest_eb, EB2.0$stackair, decay2)
test_eb <- as.data.frame(cbind(spdf2$epr_number_TYPE, test_eb))
names(test_eb) <-  c("epr_number_TYPE", "epdecay_1km", "epdecay_5km","epdecay_10km")


test_xylene <- psEXP(matrixtest_x, XYLENE2.0$stackair, decay2)
test_xylene <- as.data.frame(cbind(spdf2$epr_number_TYPE, test_xylene))
names(test_xylene) <-  c("epr_number_TYPE", "epdecay_1km", "epdecay_5km","epdecay_10km")


test_toluene <- psEXP(matrixtest_t, TOLUENE2.0$stackair, decay2)
test_toluene <- as.data.frame(cbind(spdf2$epr_number_TYPE, test_toluene))
names(test_toluene) <-  c("epr_number_TYPE", "epdecay_1km", "epdecay_5km","epdecay_10km")



write.csv(test_toluene, "~/Desktop/TRI/TOLUENE_decay.csv") #all addresses
write.csv(test_benzene, "~/Desktop/TRI/BENZENE_decay.csv") #all addresses
write.csv(test_xylene, "~/Desktop/TRI/XYLENE_decay.csv") #all addresses
write.csv(test_eb, "~/Desktop/TRI/TOLUENE_decay.csv") #all addresses


