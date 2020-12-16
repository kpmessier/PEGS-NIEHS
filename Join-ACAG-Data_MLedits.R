# Join ACAG PM2.5 data to the PEGS cohort data

PEGS3 <- read.csv("~/Desktop/PEGS_CensusTractAddresses.csv")
PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude))


library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)

#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
PEGS2$epr_number_TYPE <- ifelse(PEGS2$geo_study_event == "Exposome Part A - Current Address", paste0(PEGS2$epr_number, "_2"),paste0(PEGS2$epr_number, "_1"))




# Some fake monitor coordinate locations
#monitor.locs <- data.frame("Long" = c(-79,-100),"Lat" = c(35.9,36))

monitor.locs <- data.frame("Long" = PEGS2$geo_longitude,"Lat" = PEGS2$geo_latitude)
row.names(monitor.locs) <- PEGS2$epr_number_TYPE

#spdf <- SpatialPointsDataFrame(coords = monitor.locs, data = monitor.locs,
#                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))



#ok, so one group is concentration and the other is percent...this will be helpful later on. 
#test1 <- raster:::raster("/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/BC/GWRwSPEC_BC_NA_200001_200012.nc")

#test2 <- raster:::raster("/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/BC/GWRwSPEC.HEI_BCp_NA_201701_201712-wrtSPECtotal.nc")

##### PM2.5  

PM25.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/PM25/"

# Get Raster data filenames/paths

PM25.filenames_perc <- list.files(path=PM25.dir,
                                 pattern="PM25p", full.names=T)

PM25.filenames_conc <- list.files(path=PM25.dir,
                                 pattern="PM25_", full.names=T)

### create a function to extract the year from each filename and then add the pollutant, 
year_pm25 <- function(x){
  ends_with <- "12-RH35.nc"
  starts_with <- "\\d{4}"
  q <- str_extract(as.character(unlist(strsplit(x, paste0(ends_with, "$")))), paste0(starts_with, "$"))
  paste0(q, "_pm25")
}
#use this to create a list of names
names_pm25 <- unlist(lapply(PM25.filenames_conc, year_pm25))

#rasterize the files from ACAG
PM25.bricks_c <- lapply(PM25.filenames_conc,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.PM25 <- lapply(PM25.bricks_c,raster::extract,spdf)


# name the columns by the year and pollutant
names(monitors.PM25) <-names_pm25
pm25_acag <- NULL

for (i in 1:length(monitors.PM25))
{
  item <- unlist(monitors.PM25[i])
  pm25_acag <- as.data.frame(cbind(pm25_acag, item))
  names(pm25_acag) <- names(monitors.PM25)[1:i]
  
}
pm25_acag$epr_number_TYPE <- PEGS2$epr_number_TYPE

##### Sulfate  

SO4.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/SO4/"


# Get Raster data filenames/paths

SO4.filenames_perc <- list.files(path=SO4.dir,
                                pattern="SO4p", full.names=T)

SO4.filenames_conc <- list.files(path=SO4.dir,
                                pattern="SO4_", full.names=T)

# Get raster bricks for each year using lapply
SO4.bricks_p <- lapply(SO4.filenames_perc,raster::brick,nl = 1)

SO4.bricks_c <- lapply(SO4.filenames_conc,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.SO4p <- lapply(SO4.bricks_p,raster::extract,monitor.locs)
monitors.SO4c <- lapply(SO4.bricks_c,raster::extract,monitor.locs)


### create a function to extract the year from each filename and then add the pollutant, 
year_so4 <- function(x){
  ends_with <- "12.nc"
  ends_with2 <- "12-wrtSPECtotal.nc"
  starts_with <- "\\d{4}"
  if (str_detect(x, paste0(ends_with, "$")) )
      {
      q <- str_extract(as.character(unlist(strsplit(x, paste0(ends_with, "$")))), paste0(starts_with, "$"))
      q <-paste0(q, "_c")
  }
  if (str_detect(x, paste0(ends_with2, "$")) )
  {
    q <- str_extract(as.character(unlist(strsplit(x, paste0(ends_with2, "$")))), paste0(starts_with, "$"))
    q <- paste0(q, "_p")
  }
  paste0(q, "_SO4")
}
#use this to create a list of names
names_so4c <- unlist(lapply(SO4.filenames_conc, year_so4))
names_so4p <- unlist(lapply(SO4.filenames_perc, year_so4))

#name the columns by the year and pollutant
names(monitors.SO4c) <- names_so4c
SO4c_acag <- NULL

for (i in 1:length(monitors.SO4c))
{
  item <- unlist(monitors.SO4c[i])
  SO4c_acag <- as.data.frame(cbind(SO4c_acag, item))
  names(SO4c_acag) <- names(monitors.SO4c)[1:i]
  
}
SO4c_acag$epr_number_TYPE <- PEGS2$epr_number_TYPE


names(monitors.SO4p) <-names_so4p
SO4p_acag <- NULL

for (i in 1:length(monitors.SO4p))
{
  item <- unlist(monitors.SO4p[i])
  SO4p_acag <- as.data.frame(cbind(SO4p_acag, item))
  names(SO4p_acag) <- names(monitors.SO4p)[1:i]
  
}
SO4p_acag$epr_number_TYPE <- PEGS2$epr_number_TYPE

##### Organic Matter  


OM.dir <-  "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/OM/"

# Get Raster data filenames/paths

OM.filenames_perc <- list.files(path=OM.dir,
                                 pattern="OMp", full.names=T)

OM.filenames_conc <- list.files(path=OM.dir,
                                 pattern="OM_", full.names=T)

# Get raster bricks for each year using lapply
OM.bricks_p <- lapply(OM.filenames_perc,raster::brick,nl = 1)

OM.bricks_c <- lapply(OM.filenames_conc,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.OMp <- lapply(OM.bricks_p,raster::extract,monitor.locs)
monitors.OMc <- lapply(OM.bricks_c,raster::extract,monitor.locs)

### create a function to extract the year from each filename and then add the pollutant, 
year_om <- function(x){
  ends_with <- "12.nc"
  ends_with2 <- "12-wrtSPECtotal.nc"
  starts_with <- "\\d{4}"
  if (str_detect(x, paste0(ends_with, "$")) )
  {
    q <- str_extract(as.character(unlist(strsplit(x, paste0(ends_with, "$")))), paste0(starts_with, "$"))
    q <-paste0(q, "_c")
  }
  if (str_detect(x, paste0(ends_with2, "$")) )
  {
    q <- str_extract(as.character(unlist(strsplit(x, paste0(ends_with2, "$")))), paste0(starts_with, "$"))
    q <- paste0(q, "_p")
  }
  paste0(q, "_OM")
}

#use this to create a list of names
names_OMc <- unlist(lapply(OM.filenames_conc, year_so4))
names_OMp <- unlist(lapply(OM.filenames_perc, year_so4))

#name the columns by the year and pollutant
names(monitors.OMc) <- names_OMc
OMc_acag <- NULL

for (i in 1:length(monitors.OMc))
{
  item <- unlist(monitors.OMc[i])
  OMc_acag <- as.data.frame(cbind(OMc_acag, item))
  names(OMc_acag) <- names(monitors.OMc)[1:i]
  
}
OMc_acag$epr_number_TYPE <- PEGS2$epr_number_TYPE


names(monitors.OMp) <-names_OMp
OMp_acag <- NULL

for (i in 1:length(monitors.OMp))
{
  item <- unlist(monitors.OMp[i])
  OMp_acag <- as.data.frame(cbind(OMp_acag, item))
  names(OMp_acag) <- names(monitors.OMp)[1:i]
  
}
OMp_acag$epr_number_TYPE <- PEGS2$epr_number_TYPE







##### Nitrate  

NIT.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/NIT/"

# Get Raster data filenames/paths

NIT.filenames_perc <- list.files(path=NIT.dir,
                                pattern="NITp", full.names=T)

NIT.filenames_conc <- list.files(path=NIT.dir,
                                pattern="NIT_", full.names=T)

# Get raster bricks for each year using lapply
NIT.bricks_p <- lapply(NIT.filenames_perc,raster::brick,nl = 1)

NIT.bricks_c <- lapply(NIT.filenames_conc,raster::brick,nl = 1)

# Extract the data at monitor locations for each year
monitors.NITp <- lapply(NIT.bricks_p,raster::extract,monitor.locs)
monitors.NITc <- lapply(NIT.bricks_c,raster::extract,monitor.locs)




### create a function to extract the year from each filename and then add the pollutant, 
year_NIT <- function(x){
  ends_with <- "12.nc"
  ends_with2 <- "12-wrtSPECtotal.nc"
  starts_with <- "\\d{4}"
  if (str_detect(x, paste0(ends_with, "$")) )
  {
    q <- str_extract(as.character(unlist(strsplit(x, paste0(ends_with, "$")))), paste0(starts_with, "$"))
    q <-paste0(q, "_c")
  }
  if (str_detect(x, paste0(ends_with2, "$")) )
  {
    q <- str_extract(as.character(unlist(strsplit(x, paste0(ends_with2, "$")))), paste0(starts_with, "$"))
    q <- paste0(q, "_p")
  }
  paste0(q, "_NIT")
}
#use this to create a list of names
names_NITc <- unlist(lapply(NIT.filenames_conc, year_NIT))
names_NITp <- unlist(lapply(NIT.filenames_perc, year_NIT))

#name the columns by the year and pollutant
names(monitors.NITc) <- names_NITc
NITc_acag <- NULL

for (i in 1:length(monitors.NITc))
{
  item <- unlist(monitors.NITc[i])
  NITc_acag <- as.data.frame(cbind(NITc_acag, item))
  names(NITc_acag) <- names(monitors.NITc)[1:i]
  
}
NITc_acag$epr_number_TYPE <- PEGS2$epr_number_TYPE


names(monitors.NITp) <-names_NITp
NITp_acag <- NULL

for (i in 1:length(monitors.NITp))
{
  item <- unlist(monitors.NITp[i])
  NITp_acag <- as.data.frame(cbind(NITp_acag, item))
  names(NITp_acag) <- names(monitors.NITp)[1:i]
  
}
NITp_acag$epr_number_TYPE <- PEGS2$epr_number_TYPE


##### Black Carbon  

BC.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/BC/"
# Get Raster data filenames/paths
#BC.filenames <- list.files(path = BC.dir, full.names = TRUE)

BC.filenames_perc <- list.files(path=BC.dir,
                                pattern="BCp", full.names=T)
BC.filenames_conc <- list.files(path=BC.dir,
                                pattern="BC_", full.names=T)


# Get raster bricks for each year using lapply
BC.bricks_p <- lapply(BC.filenames_perc,raster::brick,nl = 1)
BC.bricks_c <- lapply(BC.filenames_conc,raster::brick,nl = 1)


# Extract the data at monitor locations for each year
monitors.BCp <- lapply(BC.bricks_p,raster::extract,monitor.locs)
monitors.BCc <- lapply(BC.bricks_c,raster::extract,monitor.locs)





### create a function to extract the year from each filename and then add the pollutant, 
year_BC <- function(x){
  ends_with <- "12.nc"
  ends_with2 <- "12-wrtSPECtotal.nc"
  starts_with <- "\\d{4}"
  if (str_detect(x, paste0(ends_with, "$")) )
  {
    q <- str_extract(as.character(unlist(strsplit(x, paste0(ends_with, "$")))), paste0(starts_with, "$"))
    q <-paste0(q, "_c")
  }
  if (str_detect(x, paste0(ends_with2, "$")) )
  {
    q <- str_extract(as.character(unlist(strsplit(x, paste0(ends_with2, "$")))), paste0(starts_with, "$"))
    q <- paste0(q, "_p")
  }
  paste0(q, "_BC")
}
#use this to create a list of names
names_BCc <- unlist(lapply(BC.filenames_conc, year_BC))
names_BCp <- unlist(lapply(BC.filenames_perc, year_BC))

#name the columns by the year and pollutant
names(monitors.BCc) <- names_BCc
BCc_acag <- NULL

for (i in 1:length(monitors.BCc))
{
  item <- unlist(monitors.BCc[i])
  BCc_acag <- as.data.frame(cbind(BCc_acag, item))
  names(BCc_acag) <- names(monitors.BCc)[1:i]
  
}
BCc_acag$epr_number_TYPE <- PEGS2$epr_number_TYPE


names(monitors.BCp) <-names_BCp
BCp_acag <- NULL

for (i in 1:length(monitors.BCp))
{
  item <- unlist(monitors.BCp[i])
  BCp_acag <- as.data.frame(cbind(BCp_acag, item))
  names(BCp_acag) <- names(monitors.BCp)[1:i]
  
}
BCp_acag$epr_number_TYPE <- PEGS2$epr_number_TYPE



##### Ammonium ion  

NH4.dir <- "/Volumes/SHAG/EPR Geospatial Data/ACAG/stetson.phys.dal.ca/Aaron/V4NA02/Annual/netcdf/NH4/"

# Get Raster data filenames/paths


NH4.filenames_perc <- list.files(path=NH4.dir,
                                pattern="NH4p", full.names=T)
NH4.filenames_conc <- list.files(path=NH4.dir,
                                pattern="NH4_", full.names=T)


# Get raster bricks for each year using lapply
NH4.bricks_p <- lapply(NH4.filenames_perc,raster::brick,nl = 1)
NH4.bricks_c <- lapply(NH4.filenames_conc,raster::brick,nl = 1)


# Extract the data at monitor locations for each year
monitors.NH4p <- lapply(NH4.bricks_p,raster::extract,monitor.locs)
monitors.NH4c <- lapply(NH4.bricks_c,raster::extract,monitor.locs)


year_NH4 <- function(x){
  ends_with <- "12.nc"
  ends_with2 <- "12-wrtSPECtotal.nc"
  starts_with <- "\\d{4}"
  if (str_detect(x, paste0(ends_with, "$")) )
  {
    q <- str_extract(as.character(unlist(strsplit(x, paste0(ends_with, "$")))), paste0(starts_with, "$"))
    q <-paste0(q, "_c")
  }
  if (str_detect(x, paste0(ends_with2, "$")) )
  {
    q <- str_extract(as.character(unlist(strsplit(x, paste0(ends_with2, "$")))), paste0(starts_with, "$"))
    q <- paste0(q, "_p")
  }
  paste0(q, "_NH4")
}
#use this to create a list of names
names_NH4c <- unlist(lapply(NH4.filenames_conc, year_NH4))
names_NH4p <- unlist(lapply(NH4.filenames_perc, year_NH4))

#name the columns by the year and pollutant
names(monitors.NH4c) <- names_NH4c
NH4c_acag <- NULL

for (i in 1:length(monitors.NH4c))
{
  item <- unlist(monitors.NH4c[i])
  NH4c_acag <- as.data.frame(cbind(NH4c_acag, item))
  names(NH4c_acag) <- names(monitors.NH4c)[1:i]
  
}
NH4c_acag$epr_number_TYPE <- PEGS2$epr_number_TYPE


names(monitors.NH4p) <-names_NH4p
NH4p_acag <- NULL

for (i in 1:length(monitors.NH4p))
{
  item <- unlist(monitors.NH4p[i])
  NH4p_acag <- as.data.frame(cbind(NH4p_acag, item))
  names(NH4p_acag) <- names(monitors.NH4p)[1:i]
  
}
NH4p_acag$epr_number_TYPE <- PEGS2$epr_number_TYPE



### Create a single file....

file1 <- merge(NH4p_acag, NH4c_acag , by.x = "epr_number_TYPE", by.y = "epr_number_TYPE")
file2 <- merge(BCp_acag, BCc_acag , by.x = "epr_number_TYPE", by.y = "epr_number_TYPE")
file3 <- merge(NITp_acag, NITp_acag , by.x = "epr_number_TYPE", by.y = "epr_number_TYPE")
file4 <- merge(OMp_acag, OMc_acag, by.x = "epr_number_TYPE", by.y = "epr_number_TYPE")
file5 <- merge(SO4p_acag, SO4c_acag, by.x = "epr_number_TYPE", by.y = "epr_number_TYPE")

filea <- merge(file1, file2, by.x = "epr_number_TYPE", by.y = "epr_number_TYPE")
fileb <- merge(filea, file3, by.x = "epr_number_TYPE", by.y = "epr_number_TYPE")
filec <- merge(fileb, file4, by.x = "epr_number_TYPE", by.y = "epr_number_TYPE")
filed <- merge(filec, file5, by.x = "epr_number_TYPE", by.y = "epr_number_TYPE")
filef <- merge(filed, pm25_acag, by.x = "epr_number_TYPE", by.y = "epr_number_TYPE")

#get the epr number back associated with the item so that it can be tied to a subject
filef$epr_number <- str_replace(str_replace(filef$epr_number_TYPE, paste0("_1", "$"),""), paste0("_2", "$"),"")

#move epr number so it's the first column in the dataframe
fileg <- filef %>% relocate(epr_number, .before = epr_number_TYPE)

fileg <- as.data.frame(fileg)

write.csv(fileg, file = "~/Desktop/PEGS_ACAG.csv", row.names=F)
