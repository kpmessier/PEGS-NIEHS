# Join ACAG PM2.5 data to the PEGS cohort data

PEGS3 <- read.csv("~/Desktop/PEGS_CensusTractAddresses.csv")
PEGS2 <- subset(PEGS3, !is.na(PEGS3$geo_latitude))


library(ncdf4)           
library(raster)
library(reshape2)
library(ggplot2)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(sp)
#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
PEGS2$epr_number_TYPE <- ifelse(PEGS2$geo_study_event == "Exposome Part A - Current Address", paste0(PEGS2$epr_number, "_2"),paste0(PEGS2$epr_number, "_1"))


nc_map <-rgdal::readOGR(dsn="~/Documents/tl_2016_37_place")

nc_map2 <- spTransform(nc_map, CRS("+proj=longlat +datum=NAD83 +no_defs"))
#spdf <- SpatialPointsDataFrame(coords = monitor.locs, data = monitor.locs,
#                              proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

extent(nc_map2)
acag <- read.csv( "~/Desktop/PEGS_ACAG.csv")


acag2 <- merge(PEGS2, acag, by.x="epr_number_TYPE", by.y="epr_number_TYPE", all.y=T)
# longitude and latitude (x,y) coordinates

acag3 <- st_as_sf(acag2, coords=c("geo_longitude", "geo_latitude"))

no2 <- read.csv("~/Desktop/PEGS_AirPollutionExposure/no2_geocoded.csv")
no2 <- subset(no2, !is.na(no2$geo_latitude))

#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
no2$epr_number_TYPE <- ifelse(no2$geo_study_event == "Exposome Part A - Current Address", paste0(no2$epr_number, "_2"),paste0(no2$epr_number, "_1"))
no23 <- st_as_sf(no2, coords=c("geo_longitude", "geo_latitude"))
no23 <- as(no23, 'Spatial')

no23 <- crop(no23, extent(nc_map2)) # transform CRS


acag3 <- as(acag3, 'Spatial')

acag3 <- crop(acag3, extent(nc_map2)) # transform CRS
library(leaflet)
library(maps)

pal <-  colorBin("magma", domain = 0:10)
hist(acag3$X2000_c_SO4.x)
leaflet(acag3) %>% addTiles() %>%
  addCircleMarkers(
    color = ~pal(X2000_c_SO4.x),
    stroke = FALSE, fillOpacity = 0.5
  ) %>% addLegend("bottomright", pal = pal, values = ~X2000_c_SO4.x,
          title = "Est. SO4 Share 2000",
          labFormat = labelFormat(prefix = "$"),
          opacity = 1
)
pal <-  colorBin("magma", domain = 0:100)

leaflet(acag3) %>% addTiles() %>%
  addCircleMarkers(
    color = ~pal(X2016_p_SO4.x),
    stroke = FALSE, fillOpacity = 0.5
  ) %>% addLegend("bottomright", pal = pal, values = ~X2016_p_SO4.x,
                  title = "Est. SO4 Share 2016",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 1
  )


pal <-  colorNumeric("magma", domain = 0:10)
no24 <- subset(no23, no23$X2008 <=10)
leaflet(no24) %>% addTiles() %>%
  addCircleMarkers(radius=3,
    color = ~pal(X2008),
    stroke = FALSE, fillOpacity = 0.5
  ) %>% addLegend("bottomright", pal = pal, values = ~X2008,
                  title = "Est. NO2 Conc. \n 2008 (\u00b5g/m\u00b3)",
                  labFormat = labelFormat(prefix = ""),
                  opacity = 1)

range(as.numeric(no2$X2008), na.rm=T)







