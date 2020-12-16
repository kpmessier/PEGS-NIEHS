##### Geocoding #####
## import the geocodes from the pegs file
geocodes <- read.csv("/Volumes/PEGS/Data_Freezes/freeze_v1/GIS/geo_addresses_01oct20_fmtd_v1.csv")


#geocodes <- read.csv("~/Desktop/geocodes_byhand.csv") # these are the ones that I had to do by hand

#names of dataset
names(geocodes)
geocodes$state <- as.factor(geocodes$geo_state)
#they have a weird structure - make sure everything is correct in the state column especially
geocodes$state <- ifelse((geocodes$geo_state == "SKIPPED" | geocodes$geo_state == "AE"), NA, geocodes$geo_state)
geocodes$state <- as.factor(ifelse(geocodes$state == "MISSING", NA, geocodes$state))

table(geocodes$state)
levels(geocodes$state)
table(geocodes$geo_country)

geocodes$country <-ifelse(is.na(geocodes$state), NA, geocodes$geo_country)
table(geocodes$country)

#double check that everything is numeric.
geocodes$latitude <- as.numeric(ifelse(is.na(geocodes$state), NA, geocodes$geo_latitude))
geocodes$longitude<- as.numeric(ifelse(is.na(geocodes$state), NA, geocodes$geo_longitude))
geocodes$location <- paste0(geocodes$latitude, geocodes$longitude)
check1 <- unique(geocodes$location)
#geocodes$check <- ifelse(geocodes$location %in% check1, 1, 0)
library(rgdal)
library(sp)
library(sf)
library(rgeos)

#remove missing addresses
geocodes2 <- subset(geocodes, !is.na(geocodes$longitude)) #12448

#convert to spatial files
geocodes2 <- st_as_sf(geocodes2, coords = c('longitude','latitude'), crs ="+proj=longlat +datum=NAD83 +no_defs")

geocodes2 <- sf:::as_Spatial(geocodes2)

###### import census tract files ######

geocodes3 <- geocodes2 

#pull all of the census tract files for each state
folders1 <- list.files(path="~/Downloads/censustracts/", all.files=T)[5:59]
geocodest <- NA



#### connect the geocodes to the census tract

for (i in 1:55){
  name1 <- paste0("~/Downloads/censustracts/",folders1[i])
  print(name1)
  censustract <- rgdal::readOGR(dsn=name1)
 # censustract <- spTransform(censustract, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
  #censustract2 <- raster:::extract(censustract, geocodes3)
  censustract2 <- over(geocodes3,censustract)
  geocodes3@data$STATEFP <- censustract2$STATEFP
  geocodes3@data$COUNTYFP <- censustract2$COUNTYFP
  geocodes3@data$TRACTCE <- censustract2$TRACTCE
  geocodes3@data$GEOID <- censustract2$GEOID
  geocodes3@data$NAME <- censustract2$NAME
  geocodes3@data$NAMELSAD <- censustract2$NAMELSAD
  geocodes3@data$MTFCC <- censustract2$MTFCC
  geocodes3@data$FUNCSTAT <- censustract2$FUNCSTAT
  geocodes3@data$ALAND <- censustract2$ALAND
  geocodes3@data$AWATER <- censustract2$AWATER
  geocodes3@data$INTPTLAT <- censustract2$INTPTLAT
  geocodes3@data$INTPTLON <- censustract2$NTPTLON
  geocodesx2 <- as.data.frame(geocodes3@data)
  geocodesx <- subset(geocodesx2, !is.na(geocodesx2$STATEFP))
  geocodest <- rbind(geocodest, geocodesx)
}

#check how many need to be added by hand
completed_id <- unique(geocodest$epr_number)
all_id <- unique(geocodes3@data$epr_number)

missing_id <- setdiff(all_id,completed_id)

miss_check <- subset(geocodes3, geocodes3@data$epr_number %in% missing_id)

miss_check2 <- as.data.frame(miss_check)

write.csv(geocodest, file ="~/Desktop/EPR_geocodes_censustract.csv") #addresses that merged ok
write.csv(miss_check2, file="~/Desktop/EPR_geocodes_nocensustract.csv") #addresses that needed to be handmerged

censusbyhand <- read.csv("~/Desktop/EPR_geocodes_censusbyhand.csv")[,2:29] #import hand-merged addresses

geocodest <- rbind(geocodest, censusbyhand) #add hand-merged addresses to the rest
#### Air Pollution ####

caces <- read.csv("~/Documents/caces_tracts.csv") #from all US states and territories

# double check the names
names(caces) <- c("fips" ,  "pollutant" , "year" , "pred_wght"  ,"state_abbr" ,"latitude" ,  "longitude"       )

#make a simpler dataframe to connect to the addresses
cacesx <- unique(subset(caces, select=c("fips","state_abbr","latitude","longitude")))

#convert to spatial
caces2 <- st_as_sf(cacesx, coords = c('longitude','latitude'), crs ="+proj=longlat +datum=NAD83 +no_defs")

caces2 <- sf:::as_Spatial(caces2)
#didn't end up using this vector
#states <- c("AL","AK","AZ","AR","CA", "CO", "CT", "DE", "FL","GA","HI", "ID","IL","IN","IA", "KS","KY","LA","ME","MD","MA","MI",
 #           "MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX", "UT","VT", "VA","WA","WV","WI","WY","DC")
cacest <- NULL #empty file to store results
for (i in 1:55){
  name1 <- paste0("~/Downloads/censustracts/",folders1[i])
  print(name1)
  censustract <- rgdal::readOGR(dsn=name1)
 # censustract <- spTransform(censustract, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")) #don't need this
  print(proj4string(censustract))
  print(proj4string(caces2))
  censustract2 <- raster:::extract(censustract, caces2) #works better than over. 
  #censustract2 <- over(caces2,censustract)
  caces2@data$STATEFP <- censustract2$STATEFP
  caces2@data$COUNTYFP <- censustract2$COUNTYFP
  caces2@data$TRACTCE <- censustract2$TRACTCE
  caces2@data$GEOID <- censustract2$GEOID
  caces2@data$NAME <- censustract2$NAME
  caces2@data$NAMELSAD <- censustract2$NAMELSAD
  caces2@data$MTFCC <- censustract2$MTFCC
  caces2@data$FUNCSTAT <- censustract2$FUNCSTAT
  caces2@data$ALAND <- censustract2$ALAND
  caces2@data$AWATER <- censustract2$AWATER
  caces2@data$INTPTLAT <- censustract2$INTPTLAT
  caces2@data$INTPTLON <- censustract2$NTPTLON
  caces2x2 <- as.data.frame(caces2@data)
  cacest <- rbind(cacest, caces2x2)
}

cacest2 <- subset(cacest, !is.na(cacest$NAME)) # remove excess values

#make dataframe


caces <- as.data.frame(caces)


library(tidyr)


#organize into subsets of pollutants, 
caces$pollutant <- as.factor(caces$pollutant)

caces$state_abbr <- as.factor(caces$state_abbr)

table(caces$state_abbr)
#empty list for loop
caces_list <- list()


names(caces)
names(cacest2)
names(geocodest)

 for (i in 1:6){
   #name for each dataset
  name <- paste('censtrc_', levels(caces$pollutant)[i],sep='')
  #subset by pollutant type
  item <- as.data.frame(subset(caces, caces$pollutant == levels(caces$pollutant)[i]))
  #merge pollutant with the caces and census tracts
  item2 <- merge(item, cacest2, by.x=c("fips", "state_abbr"), by.y=c("fips", "state_abbr"), all.y=T)
  #merge caces in census tracts with addresses in census tracts
  item3 <- merge(geocodest,item2, by.x=c("STATEFP" , "COUNTYFP","TRACTCE","GEOID", "NAME" ,"NAMELSAD" , "MTFCC","FUNCSTAT", "ALAND", "AWATER", "INTPTLAT"), 
                 by.y=c("STATEFP" , "COUNTYFP","TRACTCE","GEOID", "NAME" ,"NAMELSAD" , "MTFCC","FUNCSTAT", "ALAND", "AWATER", "INTPTLAT"),all.x=T)
  #remove excess addresses
  item4 <- unique(item3)
  library(dplyr)
  #flip it so each location has a row (or at least each address by survey source)
  item5 <- item4 %>% group_by(fips) %>% 
    pivot_wider(names_from = year, values_from =  pred_wght)
  item6 <- unique(item5)
  #remove empty row
  caces_list[[name]] <- as.data.frame(item6)[,1:49]
 }


#write to csv file

col_order <- c("STATEFP", "COUNTYFP" , "TRACTCE" ,"GEOID",                
               "NAME", "NAMELSAD","MTFCC" ,"FUNCSTAT" ,            
               "ALAND" ,"AWATER","INTPTLAT" ,"epr_number" ,          
               "geo_study_event" , "geo_date_collected" , "geo_is_po_box_derived", "geo_address"  ,        
               "geo_city" ,"geo_state", "geo_zip_code" ,"geo_territory"    ,    
               "geo_country","geo_longitude","geo_latitude", "geo_geocode_quality"  ,
               "state" , "country" ,"location" ,           
               "fips" , "state_abbr" , "pollutant" ,"latitude"  ,           
               "longitude" , "2000",  "2001", "2002" ,                
               "2003", "2004" ,  "2005" , "2006"  ,               
               "2007","2008", "2009", "2010" ,                
               "2011","2012", "2013", "2014",                 
               "2015" )
so2_census <- caces_list$censtrc_so2
so2_census2 <- so2_census[, col_order]
write.csv(so2_census2, file ="~/Desktop/so2_geocoded.csv")

o3_census <- caces_list$censtrc_o3
o3_census2 <- o3_census[, col_order]
write.csv(o3_census2, file ="~/Desktop/o3_geocoded.csv")

pm10_census <- caces_list$censtrc_pm10
pm10_census2 <- pm10_census[, col_order]
write.csv(pm10_census2, file ="~/Desktop/pm10_geocoded.csv")

pm25_census <- caces_list$censtrc_pm25
pm25_census2 <- pm25_census[, col_order]
write.csv(pm25_census2, file ="~/Desktop/pm25_geocoded.csv")

no2_census <- caces_list$censtrc_no2
no2_census2 <- no2_census[, col_order]
write.csv(no2_census2, file ="~/Desktop/no2_geocoded.csv")

co_census <- caces_list$censtrc_co
co_census2 <- co_census[, col_order]
write.csv(co_census2, file ="~/Desktop/co_geocoded.csv")



head(co_census2)

levels(as.factor(co_census$geo_geocode_quality))


library(tidycensus)

states <- c("AL","AK","AZ","AR","CA", "CO", "CT", "DE", "FL","GA","HI", "ID","IL","IN","IA", "KS","KY","LA","ME","MD","MA","MI",
"MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX", "UT","VT", "VA","WA","WV","WI","WY","DC")

census_api_key("6b04c73f35db65b2e548eb94811605430b9141e6", overwrite = TRUE)
readRenviron("~/.Renviron")
v10 <- load_variables(2010, "sf1", cache = TRUE)
v15 <- load_variables(2017, "acs5", cache = TRUE)
#names_v15 <- v15[c(3,362,361,458,614,722,2352,6577,6787,6894,6933,7785,11960,11961,13750,18076,18117,18373,18558,18569),]
#names_v10 <- v10[c(1,3,6,48,149,719,724,1547,2828),]
names_v15 <- v15[c(941,942,6235,6283,6940,8130,11041,11042,11057,11957,11978,11981,11979,11980,11982,11995,18380,18381,18568,18567,18566,18565,18564, 18569,18851),]
names_v10 <- v10[c(1,3,6,23,48,141,148,155,163,638,640,643,650,652),]

values_acs <- as.vector(names_v15[,1])
values_cens <- names_v10[,1]

acs17 <-get_acs(geography = "tract", variables=c("B01003_001","B25035_001","B25064_001","B19080_001","B19083_001" ,"B19057_001"),
                cache_table = FALSE, year = 2017,
                output = "wide", state = "HI", geometry = FALSE, survey = "acs5")

census <-get_decennial(geography = "tract", variables=c("P001001", "P003006", "P003008","H002002","H002005"), year = 2010,
                       output = "wide", state = "HI", geometry = FALSE)


names(acs17) <- c("geoid" ,"NAME","Population_E", "Population_M", "MedianYearBuilt_E", "MedianYearBuilt_M" ,"MedGrossRent_E", "MedGrossRent_M", "LowestIncQuint_E", "LowestIncQuint_M", "GINI_E" ,"GINI_M", "PublicAssist_E", "PublicAssist_M")

names(census) <- c("geoid", "NAME", "Population", "Pop_PacIsl", "TwoplRaces", "UrbanPop" ,"RuralPop")


censusdata <- merge(acs17, census, by.x= c("geoid","NAME"),by.y= c("geoid","NAME"))


ntm_pointscensus <- merge(df4, censusdata, by.x="geoid", by.y="geoid", all.x=T)
