##### Geocoding #####
## import the geocodes from the pegs file
geocodes <- read.csv("/Volumes/PEGS/Data_Freezes/freeze_v1/GIS/geo_addresses_01oct20_fmtd_v1.csv")

write.csv(geocodes, file ="~/Desktop/geocodes_addresses.csv",row.names=F)

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



library(rgdal)
library(sp)
library(sf)
library(rgeos)
library(tidyr)

#remove missing addresses
geocodes2 <- subset(geocodes, !is.na(geocodes$longitude)) #12448


#some subjects have more than 1 address, need to modify our epr number so these can be dealt with
geocodes2$epr_number_TYPE <- ifelse(geocodes2$geo_study_event == "Exposome Part A - Current Address", paste0(geocodes2$epr_number, "_2"),paste0(geocodes2$epr_number, "_1"))

geocodes3 <- geocodes2[,c(1,2,16,17,19:21)]
sep1 <- subset(geocodes3, geocodes3$geo_study_event == "Exposome Part A - Current Address")
sep2 <- subset(geocodes3, geocodes3$geo_study_event != "Exposome Part A - Current Address")

together <- merge(sep2, sep1, by.x="epr_number", by.y="epr_number", all.x=T)


together$identical <- ifelse(together$round_lat.x == together$round_lat.y & together$round_long.x == together$round_long.y,1,0)
table(together$identical)

#write.csv(together, file ="~/Desktop/geocodes_addresses2check.csv",row.names=F)
geocodes_check <- read.csv("~/Desktop/geocodes_addresses2check.csv")


flipping <- subset(geocodes_check, geocodes_check$problem == "flip lat.y and long.y"|geocodes_check$problem == "check")

geocodes$problem <- ifelse(geocodes$epr_number %in% flipping$epr_number,"flag","ok")
table(geocodes$problem)

#write.csv(geocodes, file ="~/Desktop/geocodes_corrected2.csv",row.names=F)

# What I did here - I switched the latitude and longitude of a bunch of incorrectly labeled locations
# I also had a couple that were missing negative signs. Obviously, we want to get rid of those. 

#so now, I'm going to read in the refabbed data. This needs to be saved in a safe place because it took forever.

geocodes <- read.csv("~/Desktop/geocodes_corrected.csv")

#double check that everything is numeric.
geocodes$latitude <- as.numeric(ifelse(is.na(geocodes$state), NA, geocodes$geo_latitude))
geocodes$longitude<- as.numeric(ifelse(is.na(geocodes$state), NA, geocodes$geo_longitude))

geocodes$round_lat <- round(geocodes$latitude, 2)
geocodes$round_long <- round(geocodes$longitude,2)

### going back through to remove duplicate addresses ### (I know I already used this code above)


geocodes3 <- geocodes[,c(1,2,7,17:18,20:21)]
sep1 <- subset(geocodes3, geocodes3$geo_study_event == "Exposome Part A - Current Address")
sep2 <- subset(geocodes3, geocodes3$geo_study_event != "Exposome Part A - Current Address")

together <- merge(sep2, sep1, by.x="epr_number", by.y="epr_number", all.x=T)


together$identical <- ifelse(together$round_lat.x == together$round_lat.y & together$round_long.x == together$round_long.y,1,0)
table(together$identical)

identicaldata <- subset(together, together$identical == 1)


geocodes$identicaladdress <- ifelse(geocodes$epr_number %in% identicaldata$epr_number,1,0)

table(geocodes$identicaladdress)


geocodes$identical_drop <- ifelse(geocodes$identicaladdress==1 & geocodes$geo_study_event == "Exposome Part A - Current Address",1,0)

geocodes_final_ml <- subset(geocodes, geocodes$identical_drop != 1)

### now I'll choose which rows to keep?

write.csv(geocodes_final_ml, file ="~/Desktop/PEGS_addresses4ML.csv",row.names=F)

