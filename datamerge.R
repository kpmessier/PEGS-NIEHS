library(stringr)
files.dir <- "/Volumes/shag/loweme/datamergestuff" #creating a link to the directory where the files are stored
#you'll need to be connected through cisco to the server for this to work

filenames_all <- list.files(path=files.dir, full.names=T)  #this lists all the files in the folder so we don't have to type them
qfile <- list()
q <- list()
for (i in 1:length(filenames_all)){
  #for (i in 1:3){
  name1 <- (filenames_all[i]) #extracts the individual file path
  print(name1)
  q[[i]]<- unlist(strsplit(unlist(strsplit(name1, "/Volumes/shag/loweme/datamergestuff/"))[2], " copy.csv"))
  #extracts the simplest name of the file
  qfile[[i]] <- read.csv(name1) #reads in the file
}
namesforfileorganization <- unlist(q)

### Ok, so we've imported the data. each dataframe is an item of this list. So, to pull them out we'll do the following:

namesforfileorganization[1] #print the correct name
co_geocoded <- qfile[[1]] # copy and paste the correct name for the file and then assign it to the associated dataframe in the list

## if you have a file that doesn't have epr_number_TYPE this is how I created it. That will be the value that you'll use to merge the files
#PEGS2$epr_number_TYPE <- ifelse(PEGS2$geo_study_event == "Exposome Part A - Current Address", paste0(PEGS2$epr_number, "_2"),paste0(PEGS2$epr_number, "_1"))


#In terms of format, I want a row per epr_number_TYPE. The exposures should be labeled as the exposure_year.

#you'll have to probably rename some columns. 

#also, you'll want to make sure that you don't have duplicate columns.


### GO TEAM ! ###

### Also, please message me if you have any questions or get stuck on anything 


