data1  <- read.csv("/Volumes/PEGS/Data_Freezes/freeze_v1/Surveys/Health_and_Exposure/healthexposure_02jan20_fmtd_v1.csv")
geocodes <- read.csv("/Volumes/PEGS/Data_Freezes/freeze_v1/GIS/geo_addresses_01oct20_fmtd_v1.csv")

demo <- read.csv("/Volumes/PEGS/Data_Freezes/freeze_v1/Map/bcbb_map_02jan20_fmtd_v1.csv")
#diabetes <- read.csv("/Volumes/EPR/data_S3/Surveys/Diabetes Screener/diabetes_31mar20_v02_fmts.csv")
library(tidyverse)
demo2 <- subset(demo, demo$epr_number %in% data1$epr_number)
demo3 <- subset(demo2, select=c(epr_number,age_derived, gender, race, ethnicity))


#diabetestype <- subset(diabetes, select=c(epr_number, ds_diabetes_type_CHILDQ))
#table(as.factor(diabetestype$ds_diabetes_type_CHILDQ))
autoimm <- subset(data1, select= c(epr_number,he_c022_diabetes_PARQ, he_e036_ms, 
                                  he_b016_raynauds,he_c023a_hyperthyroidism_CHILDQ,
                    he_c023b_hypothyroidism_CHILDQ, he_f037_celiac,
                    he_f039_crohns, he_f040_ulcerative_colitis,
                   he_h053_scleroderma, 
                    he_h056_lupus, he_h057_sjogrens, 
                    he_i060_pernicious_anemia,he_j065_myositis,
                    he_j066_rheu_arthritis_PARQ,  he_j068_arthritis_other_PARQ,
                    he_k069_psoriasis, he_k070_eczema,X_he_gender_, he_a001a_height_in, he_a002_weight, he_bmi_derived,
                   he_bmi_cat_derived, he_r166a_diabetes_mom, he_r166b_diabetes_dad,
                   he_r172a_rheu_arthritis_mom,he_r172b_rheu_arthritis_dad, he_s180b_smoke_yrs_CHILDQ))
autoimm2 <- merge(autoimm, demo3, by.x="epr_number", by.y="epr_number", all.x=T)





epr_geocodes <- geocodes$epr_number
length(unique(epr_geocodes)) #9765

table(geocodes$geo_study_event)
currentadd <- subset(geocodes, geocodes$geo_study_event == "Exposome Part A - Current Address" )
currentadd <- subset(currentadd, select=c(epr_number, geo_study_event, geo_longitude, geo_latitude))

length(currentadd$epr_number) #3530



hesadd <- subset(geocodes, geocodes$geo_study_event == "Health and Exposure Survey")
length(hesadd$epr_number) #9426
hesadd <- subset(hesadd, select=c(epr_number, geo_study_event, geo_longitude, geo_latitude))




addresscount <-  merge(hesadd, currentadd, by.x=c("epr_number"), by.y=c("epr_number"), all.x=T)
addresscount$dual_addresses <- ifelse(!is.na(addresscount$geo_study_event.y), 1, 0)


final <- merge(addresscount, autoimm2, by.x="epr_number", by.y="epr_number", all.y=T)


names(final) <- c( "epr_number" ,  "geo_study_event.x", "geo_longitude.x", "geo_latitude.x",                 
                  "geo_study_event.y" ,  "geo_longitude.y" ,"geo_latitude.y" , "dual_addresses" ,                
                  "he_c022_diabetes_PARQ",  "he_e036_ms", "he_b016_raynauds","he_c023a_hyperthyroidism_CHILDQ",
                 "he_c023b_hypothyroidism_CHILDQ","he_f037_celiac" , "he_f039_crohns" , "he_f040_ulcerative_colitis" ,    
                   "he_h053_scleroderma" , "he_h056_lupus" ,"he_h057_sjogrens" , "he_i060_pernicious_anemia" ,     
                 "he_j065_myositis", "he_j066_rheu_arthritis_PARQ", "he_j068_arthritis_other_PARQ","he_k069_psoriasis" ,             
                  "he_k070_eczema", "age_derived" , "gender" , "race" , "ethnicity"  )




names(final[9:25])

for (i in 9:25){
  x <- final[i]
  x <- ifelse(x == "Yes", 1, 0)
  final[i] <- x

}

final$AI_count <- rowSums(final[9:25])
final$age_derived <- as.numeric(final$age_derived)


final$sclerosis_groupct <- rowSums(final[, c("he_h053_scleroderma", "he_h057_sjogrens", "he_b016_raynauds")])
final$sclerosis_group <- ifelse(final$sclerosis_groupct >= 1, 1,0)


final$atopicd_groupct <- rowSums(final[, c("he_k070_eczema", "he_k069_psoriasis" )])
final$atopicd_group <- ifelse(final$atopicd_groupct  >= 1, 1,0)

library(tableone)
## Vector of variables to summarize
myVars <- c("age_derived" , "gender"  ,  "race"  ,"ethnicity" , "AI_count","dual_addresses", "atopicd_groupct","sclerosis_groupct", "atopicd_group", "sclerosis_group" )
## Vector of categorical variables that need transformation
catVars <- c( "gender"  ,  "race"  ,"ethnicity", "dual_addresses", "atopicd_group", "sclerosis_group")

library(stringr)
## Create a TableOne object
tab2 <- CreateTableOne(vars = myVars, data = final, factorVars = catVars)
tab3 <- NULL

data <- subset(final, sclerosis_group == 1 | atopicd_group == 1)
mean(data$sclerosis_groupct)
mean(data$atopicd_groupct)
#for (i in 1:14)
#{
#  dat1 <- as.data.frame(tab2$CatTable$Overall[i])
# dat1$condition <- rep(str_split(names(tab2$CatTable$Overall[i]),"_")[[1]][3],nrow(dat1))
 # names(dat1) <- c("n","missing","p_missing","levels","frequency","percent","cum.perc","condition")
 # tab3 <- rbind(tab3,dat1)
#}

#tab3 <- as.data.frame(tab3)
#dat1 <- as.data.frame(tab2$CatTable$Overall[2])

#write.csv(tab3, "epr_autoimm2.csv")

