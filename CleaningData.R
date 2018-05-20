

library(dplyr)
##---------------------------------------------
## 1) Load-In Data
##---------------------------------------------
setwd("C:/Users/iwilli11/Desktop/CrimeData/Data")
gun_data = read.csv('gun-violence-data_01-2013_03-2018.csv')
us_info = read.csv('us_city_state_county.csv')


##---------------------------------------------
## 2) Analyze each column and Transform Based on need
##---------------------------------------------

## Remove useless Columns: notes, sources and urls
gun_data_mod1 = gun_data %>% select(-incident_url,
                                    -source_url,
                                    -sources,
                                    -notes,
                                    -incident_url_fields_missing)


##----------------------------------------------
##  a. Verify Ids are all unique and every row has an id 
## Status --> TRUE
##----------------------------------------------
nrow(gun_data_mod1) == length(unique(gun_data_mod1$incident_id))


##----------------------------------------------
## b. Change date into a date variable
## Status --> DONE
##----------------------------------------------
gun_data_mod1$date <- as.Date(gun_data_mod1$date)

##----------------------------------------------
##  c. Change "city_or_county" into two columns city and county 
## Status --> DONE
##----------------------------------------------
gun_data_mod1 = gun_data_mod1 %>%
  mutate(county = apply(as.matrix(gun_data_mod1$city_or_county),1,findCounty))
gun_data_mod1 = gun_data_mod1 %>%
  mutate(city = apply(as.matrix(gun_data_mod1$city_or_county),1,findCity))


##----------------------------------------------
## d. Nothing to currently to do with address
## Status --> No Update
##----------------------------------------------

##----------------------------------------------
## e. View "n_killed"
## Status --> No NA
## NOTE: Could be dependent variable
##----------------------------------------------
table(gun_data_mod1$n_killed)
nrow(gun_data_mod1) == sum(!is.na(gun_data_mod1$n_killed))



##----------------------------------------------
## f. View "n_injured"
## Status --> No NA
## NOTE: Could be dependent variable
##----------------------------------------------
table(gun_data_mod1$n_injured)
nrow(gun_data_mod1) == sum(!is.na(gun_data_mod1$n_injured))




##----------------------------------------------
## g. View "congressional_district"
## Status --> 11944 NAs
## NOTE: Just a geo-code
## Improvement: 
## a) Find out the congressional_district based on
## other rows that share same city and/or county
##----------------------------------------------
table(gun_data_mod1$congressional_district)
nrow(gun_data_mod1) - sum(!is.na(gun_data_mod1$congressional_district))




##**
## h. gun_stolen
##**
table(gun_data_mod1$gun_stolen)



##----------------------------------------------
## [10] "gun_stolen"    
##----------------------------------------------

##----------------------------------------------
## [11] "gun_type"                   
##----------------------------------------------

##----------------------------------------------
## [12] "incident_characteristics"   
##----------------------------------------------

##----------------------------------------------
## [13] "latitude"                   
##----------------------------------------------

##----------------------------------------------
## [14] "location_description"       
##----------------------------------------------

##----------------------------------------------
## [15] "longitude"                  
##----------------------------------------------

##----------------------------------------------
## [16] "n_guns_involved"            
##----------------------------------------------

##----------------------------------------------
## [17] "participant_age"            
##----------------------------------------------

##----------------------------------------------
## [18] "participant_age_group"      
##----------------------------------------------

##----------------------------------------------
## [19] "participant_gender"         
##----------------------------------------------

##----------------------------------------------
## [20] "participant_name"           
##----------------------------------------------

##----------------------------------------------
## [21] "participant_relationship"   
##----------------------------------------------

##----------------------------------------------
## [22] "participant_status"         
##----------------------------------------------

##----------------------------------------------
## [23] "participant_type"           
##----------------------------------------------

##----------------------------------------------
## [24] "state_house_district"       
##----------------------------------------------

##----------------------------------------------
## [25] "state_senate_district"
##----------------------------------------------

