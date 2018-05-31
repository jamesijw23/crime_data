

library(dplyr)
library(stringr)
##---------------------------------------------
## 1) Load-In Data
##---------------------------------------------
setwd("C:/Users/iwilli11/Desktop/Algorithms/Data/crime_csv")
gun_data = read.csv('gun-violence-data_01-2013_03-2018.csv')
us_info = read.csv('us_city_state_county.csv')


##---------------------------------------------
## 2) Analyze each column and Transform Based on need
##---------------------------------------------
##----------------------------------------------
## 2.1 Remove useless Columns: notes, sources and urls
## Status --> Good
##----------------------------------------------
gun_data_mod1 = gun_data %>% select(-incident_url,
                                    -source_url,
                                    -sources,
                                    -notes,
                                    -incident_url_fields_missing)


##----------------------------------------------
##  2.2. Verify Ids are all unique and every row has an id 
## Status --> TRUE
##----------------------------------------------
nrow(gun_data_mod1) == length(unique(gun_data_mod1$incident_id))


##----------------------------------------------
## 2.3. Change date into a date variable
## Status --> DONE
##----------------------------------------------
gun_data_mod1$date <- as.Date(gun_data_mod1$date)

##----------------------------------------------
## 2.4 Change "city_or_county" into two columns city and county 
## Status --> DONE
##----------------------------------------------
gun_data_mod1 = gun_data_mod1 %>%
  mutate(county = apply(as.matrix(gun_data_mod1$city_or_county),1,findCounty))
gun_data_mod1 = gun_data_mod1 %>%
  mutate(city = apply(as.matrix(gun_data_mod1$city_or_county),1,findCity))


##----------------------------------------------
## 2.5 Nothing to currently to do with address
## Status --> No Update
##----------------------------------------------

##----------------------------------------------
## 2.6 View "n_killed"
## Status --> No NA
## NOTE: Could be dependent variable
##----------------------------------------------
table(gun_data_mod1$n_killed)
nrow(gun_data_mod1) == sum(!is.na(gun_data_mod1$n_killed))



##----------------------------------------------
## 2.7 View "n_injured"
## Status --> No NA
## NOTE: Could be dependent variable
##----------------------------------------------
table(gun_data_mod1$n_injured)
nrow(gun_data_mod1) == sum(!is.na(gun_data_mod1$n_injured))




##----------------------------------------------
## 2.8 View "congressional_district"
## Status --> 11944 NAs
## NOTE: Just a geo-code
## Improvement: 
## a) Find out the congressional_district based on
## other rows that share same city and/or county 
## that are missing
##----------------------------------------------
table(gun_data_mod1$congressional_district)
sum(is.na(gun_data_mod1$congressional_district))




##-----------------
## 2.9a Change "gun_stolen" into number of guns
## Status --> DONE
##------------------
gun_stolen_mat = as.matrix(as.character(gun_data_mod1$gun_stolen))
gun_data_mod1$number_guns = apply(gun_stolen_mat , 1,findNumberGuns)




##-----------------
## 2.9b If a gun involved was stolen
## Status --> DONE
##-----------------


gun_data_mod1$stolen_gun_status = apply(as.matrix(gun_data_mod1$gun_stolen), 1,
      stolen_gun_status_function)


##-----------------
## 2.9c If a gun involved was not stolen
## Status --> DONE
##-----------------


gun_data_mod1$not_stolen_gun_status = apply(as.matrix(gun_data_mod1$gun_stolen), 1,
                                        not_stolen_gun_status_function)




##----------------------------------------------
## 2.10 "gun_type"                   
##----------------------------------------------
v = gun_data_mod1$gun_type
d = str_replace_all(unlist(lapply(v,remove_colons_numbers))," ", "" )
n = table(d)

##-----------------
## 2.10a If the gun that is involved extremely deadly
## More than 6 bullets in a round Feed system
## Status --> (Need to Do)
##-----------------


##----------------------------------------------
## 2.11 "incident_characteristics"   
##----------------------------------------------
v = gun_data_mod1$incident_characteristics



##-----------------
## 2.11a is a Mass Shooting or not
## Status --> Done
##-----------------
ms_check = c("Mass Shooting|mass shooting|Mass shooting|mass Shooting")
gun_data_mod1$is_mass_shooting = str_detect(v,ms_check)
gun_data_mod1$is_mass_shooting = ifelse(gun_data_mod1$is_mass_shooting ==T,1,0)



##-----------------
## 2.11b is a gang involved or not
## Status --> Done
##-----------------
gang_check = c("gang|Gang")
gun_data_mod1$is_gang_related = str_detect(v,gang_check)
gun_data_mod1$is_gang_related = ifelse(gun_data_mod1$is_gang_related ==T,1,0)


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

