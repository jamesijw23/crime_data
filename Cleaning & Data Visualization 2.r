
library(dplyr)
library(ggplot2)
library(tools)

gun = read.csv('gun-violence-data_01-2013_03-2018.csv')
us_info = read.csv('us_city_state_county.csv')
colnames(us_info)[1] = 'city'

## Important Functions
removal_outlier <- function(dataVar){
  summaryVar <- summary(dataVar)
  Q1 <- as.numeric(summaryVar)[2]
  Q3 <- as.numeric(summaryVar)[5]
  IQR <- Q3 - Q1
  
  LF <- Q1 - 1.5 * IQR
  UF <- Q3 + 1.5 * IQR
  outlier_decision <- ifelse(dataVar<= UF & dataVar>=LF,T,F)
  return(outlier_decision)
}


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}



find_city_count = function(city_county,state){
    ## Ways to write out county based on data
    possible_county = c('County', '(county)')
 
   
    ## Check to see if it is a conunty
    cond1 = grep(paste(possible_county,collapse="|"), city_county, value=TRUE)

    ## Only County
    if(length(cond1) > 0 ){
        county_name = sub('County|\\(county\\)','',cond1)
        tmp_cc = data.frame(city = ' ',county = county_name) 
     
    ## City but find county
    } else if (as.character(city_county) %in% us_info$city & as.character(state) %in% us_info$State.full){
        tmp = us_info[as.character(city_county) == us_info$city & 
                  as.character(state) == us_info$State.full,]
        county_info = simpleCap(tolower(as.character(tmp[1,4])))
        tmp_cc = data.frame(city = city_county,county = county_info)
    ## Not a county or a city    
    } else {
        tmp_cc = data.frame(city = city_county,county = ' ') 
    }
    return(tmp_cc)
}



dim(gun)
sum(complete.cases(gun))
str(gun)


gun = gun %>% select(-incident_url,-source_url,-sources,-notes)
head(gun)

## i.Verify Ids are all unique and every row has an id
nrow(gun) == length(unique(gun$incident_id))

## ii.Change date into a date variable 
gun$date <- as.Date(gun$date)

## iii. Determine the range of time: 2013-01-01 to 2018-03-31
max(gun$date)
min(gun$date)
sum(is.na(gun$date))

## iv. Make sure States are spelled right and each incident has a state
table(gun$state)
sum(is.na(gun$state))


rs = sample(100)
x = data.frame(city_county= gun$city_or_county[rs])
y = data.frame(state = gun$state[rs])

cc_df = apply(x,2,find_city_count,state=y)

c


## Create new columns counties and cities
gun$county =  cc_df$county
gun$city =  cc_df$city 

