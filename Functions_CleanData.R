##--------------------------------------
## Name: findCounty
## Input: city_county
## Output: County where incident took place
## Purpose: Provide the county of this column
## Improvements:
## a) Use Coordinates to determine the city
##-------------------------------------- 

findCounty= function(city_county){
  ## Ways to write out county based on data
  possible_county = c('County', '(county)')
  ## Check to see if it is a conunty
  cond1 = grep(paste(possible_county,collapse="|"), city_county, value=TRUE)
  if(length(cond1)>0){
  ## Remove County From name
  county_name = sub('County|\\(county\\)','',cond1)
  } else {
    county_name =''
  }
  ## Remove Space
  county_name = trimws(county_name, which="right")
  
 return(county_name)   
}


##--------------------------------------
## Name: findCity
## Input: city_county
## Output: city
## Purpose: Provide the city of this column
## Improvements:
## a) Check if city is in US based on another excel sheet
## b) Determine the county  based on city and State
## c) If a city is not in excel sheet use Coordinates to determine
## the city, county and state
##-------------------------------------- 
findCity= function(city_county){
  ## Ways to write out county based on data
  possible_county = c('County', '(county)')
  ## Check to see if it is a conunty
  cond1 = grep(paste(possible_county,collapse="|"), city_county, value=TRUE)
  if(length(cond1) == 0){
    ## Remove County From name
    city_name = city_county
  } else {
    city_name =''
  }
  ## Remove Space
  city_name = trimws(city_name, which="right")
  return(city_name)   
}




##--------------------------------------
## Name: findNumberGuns
## Input: If Gun was stolen
## Output: Number of Guns
## Purpose: Find number of Guns 
## Improvements:
## a) 
##--------------------------------------
findNumberGuns = function(x){
  guns_stole_type = unlist(strsplit(as.character(x),split="[||]"))
  number_of_guns = length(guns_stole_type[guns_stole_type != ""])
  return(number_of_guns)
}


##--------------------------------------
## Name: remove_colons_numbers 
## Input: string1 --> row in df (a string)
## Output:  a list of stings
## Purpose: Remove colons and numbers from strings
## Improvements:
## a) 
##--------------------------------------
remove_colons_numbers = function(string1){
  colons <- c("::|:::")
  make_vector = unlist(strsplit(as.character(string1),split="[||]"))
  remove_colons = str_replace(make_vector,colons ,"")
  remove_number = gsub('[0-9]+', '', remove_colons)
  return(remove_number)
}

##--------------------------------------
## Name: stolen_gun_status_function
## Input: string1 --> row in df (a string)
## Output:  T/F If a stolen gun is involved
## Purpose: State whether stolen gun is involved
## Improvements:
## a) 
##--------------------------------------
stolen_gun_status_function = function(string1){
  check_gun_status = c(":Stolen|Stolen")
  list_strings = remove_colons_numbers(string1) 
  result = sum(str_detect(list_strings,check_gun_status))
  result = ifelse(result>0,T,F)
  return(result)
}

##--------------------------------------
## Name: stolen_gun_status_function
## Input: string1 --> row in df (a string)
## Output:  T/F If Not stolen gun is involved
## Purpose: State whether not stolen gun is involved
## Improvements:
## a) 
##--------------------------------------
not_stolen_gun_status_function = function(string1){
  check_gun_status = c(":Not Stolen|Not Stolen")
  list_strings = remove_colons_numbers(string1) 
  result = sum(str_detect(list_strings,check_gun_status))
  result = ifelse(result>0,T,F)
  return(result)
}



##--------------------------------------
## Name: children_involved
## Input: strings --> row in df (a string)
## Output:  T/F If child less than 18 is involved
## Purpose: State whether children is involved
## Improvements:
## a) 
##--------------------------------------
children_involved = function(strings){
  tmp_list_str = unlist(strsplit(as.character(strings),split="[||]"))
  age_strs = str_replace(tmp_list_str,"\\d::","")
  ages = as.numeric(age_strs[age_strs!=""])
  child_less18 = ifelse(sum(ages<18)>0,T,F)
  return(child_less18)
}
