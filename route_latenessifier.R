##########################################################################################
# Programmer: Maximus
# Code: route_latenessifier.R
# Purpose: create dataframe with bus lateness at given api cal tim
# Method: 
# a: call bus position api
# b: sort the json response into df with timestamp cal and unique rows for each route

##########################################################################################

### Conditionally install/load needed packages ###

# list of packages used in script
packages <- c( "jsonlite", "rgdal", "rgeos", 
               "raster", "maptools", "RPostgreSQL" )

# coerce response from installed.packages() to be clean data.frame of packages installed
packages_installed <- as.data.frame( installed.packages()[,c(1,3:4)] )

# remove rownames
rownames( packages_installed ) <- NULL

# use null value found in $Priority, this signifies already being downloaded
packages_installed <- packages_installed[is.na(
  packages_installed$Priority),
  1:2,drop=FALSE]

# get true/false index of packages on computer. true here is NOT INSTALLED
packages_index <- packages[!packages %in% as.character(packages_installed$Package)]

# if packages_index length greater than 1, install the packages in _packages_index

if (identical( packages_index, character(0) )) { 
  
} else { install.packages(packages_index) }

for(package in packages) {
  
  library( package, character.only = TRUE)
  
} 

##########################################################################################

key <- readline( prompt = "Provide API Key: ")

bus_positions <- fromJSON( txt = system( paste(
  'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jBusPositions" -H "api_key: ',
  key,' "', sep = "" ) , intern=TRUE ))

##########################################################################################


