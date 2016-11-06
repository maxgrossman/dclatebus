##########################################################################################
# Programmer: Maximus
# Purpose: Create SPDF and geojson of WMATA bus routes and each route's stops. 
         # This will be used for viz later
# Method: 
        # a: call api for routes 
        # b: use forloop to get json for for each route
        # c: take these jsons and create spdf for route lines and stops, both in include
##########################################################################################

### Install needed packages, load them ###
install.packages("httr")
install.packages("jsonlite")
require(httr)
require(jsonlite)
require(rgdal)

### a: call api for routes ###

# api call parameters
key<-readline(prompt="Provide API Key: ")

# call api from terminal, save to wmata.routes with fromJSON
wmata.routes <- fromJSON(txt=system(paste(
  'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jRoutes" -H "api_key: ',
  key,' "', sep=""),intern=TRUE))

### b: use forloop to get lat.lon, make spatial lines dataframe ###

# api parameters
date<-readline(prompt="Provide date in format YYYY-MM-DD: ")

# list to extend with each wmata.path 
lst.wmata.path <- c()

# loop to extend the list
for(route in wmata.routes$Routes$RouteID)
{
  
  # api call with route
  wmata.path <- fromJSON(txt=system(
    paste(
      'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jRouteDetails?RouteID=',
      route,'&Date=',date,'" -H "api_key: ',
      key,' "', sep=""),intern=TRUE))
  
  # extend list with returned json
  lst.wmata.path[[length(lst.wmata.path)+1L]] <- wmata.path
  
}
