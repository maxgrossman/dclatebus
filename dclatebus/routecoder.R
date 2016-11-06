# Programmer: Maximus
# Purpose: Create SPDF and geojson of WMATA bus routes and each route's stops. This will be used for viz later
# Method: 
        # a: call api for routes 
        # b: use forloop to get json for for each route
        # c: take these jsons and create spdf for route lines and stops, both in include

### Install needed packages, load them ###
install.packages("httr")
install.packages("jsonlite")
require(httr)
require(jsonlite)
require(rgdal)
### a: call api for routes ###

# call api from terminal, save to wmata.routes with fromJSON
wmata.routes <- fromJSON(txt=system(paste(
  'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jRoutes" -H "api_key: ',
  readline(prompt="Provide API Key: "),
  ' "', sep=""),intern=TRUE))

### b: use forloop to get lat.lon, then make spatial lines dataframe ###

# list to hold returned jsons
paths.list <- c()

# for loop to extend routes.list

# api parameters
key<-readline(prompt="Provide API Key: ")
date<-readline(prompt="Provide date in format YYYY-MM-DD: ")

# list to extend with each wmata.path 
lst.wmata.path <- c()

# loop to extend the list
for(route in wmata.routes$Routes$RouteID)
{
  wmata.path <- fromJSON(txt=system(
    paste(
      'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jRouteDetails?RouteID=',
      route,'&Date=',date,'" -H "api_key: ',
      key,' "', sep=""),intern=TRUE))
  
  lst.wmata.path[[length(lst.wmata.path)+1L]] <- wmata.path
  
}
