# Programmer: Maximus
# Purpose: Create SPDF and geojson of WMATA bus routes. This will be used for viz later
# Method: a: call api for routes 
        # b: use forloop to get lat.lon of route paths for each RouteID in list

### Install needed packages, load them ###
install.packages("httr")
install.packages("jsonlite")
require(httr)
require(jsonlite)

### a: call api for routes ###

# call api from terminal, save to wmata.routes with fromJSON
wmata.routes <- fromJSON(txt=system(paste(
  'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jRoutes" -H "api_key: ',
  readline(prompt="Provide API Key: "),
  ' "', sep=""),intern=TRUE))
