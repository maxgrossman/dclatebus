##########################################################################################
# Programmer: Maximus
# Purpose: Create SPDF and geojson of WMATA bus routes and each route's stops. 
         # This will be used for viz later
# Method: 
        # a: create PostGIS db and schema to write route stop geom to.
        # b: call api for route names and stops.
        # c: use forloop to create bus route paths and stops from stop json:
            # here, for each route in the returned JSON I find stops that serve that route
            # and write the route_id, stop_name, and lat lon to a dataframe
            # this is then written as a table in the db schema created in step a

# Attribution: 
        # huge kudos to the following smart people who helped me figure out this:
            # PostGIS meets R:  
            

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

for(package in packages) {3
  
  library( package, character.only = TRUE)

} 

##########################################################################################

### a: create postgis db and schema ###

# get database name

database_name <- readline( prompt = "Provide name of database to create: ")

# get schema name

schema_name <- readline( prompt = "Provide name of schema to create: ")

# create database

system( paste( "createdb", database_name, sep = " " ))

# connect to database by setting driver the PostgreSQL, then connect

drv <- dbDriver("PostgreSQL")

connection <- dbConnect( drv = drv,
                         dbname = database_name,
                         host = "localhost",
                         port = 5432 )

# make it a PostGIS db

postgis_query <- "CREATE EXTENSION postgis;"

dbSendQuery( connection, postgis_query)

# create schema

schema_query <- paste( "CREATE SCHEMA ", schema_name, ";", sep = "" )

dbSendQuery( connection, schema_query )

##########################################################################################

### b: call api for routes and stops ###

key <- readline( prompt = "Provide API Key: ")

# call routes api from cmd, save to wmata.routes with fromJSON. makes list with many nested lists

wmata_routes <- fromJSON( txt = system( paste(
  'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jRoutes" -H "api_key: ',
  key,' "', sep = "" ) , intern=TRUE ))

# call bus stops api

wmata_stops <- fromJSON( txt = system( paste(
  'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jStops" -H "api_key: ',
  key,' "', sep = "" ) , intern=TRUE ))

##########################################################################################


# Loop over RouteIDs in wmata.routes to see which bus stops they pass through, getting
# index of those bus stops and then using that index to create spatial points data frame
# with 1) the location of the stop, 2) the id of the stop. 3) the stop name 
# write these as tables in the stops SCHEMA in wmata_stops 

for( route_id in wmata_routes$Routes$RouteID ) {
  
    # get table name being created. b/c I use numeric, needs to in dbl-quotes
  
    route_tab <- paste( schema_name, 
                        shQuote(route_id, type = "cmd"), 
                        sep = ".")
    

    # index the wmata_stops to get only those route_id passes through
  
    route_index <- grep( route_id , wmata_stops$Stops$Routes )
    
    # grab stop id
    
    stops_id <- wmata_stops$Stops$StopID[ route_index ]
    
    # grab stop names 
    
    stops_nam <- wmata_stops$Stops$Name[ route_index ]
    
    # make df with lat & lng from json
    
    stops_latlng <- cbind( x = wmata_stops$Stops$Lon[ route_index ],
                           y = wmata_stops$Stops$Lat[ route_index ] )
    
    # make stops df, we'll write this next to the postgis db
    
    stops_df <- data.frame( stops_id = stops_id,
                            stops_nam = stops_nam,
                            stops_latlng )
    
    # write the route to the route schema
    
    dbWriteTable( connection, 
                  c( schema_name , route_id ),
                  stops_df )
    
    # make stop_geom column in table to hold point locatoins
    
    geom_query <- paste( "ALTER TABLE",
                         route_tab, 
                         "ADD COLUMN stop_geom geometry(POINT,4326);",
                         sep = " " )

    dbSendQuery( connection , geom_query )
    
    # take the x, y columns and use them to populate the stop_geom column
    
    setgeom_query <- paste( "UPDATE",
                            route_tab,
                            "SET stop_geom=ST_SetSRID(ST_MakePoint(x, y), 4326);",
                            sep = " " )

    dbSendQuery(connection, setgeom_query)
    
    # print out successes
    
    cat("Made table for route: ", route_id, "\n\n")
  }