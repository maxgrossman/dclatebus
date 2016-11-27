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
  
} else { install.packages( packages_index ) }

for(package in packages) {
  
  library( package, character.only = TRUE)

} 

##########################################################################################

### a: create postgis db and schemas ###

# get database name

database_name <- readline( prompt = "Provide name of database to create: ")

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

# create schema for dir0 and dir1

dbSendQuery( connection, "CREATE SCHEMA dir0routes;")

dbSendQuery( connection, "CREATE SCHEMA dir1routes;")

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


# use list of route names to get list of route details for each route

route_names <- wmata_routes$Routes$RouteID

# initialize empty list to be filled below

wmata_routes_lst <- list()

# loop through all routes, api call for each, extend wmata_routes_lst with output

for( route in route_names ) {
  
  wmata_route <- fromJSON( txt = system( paste( 
    'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jRouteDetails?RouteID=', 
    route, '"',' -H "api_key: ', key,' "', sep = "" ) , intern=TRUE ))
  
  wmata_routes_lst[[ length( wmata_routes_lst ) + 1L ]] <- wmata_route
  
}

# make the direction0 and direction1 lists, getting rid of nulls in both. 

# lapply function to make the dir0 list

dir0_wmata_routes <- wmata_routes_lst[ !sapply(
  
  seq_along( wmata_routes_lst ),
  
  function(i) 
    {
    
    # an if else statement to return index of
    # items in for those routes with stop geometry objects that aren't null
      
    if(is.null(wmata_routes_lst[[i]]$Direction0$Stops)) {
        
    } else if(length(wmata_routes_lst[[i]]$Direction0$Stops)>1) {i}
      
    }) %in% "NULL" ] 
  
# same as above for dir1

dir1_wmata_routes <- wmata_routes_lst[ !sapply(
  
  seq_along( wmata_routes_lst ),
  
  function(i) 
  {
    
    # an if else statement to return index of
    # items in for those routes with stop geometry objects that aren't null
    
    if(is.null(wmata_routes_lst[[i]]$Direction1$Stops)) {
      
    } else if(length(wmata_routes_lst[[i]]$Direction1$Stops)>1) {i}
    
  }) %in% "NULL" ]

# make list of these two direction lists, we'll use this for a loop later on. 

dir_wmata_routes_lst <- list( dir0_wmata_routes , dir1_wmata_routes)

names(dir_wmata_routes_lst) <- c( deparse( substitute( dir0_wmata_routes )),
                                  deparse( substitute( dir1_wmata_routes)))

##########################################################################################

# Loop over routes in dir0 and dir1 list and do the followings
    # 1) create a dir0 and dir1 table in wmata_routes db
    # 2) nest loop in that loop that for both dir0 and dir1:
      # a) takes first two stops in each to make a data frame including: 
        # stop_id 
        # stop_names 
        # lat,lon
      # b) write this out to created dir0 or dir1 tab in wmata_routes db

for ( i in 1:length(dir_wmata_routes_lst) ) {
  
    # get directory schema and route table names 
    
    dir_schema <- paste( substring( names(dir_wmata_routes_lst)[i] ,1,4),
                           "routes", sep = "" )
      
    for ( a in 1 : length( dir_wmata_routes_lst[[i]] )) {
        
        route_id <- dir_wmata_routes_lst[[i]][[a]]$RouteID
        
        route_tab <- paste( shQuote( dir_schema , type = 'cmd'), 
                            shQuote( route_id, type = 'cmd'), sep = ".")
        
        if ( dir_schema == "dir0routes" ) {
          
          for ( b in 1: length( dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$StopID )) {
            
            # get stops at route segment of interest
            
            seg_stops_id <- cbind( dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$StopID[b],
                                   dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$StopID[b+1])
          
            # get stop names at route segment of interest
          
            seg_stops_nams <- cbind( gsub(" ", "_" , 
                                          dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Name[b]),
                                     gsub(" ", "_" , 
                                          dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Name[b+1]))
          
            # get stop latlngs at route segment of interest
          
            seg_stops_latlngs <- cbind( a_x = dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Lon[b],
                                        a_y = dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Lat[b],
                                        b_x = dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Lon[b+1],
                                        b_y = dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Lat[b+1])
          
            # write all segment attributes to dataframe
          
            seg_df <- data.frame( seg_stops_id,
                                  seg_stops_nams,
                                  seg_stops_latlngs )
          
            # coerce clean column names. 
          
            names(seg_df) <- c("stop_a_id", "stop_b_id",
                               "stop_a_nam", "stop_b_nam",
                               "a_x", "a_y",
                               "b_x", "b_y")
          
          # conditional statement to write table for first statement, then update for all other
          
          if ( b = 1 ) {
            
            # intialize table, writing first row
            
            dbWriteTable( connection,  c( dir_schema , route_id ), seg_df)
            
          } else if ( b < length( dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$StopID ) - 1 ) {
            
            # update the table with new row
            
            update_query <-  paste("INSERT INTO ", route_tab, " ",
                                   "(stop_a_id, stop_b_id, stop_a_nam,",
                                   "stop_b_nam, a_x, a_y, b_x, b_y) "," VALUES (",
                                   seg_df$stop_a_id,", ", seg_df$stop_b_id, ", ",
                                   shQuote(seg_df$stop_a_nam), ", " ,
                                   shQuote(seg_df$stop_b_nam), ", ",
                                   seg_df$a_x, ", ", seg_df$a_y, ", ",
                                   seg_df$b_x, ", ", seg_df$b_y, ");", sep = "")
            
            dbSendStatement( connection, update_query )
            
            
          } else if ( b + 1 = length( dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$StopID )) {
            
              # update the table with the final row 
            
              update_query <-  paste( "INSERT INTO ", route_tab, " ",
                                      "(stop_a_id, stop_b_id, stop_a_nam,",
                                      "stop_b_nam, a_x, a_y, b_x, b_y) "," VALUES (",
                                      seg_df$stop_a_id,", ", seg_df$stop_b_id, ", ",
                                      shQuote(seg_df$stop_a_nam), ", " ,
                                      shQuote(seg_df$stop_b_nam), ", ",
                                      seg_df$a_x, ", ", seg_df$a_y, ", ",
                                      seg_df$b_x, ", ", seg_df$b_y, ");", sep = "")
              
              # create line column
              
              line_query <- paste( "ALTER TABLE", route_tab, 
                                   "ADD seg_line Geometry(LINESTRING);",
                                   sep = " ")
              
              dbSendStatement( connection, line_query )
              
              # populate seg_line with spatial information
              
              spatialize_query <- paste( "UPDATE", route_tab,
                                         "SET seg_line =",
                                         "ST_MakeLine(ST_MakePoint(a_x,a_y),",
                                         "ST_MakePoint(b_x,b_y));",
                                          sep = " ")
              
              dbSendStatement( connection, spatialize_query)
          }
        }
          
        } else if ( dir_schema == "dir1routes") {
          
          for ( c in 1: length( dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$StopID )) {
            
            # get stops at route segment of interest
            
            seg_stops_id <- cbind( dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$StopID[c],
                                   dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$StopID[c+1])
            
            # get stop names at route segment of interest
            
            seg_stops_nams <- cbind( gsub(" ", "_" , 
                                          dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Name[c]),
                                     gsub(" ", "_" , 
                                          dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Name[c+1]))
            
            # get stop latlngs at route segment of interest
            
            seg_stops_latlngs <- cbind( a_x = dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Lon[c],
                                        a_y = dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Lat[c],
                                        b_x = dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Lon[c+1],
                                        b_y = dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Lat[c+1])
            
            # write all segment attributes to dataframe
            
            seg_df <- data.frame( seg_stops_id,
                                  seg_stops_nams,
                                  seg_stops_latlngs )
            
            # coerce clean column names. 
            
            names(seg_df) <- c("stop_a_id", "stop_b_id",
                               "stop_a_nam", "stop_b_nam",
                               "a_x", "a_y",
                               "b_x", "b_y")
            
            # conditional statement to write table for first statement, then update for all other
            
            if ( c = 1 ) {
              
              # intialize table, writing first row
              
              dbWriteTable( connection,  c( dir_schema , route_id ), seg_df)
              
              
            } else if ( c < length( dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$StopID ) - 1 ) {
              
              # update the table with new row
              
              update_query <-  paste("INSERT INTO ", route_tab, " ",
                                   "(stop_a_id, stop_b_id, stop_a_nam,",
                                   "stop_b_nam, a_x, a_y, b_x, b_y) "," VALUES (",
                                   seg_df$stop_a_id,", ", seg_df$stop_b_id, ", ",
                                   shQuote(seg_df$stop_a_nam), ", " ,
                                   shQuote(seg_df$stop_b_nam), ", ",
                                   seg_df$a_x, ", ", seg_df$a_y, ", ",
                                   seg_df$b_x, ", ", seg_df$b_y, ");", sep = "")
              
              dbSendStatement( connection, update_query )
              
              
            } else if ( c + 1 = length( dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$StopID )) {
              
              # update the table with the final row 
              
              update_query <-  paste("INSERT INTO ", route_tab, " ",
                                   "(stop_a_id, stop_b_id, stop_a_nam,",
                                   "stop_b_nam, a_x, a_y, b_x, b_y) "," VALUES (",
                                   seg_df$stop_a_id,", ", seg_df$stop_b_id, ", ",
                                   shQuote(seg_df$stop_a_nam), ", " ,
                                   shQuote(seg_df$stop_b_nam), ", ",
                                   seg_df$a_x, ", ", seg_df$a_y, ", ",
                                   seg_df$b_x, ", ", seg_df$b_y, ");", sep = "")
              
              dbSendStatement( connection, update_query )
              
              # create line column
              
              line_query <- paste("ALTER TABLE", route_tab, 
                                  "ADD seg_line Geometry(LINESTRING);",
                                  sep = " ")
              
              dbSendStatement( connection, line_query )
              
              # populate seg_line with spatial information
              
              spatialize_query <- paste("UPDATE", route_tab,
                                        "SET seg_line =",
                                        "ST_MakeLine(ST_MakePoint(a_x,a_y),",
                                        "ST_MakePoint(b_x,b_y));",
                                        sep = " ")
              
              dbSendStatement( connection, spatialize_query)
              
              
              # delete all spatial fields except for the final line field
            }
          }
          
        }
  
        # reset b and c to 1 for next loops
        
        b=1; c=1; a=1
      }
}

