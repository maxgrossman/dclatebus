##########################################################################################
# Programmer: Maximus
# Name: route_seg_spatializerr.R
# Purpose: Create SPDF and geojson of WMATA bus routes and each route's segments between stops 
         # This will be used for viz later
# Method: 
        # a: create PostGIS db and schema to write route stop geom to.
        # b: call api for route names and stops.
            # 1) GET route names (jRoutes) and save to list of all route names
            # 2) For each route in part 1 list, GET route details (jRouteDetails) and save a new list for route details
            # 3) Split the list of route details by direction of busses and get rid of all junk routes as some routes returned include no spatial info.
        # c: use forloop over the bus direction list to create tables with rows representing bus segments between consecutive stops. Write this to PostGIS db made in part 1:
            # 1) use name of item i to generate db_schema for direction
            # 2) for loop over all bus_routes within item i 
              # 2_a) for route a, increase number of points underlying geometry of route. this is done to make loc of start and end points of segments closer to bus stop loc
              # 2_b) initialize dataframe, all_seg_df that will hold the following for each bus in route a
                # segment_ids, i.e "10A_1"
                # stop_id_a
                # stop_id_b
                # stop_nam_a
                # stop_nam_b
                # stop_geom, a MUTLIPOINT postgis geom feature with loc of stop_a & stop_b
                # seg_geom, a LINESTRING postgis geom feature representing segment of bus route between stop_a and stop_b
              # 2_c) for each bus stop b (if dirschema=direction0, c if it's direction1) from b = 1 to b = second to last in route a do the following:
                # 2_c_1) query json for route a to get all but  seg_geom for row in all_seg_df. that are created in 2_c_2 ~ 2_c_6.
                # 2_c_2) buffer bus stop b and bus stop b+1 
                # 2_c_3) select all points in route a with buffer. iterate this process until stop b is selecting points or b+1 is selecting or points selected > 1
                # 2_c_4) attribute the "end points" of teh segment as those selected points with shortest distance to stop b and b+1, resectively
                # 2_c_5) if "end points" are the same, meaning midpoint is selected by both, make selected end point for closest stop and original bus stop endpoint for farthest
                # 2_c_6) if 2_c_5 is false, index route a from stop b endpoint to stop b+1 endpoint and use ths index to make subset of route a. this is this seg's 'seg_geom'
                # 2_c_7) convert this geom into LINESTRING() format to be written to PostGIS db
              # 2_d) write all but stop_geom and seg_geom to a table made for route a within dirschema for direction i
              # 2_f) create columns in that table for stop_geom and seg_geom, specify them as MULTIPOINT and LINESTRING, respectively
              # 2_g) for columns made in 2_f, for loop over corresponding column in all_seg_df, writing each row its respective column

##########################################################################################

### Conditionally install/load needed packages ###

# list of packages used in script
packages <- c( "jsonlite", "RPostgreSQL", "rgdal", "rgeos", "geojsonio", "raster","stringr","stringi" )

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

### CRS ###

wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

msp <- "+proj=lcc +lat_1=38.3 +lat_2=39.45 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

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
                         ##passward = 
                         port = 5432 )

# make it a PostGIS db

postgis_query <- "CREATE EXTENSION postgis;"

dbSendQuery( connection, postgis_query )

# create schema for dir0 and dir1

dbSendQuery( connection, "CREATE SCHEMA dir0routes;")

dbSendQuery( connection, "CREATE SCHEMA dir1routes;")

##########################################################################################

### b: call api for routes and stops ###

#key <- readline( prompt = "Provide API Key: ")

key <- "dd7ff89ac1744089adfa326205e723b4"

# call routes api from cmd, save to wmata.routes with fromJSON. makes list with many nested lists

wmata_routes <- fromJSON( txt = system( paste(
  'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jRoutes" -H "api_key: ',
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

# lapply function to make the dir0 list; which goes north and east
# then change the name of the route to have either w or s for directions

dir0_wmata_routes <- wmata_routes_lst[ !sapply(
  
  seq_along( wmata_routes_lst ),
  
  function(i) 
    {
    
    # an if else statement to return index of
    # items in for those routes with stop geometry objects that aren't null
      
    if(is.null(wmata_routes_lst[[i]]$Direction0$Stops)) {
        
    } else if(length(wmata_routes_lst[[i]]$Direction0$Stops)>1) {i}
      
    }) %in% "NULL" ] 
  
# same as above for dir1; which is south and west

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

names(dir_wmata_routes_lst) <- c( "dir0routes",
                                  "dir1routes")

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
    
    dir_schema <- paste( substring( names(dir_wmata_routes_lst)[i] ,1,4),"routes", sep = "" )
      
    for ( a in 1:length( dir_wmata_routes_lst[[i]] ) ) {
      
        route_id <- dir_wmata_routes_lst[[i]][[a]]$RouteID
      
        route_tab <- paste( shQuote( dir_schema , type = 'cmd'), 
                            shQuote( route_id, type = 'cmd'), sep = ".")
        
        if ( dir_schema == "dir0routes" ) {
          
          route_points_df <- cbind(lon <- dir_wmata_routes_lst[[i]][[a]]$Direction0$Shape$Lon,
                                   lat <- dir_wmata_routes_lst[[i]][[a]]$Direction0$Shape$Lat,
                                   seqnum <- dir_wmata_routes_lst[[i]][[a]]$Direction0$Shape$SeqNum)
          
          route_points <- SpatialPointsDataFrame(
                            SpatialPoints(route_points_df[,1:2], CRS(wgs84)),
                                          data = data.frame(route_points_df[,3])
          )
          
          # do the following method twice to increase numbe of points in bus_route.points:
          
            # 1) find midpoint between zth and z+1th point.
            # 2) find midpoint between zth and midpoint
            # 3) find midpoint between midpoint and z+1th point
          
          # the reason to do this is to make points much closer to our bus stops
          
          for(z in 1:(length(route_points@coords[,1])-1)) {
            
            e <- route_points@coords[z,]
            
            f <- route_points@coords[z+1,]
            
            mid <- cbind((e[1]+f[1])/2,(e[2]+f[2])/2)
            
            amid <- cbind((e[1]+mid[1])/2,(e[2]+mid[2])/2)
            
            midb <- cbind((mid[1]+f[1])/2,(mid[2]+f[2])/2)
            
            better_seg_points <- rbind(e, amid, mid, midb, f)
            
            row.names(better_seg_points) <- stri_rand_strings(
              length(better_seg_points[,1]), 
              length=5, pattern = "[A-Za-z0-9]")
            
            # for pair of coordinates, initailize better_seg_points_fin and get rid of garbage.
            
            if (z == 1) {
              
              better_seg_points_fin = 0
              
              better_seg_points_fin = data.frame(rbind(better_seg_points_fin,better_seg_points))
              
              better_seg_points_fin = better_seg_points_fin[better_seg_points_fin[,1] > 0,]
              
            } else {
              
              # for all scenarios except the last, rbind, or extend rows, of better_seg_points_fin with better_seg_points
              
              if ( z < (length(route_points@coords[,1])-1)) {
                
                better_seg_points_fin = rbind(better_seg_points_fin,better_seg_points)
                
              } else {
                
                # in last scenario, make better_seg_points_fin SpatialPointsDataFrame
                
                route_points <- spTransform(SpatialPoints(rbind(better_seg_points_fin,better_seg_points),
                                              CRS(wgs84)),
                                            CRS(msp))
                
                route_points.ids <- seq(length(route_points@coords[,1]))
                
                route_points <- SpatialPointsDataFrame(route_points, data= data.frame(id=route_points.ids))
                
                  }
              
              }
          }
          
          all_seg_df = data.frame(seg_id=1,stop_a_id=2,stop_b_id=3,stop_a_nam=4,stop_b_nam=5,stops_geom=6,seg_geom=7)
          
          for ( b in 1: length( dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$StopID ) ) {
            
            if (b == length( dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$StopID )) {
              
            } else {
            
            # create segment_id
            
            segment_id <- paste( dir_wmata_routes_lst[[i]][[a]]$RouteID, sprintf("%02d",b), sep = "_")
            
            # get stops at route segment of interest
            
            seg_stops_id <- cbind( dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$StopID[b],
                                   dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$StopID[b+1])
          
            # get stop names at route segment of interest
          
            seg_stops_nams <- cbind( gsub("'","_",gsub(" ", "_" ,
                                       dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Name[b])),
                                     gsub("'","_",gsub(" ", "_" ,
                                       dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Name[b+1]))
            )
            
            # get stop latlngs at route segment of interest and format for postgis
          
            seg_stops <- paste("MULTIPOINT(",
                           dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Lon[b], " ",
                           dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Lat[b], ",",
                           dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Lon[b+1], " ",
                           dir_wmata_routes_lst[[i]][[a]]$Direction0$Stops$Lat[b+1],")",sep="")
            
            # create spdf of segment bus stops, buffer them, and select bus_route.points within buffer
            
            seg_stops_points <- readWKT(seg_stops,p4s=CRS(wgs84))
            
            row.names(seg_stops_points) <- seq(length(seg_stops_points@coords[,1]))
            
            seg_stops_points <- spTransform(seg_stops_points,CRS(msp))
            
            seg_stops_points <- SpatialPointsDataFrame(
                                  seg_stops_points,
                                  data=data.frame(id=seq(length(seg_stops_points@coords[,1])))
            )
            
            buff_dist = 50
            
            buff_seg_stops_points <- gBuffer(seg_stops_points, buff_dist, id=seg_stops_points$id, byid=TRUE)
            
            points_within_buff <- route_points[buff_seg_stops_points, ]
            
            # while loop to increase buffer distance until more than 1 point selected 
            
            # continue increasing until:
            # 1) more than one point is selected by buffer OR
            # 2) the buffer around  the  first spatial point selects something OR 
            # 3) the buffer around the second spatial point selects something
            
            
            # increase the buffer distance by 10 feet and reselect points. 
            
            while ( length(points_within_buff) <= 1 |
                    length(route_points[buff_seg_stops_points[2,], ]@coords) < 2 | 
                    length(route_points[buff_seg_stops_points[1,], ]@coords) < 2  )
            {
              
              buff_dist = buff_dist + 10
              
              buff_seg_stops_points <- gBuffer(seg_stops_points, buff_dist, id=seg_stops_points$id, byid=TRUE)
              
              points_within_buff <- route_points[buff_seg_stops_points, ]
              
            }
            
            bus_route_startp <- which.min(gDistance(seg_stops_points[1,],points_within_buff,byid=T))
            
            bus_route_endp <- which.min(gDistance(seg_stops_points[2,],points_within_buff,byid=T))
            
            # check to see if the selected start and end points are the same. if they are, then use
            # the end point with shorter distance and opposite line_spatialpnts endpoint to make the this_bus_route variable
            
            if( bus_route_startp == bus_route_endp ) {
              
              if( gDistance(seg_stops_points[1,],points_within_buff) < gDistance(seg_stops_points[2,],points_within_buff) ) {
                
                this_bus_route <- SpatialPoints(rbind(seg_stops_points[2,]@coords,
                                                points_within_buff[bus_route_startp,]@coords),CRS(msp))
                
              } else {
                
                this_bus_route <- SpatialPoints(rbind(seg_stops_points[1,]@coords,
                                                      points_within_buff[bus_route_endp,]@coords),CRS(msp))

              }
              
            } else {
              
              # update points_within_buff
              
              points_within_buff <- points_within_buff[bus_route_startp:bus_route_endp,]
              
              ### create sequence of all points from the 1st to last point in points.within.buffer
              
              points_within_buff_index <- seq(from=as.numeric(as.character(points_within_buff@data$id))[1],
                                              to=tail(as.numeric(as.character(points_within_buff@data$id)),n=1))
              
              ### create spatialpoints dataframe of all points in this sequence, project it back to wgs84 
              
              this_bus_route <- spTransform(route_points[as.numeric(as.character(route_points@data$id)) %in% points_within_buff_index,],
                                            CRS(msp))
            }
            
            ### create wkt line from this_bus_route
            
            wkt_xy_list <- list()
            
            for( m in 1:length(this_bus_route@coords[,1]) ) {
              
              wkt_xy <- paste(as.character(this_bus_route@coords[m,1]),
                              as.character(this_bus_route@coords[m,2]), sep=" ")
              
              wkt_xy_list[[ length( wkt_xy_list ) + 1L ]] <- wkt_xy
              
            }
            
            # take the dataframe we made and convert to linestring
            
            seg_linestring <- paste("LINESTRING(",
                                    str_c(unlist(wkt_xy_list),collapse=', '),
                                    ")",sep="")
  
            # write all segment attributes to dataframe
            
            seg_df <- data.frame( segment_id,
                                  seg_stops_id,
                                  seg_stops_nams,
                                  seg_stops,
                                  seg_linestring )
            
            # coerce clean column names. 
            
            names(seg_df) <- names(all_seg_df)
            
            all_seg_df <- rbind(all_seg_df,seg_df)
            
            if ( b == 1 ) {
              
              all_seg_df = all_seg_df[2,]  
              
            }
            
            } 
            
          }

          # intialize table, writing first row
            
          dbWriteTable( connection,  c( dir_schema , route_id ), all_seg_df[,1:5])
            
          stops_query <- paste( "ALTER TABLE", route_tab, 
                                "ADD stop_geom Geometry(MULTIPOINT);",
                                sep = " ")
            
          dbSendQuery( connection, stops_query )
            
          seg_query <- paste( "ALTER TABLE", route_tab, 
                              "ADD seg_geom Geometry(LINESTRING);",
                              sep = " ")
        
          dbSendQuery( connection, seg_query )
            
          for(x in 1:length(all_seg_df[,6])) {
              
            add_stops <- paste( "UPDATE ", route_tab,
                              " SET stop_geom = ", "'",
                               all_seg_df[x,6], "' where seg_id IN ('", all_seg_df[x,1], "');", sep = "")
              
            dbSendQuery( connection, add_stops )
              
          }
            
          for(x in 1:length(all_seg_df[,7])) {
              
            add_stops <- paste( "UPDATE ", route_tab,
                                " SET seg_geom = ", "'",
                                all_seg_df[x,7], "' where seg_id IN ('", all_seg_df[x,1], "');", sep = "")
              
            dbSendQuery( connection, add_stops )
            
          }
            
        } else if ( dir_schema == "dir1routes" ) {
          
          route_points_df <- cbind(lon <- dir_wmata_routes_lst[[i]][[a]]$Direction1$Shape$Lon,
                                   lat <- dir_wmata_routes_lst[[i]][[a]]$Direction1$Shape$Lat,
                                   seqnum <- dir_wmata_routes_lst[[i]][[a]]$Direction1$Shape$SeqNum)
          
          route_points <- SpatialPointsDataFrame(
            SpatialPoints(route_points_df[,1:2], CRS(wgs84)),
            data = data.frame(route_points_df[,3])
          )
          
          # do the following method twice to increase numbe of points in bus_route.points:
          
          # 1) find midpoint between zth and z+1th point.
          # 2) find midpoint between zth and midpoint
          # 3) find midpoint between midpoint and z+1th point
          
          # the reason to do this is to make points much closer to our bus stops
          
          for(z in 1:(length(route_points@coords[,1])-1)) {
            
            e <- route_points@coords[z,]
            
            f <- route_points@coords[z+1,]
            
            mid <- cbind((e[1]+f[1])/2,(e[2]+f[2])/2)
            
            amid <- cbind((e[1]+mid[1])/2,(e[2]+mid[2])/2)
            
            midb <- cbind((mid[1]+f[1])/2,(mid[2]+f[2])/2)
            
            better_seg_points <- rbind(e, amid, mid, midb, f)
            
            row.names(better_seg_points) <- stri_rand_strings(
              length(better_seg_points[,1]), 
              length=5, pattern = "[A-Za-z0-9]")
            
            # for pair of coordinates, initailize better_seg_points_fin and get rid of garbage.
            
            if (z == 1) {
              
              better_seg_points_fin = 0
              
              better_seg_points_fin = data.frame(rbind(better_seg_points_fin,better_seg_points))
              
              better_seg_points_fin = better_seg_points_fin[better_seg_points_fin[,1] > 0,]
              
            } else {
              
              # for all scenarios except the last, rbind, or extend rows, of better_seg_points_fin with better_seg_points
              
              if ( z < (length(route_points@coords[,1])-1)) {
                
                better_seg_points_fin = rbind(better_seg_points_fin,better_seg_points)
                
              } else {
                
                # in last scenario, make better_seg_points_fin SpatialPointsDataFrame
                
                route_points <- spTransform(SpatialPoints(rbind(better_seg_points_fin,better_seg_points),
                                                          CRS(wgs84)),
                                            CRS(msp))
                
                route_points.ids <- seq(length(route_points@coords[,1]))
                
                route_points <- SpatialPointsDataFrame(route_points, data= data.frame(id=route_points.ids))
                
              }
              
            }
          }
          
          all_seg_df = data.frame(seg_id=1,stop_a_id=2,stop_b_id=3,stop_a_nam=4,stop_b_nam=5,stops_geom=6,seg_geom=7)
          
          for ( c in 1: length( dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$StopID ) ) {
            
            if (c == length( dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$StopID )) {
              
            } else {
              
              # create segment_id
              
              segment_id <- paste( dir_wmata_routes_lst[[i]][[a]]$RouteID, sprintf("%02d",c), sep = "_")
              
              # get stops at route segment of interest
              
              seg_stops_id <- cbind( dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$StopID[c],
                                     dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$StopID[c+1])
              
              # get stop names at route segment of interest
              
              seg_stops_nams <- cbind( gsub("'","_",gsub(" ", "_" ,
                                                         dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Name[c])),
                                       gsub("'","_",gsub(" ", "_" ,
                                                         dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Name[c+1]))
              )
              
              # get stop latlngs at route segment of interest and format for postgis
              
              seg_stops <- paste("MULTIPOINT(",
                                 dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Lon[c], " ",
                                 dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Lat[c], ",",
                                 dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Lon[c+1], " ",
                                 dir_wmata_routes_lst[[i]][[a]]$Direction1$Stops$Lat[c+1],")",sep="")
              
              # create spdf of segment bus stops, buffer them, and select bus_route.points within buffer
              
              seg_stops_points <- readWKT(seg_stops,p4s=CRS(wgs84))
              
              row.names(seg_stops_points) <- seq(length(seg_stops_points@coords[,1]))
              
              seg_stops_points <- spTransform(seg_stops_points,CRS(msp))
              
              seg_stops_points <- SpatialPointsDataFrame(
                seg_stops_points,
                data=data.frame(id=seq(length(seg_stops_points@coords[,1])))
              )
              
              buff_dist = 50
              
              buff_seg_stops_points <- gBuffer(seg_stops_points, buff_dist, id=seg_stops_points$id, byid=TRUE)
              
              points_within_buff <- route_points[buff_seg_stops_points, ]
              
              # while loop to increase buffer distance until more than 1 point selected 
              
              # continue increasing until:
              # 1) more than one point is selected by buffer OR
              # 2) the buffer around  the  first spatial point selects something OR 
              # 3) the buffer around the second spatial point selects something
              
              
              # increase the buffer distance by 10 feet and reselect points. 
              
              while ( length(points_within_buff) <= 1 |
                      length(route_points[buff_seg_stops_points[2,], ]@coords) < 2 | 
                      length(route_points[buff_seg_stops_points[1,], ]@coords) < 2  )
              {
                
                buff_dist = buff_dist + 10
                
                buff_seg_stops_points <- gBuffer(seg_stops_points, buff_dist, id=seg_stops_points$id, byid=TRUE)
                
                points_within_buff <- route_points[buff_seg_stops_points, ]
                
              }
              
              bus_route_startp <- which.min(gDistance(seg_stops_points[1,],points_within_buff,byid=T))
              
              bus_route_endp <- which.min(gDistance(seg_stops_points[2,],points_within_buff,byid=T))
              
              # check to see if the selected start and end points are the same. if they are, then use
              # the end point with shorter distance and opposite line_spatialpnts endpoint to make the this_bus_route variable
              
              if( bus_route_startp == bus_route_endp ) {
                
                if( gDistance(seg_stops_points[1,],points_within_buff) < gDistance(seg_stops_points[2,],points_within_buff) ) {
                  
                  this_bus_route <- SpatialPoints(rbind(seg_stops_points[2,]@coords,
                                                        points_within_buff[bus_route_startp,]@coords),CRS(msp))
                  
                } else {
                  
                  this_bus_route <- SpatialPoints(rbind(seg_stops_points[1,]@coords,
                                                        points_within_buff[bus_route_endp,]@coords),CRS(msp))
                  
                }
                
              } else {
                
                # update points_within_buff
                
                points_within_buff <- points_within_buff[bus_route_startp:bus_route_endp,]
                
                ### create sequence of all points from the 1st to last point in points.within.buffer
                
                points_within_buff_index <- seq(from=as.numeric(as.character(points_within_buff@data$id))[1],
                                                to=tail(as.numeric(as.character(points_within_buff@data$id)),n=1))
                
                ### create spatialpoints dataframe of all points in this sequence, project it back to wgs84 
                
                this_bus_route <- spTransform(route_points[as.numeric(as.character(route_points@data$id)) %in% points_within_buff_index,],
                                              CRS(msp))
              }
              
              ### create wkt line from this_bus_route
              
              wkt_xy_list <- list()
              
              for( m in 1:length(this_bus_route@coords[,1]) ) {
                
                wkt_xy <- paste(as.character(this_bus_route@coords[m,1]),
                                as.character(this_bus_route@coords[m,2]), sep=" ")
                
                wkt_xy_list[[ length( wkt_xy_list ) + 1L ]] <- wkt_xy
                
              }
              
              # take the dataframe we made and convert to linestring
              
              seg_linestring <- paste("LINESTRING(",
                                      str_c(unlist(wkt_xy_list),collapse=', '),
                                      ")",sep="")
              
              # write all segment attributes to dataframe
              
              seg_df <- data.frame( segment_id,
                                    seg_stops_id,
                                    seg_stops_nams,
                                    seg_stops,
                                    seg_linestring )
              
              # coerce clean column names. 
              
              names(seg_df) <- names(all_seg_df)
              
              all_seg_df <- rbind(all_seg_df,seg_df)
              
              if ( c == 1 ) {
                
                all_seg_df = all_seg_df[2,]  
                
              }
              
              
            } 
            
          }
          
          # intialize table, writing first row
          
          dbWriteTable( connection,  c( dir_schema , route_id ), all_seg_df[,1:5])
          
          stops_query <- paste( "ALTER TABLE", route_tab, 
                                "ADD stop_geom Geometry(MULTIPOINT);",
                                sep = " ")
          
          dbSendQuery( connection, stops_query )
          
          seg_query <- paste( "ALTER TABLE", route_tab, 
                              "ADD seg_geom Geometry(LINESTRING);",
                              sep = " ")
          
          dbSendQuery( connection, seg_query )
          
          for(x in 1:length(all_seg_df[,6])) {
            
            add_stops <- paste( "UPDATE ", route_tab,
                                " SET stop_geom = ", "'",
                                all_seg_df[x,6], "' where seg_id IN ('", all_seg_df[x,1], "');", sep = "")
            
            dbSendQuery( connection, add_stops )
            
          }
          
          for(x in 1:length(all_seg_df[,7])) {
            
            add_stops <- paste( "UPDATE ", route_tab,
                                " SET seg_geom = ", "'",
                                all_seg_df[x,7], "' where seg_id IN ('", all_seg_df[x,1], "');", sep = "")
            
            dbSendQuery( connection, add_stops )
            
          }
          
        }

    }
    
  }

##########################################################################################
