##########################################################################################
# Programmer: Maximus
# Name: route_lateness
# Purpose: Create time stamped dataframe for bus lateness 
# Method: 
# a: connect with database, created in stops_spatializer.R
# b: call api to get deviation from schedule for each route
# c: spatial query busses with buffered segs. write bus data to segs. which in detail is to:
  # iterate over deviations_lst and do the following:
  
    # get db schema name
    # initialize dir_bus_list, holding lists for each bus route that hold segment ids for segs busses are on, their deviation, and datetime
    # iterate over each item in deviations_lst[[i]]
      # grab seg_ids and seg_geoms for route and save to route_df
      # select busses from deviation_lst[[i]] along route deviation_lst[[i]]$RouteID[j]
      # init bus_lst and seg_lst. bus_lst holds seg ids for segs busses are on, their deviation, and datetime. seg_id the seg ids for busses 
      # for each segment in route_df
        # spatialize the seg_geom linestring text
        # buffer the segment by 50 feet, passing a FLAT cap style, meaning the buffer is only parallel to the line's segment
        # select bus points wihtin the buffer
        # if buffer selects points do the following for each selected point
        # add segment id, the deviation, bus_id, date time, and row number to bus_lst. add seg_id to bus_lst
          # note the reason for adding row number is that it helps populate max_busses, which the max number of busses 
          # held by all segments within bus_lst. This number, max_busses is later added to max_busses_lst. From max_busses_lst
          # the maximum value, and resaving it to max_busses. This then represent the greatest number of busses
          # selected by any given bus route segment. This number is the used to create the deviation, bus_id, and datetime
          # columns. then the row_number element is used to guide each element in bus_lst to the correct column. 
        # if the length of the bus_lst is 0, suggesting no busses were selected by the route, then go to the next route
        # else, change the datetime reported by wmata to a format matching that acceted b postgresql
        # add bus_lst to dir_bus_lst
        # find max_busses by selecting max number of repeting segment ids in seg_lst
        # add max_busses to max_busses_lst
      # generate base column name for deviation, busid, and datetime
        # find maximum max_value with max_busses_list and iterate over it to make that many column for the three columns of interest
      # iterate over dir_bus_lst and dp the following
        # for each element in dir_bus_lst[[j]]
        # save dir_bus_lst[[j]] to bus_lst
        # save deviation_lst[[i]]$RouteID[j] to route_id
        # generate route_tab with the dir_schema and route_id
        # for each item in bus_lst
        # Use 5th element in current bus_lst item to create column names to which data should be saved
        # save element data to database

##########################################################################################

### Conditionally install/load needed packages ###

# list of packages used in script
packages <- c( "jsonlite", "RPostgreSQL", "rgdal", "rgeos" )

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


### a: connect with database ###

# get name of database

database_name <- "wmata_routes"

# set up driver and connect to database

drv <- dbDriver("PostgreSQL")

connection <- dbConnect( drv = drv,
                         dbname = database_name,
                         host = "localhost",
                         port = 5432 )

##########################################################################################

### b: call api for deviations ###

# get api key

key <- "dd7ff89ac1744089adfa326205e723b4"

######## something to iterate the below over and over ########

deviation_time <- Sys.time()

# get deviations

wmata_deviations <- fromJSON( txt = system( paste(
  'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jBusPositions" -H "api_key: ',
  key,' "', sep = "" ) , intern=TRUE ))

##########################################################################################

### add results to df, add to named talbe in deviation schema ###

# separate wmata_deviations by direction

wmata_deviations_dir0 <- wmata_deviations$BusPositions[
  wmata_deviations$BusPositions$DirectionNum %in% 0,] 

wmata_deviations_dir1 <- wmata_deviations$BusPositions[
  wmata_deviations$BusPositions$DirectionNum %in% 1,] 

# index both deviations for only those on routes within the database. 

db_dir0_routes <- fetch(dbSendQuery( connection, 
                    paste("SELECT table_name FROM information_schema.tables WHERE table_schema = ", 
                      shQuote("dir0routes"),";",sep="")))[,1]

db_dir1_routes <- fetch(dbSendQuery( connection, 
                                     paste("SELECT table_name FROM information_schema.tables WHERE table_schema = ", 
                                           shQuote("dir1routes"),";",sep="")))[,1]

db_dir0_routes_index <- list()

db_dir0_routes_index[[ length(db_dir0_routes_index) +1L ]] <- 
  
  sapply(wmata_deviations_dir0$RouteID, function(x) x %in% db_dir0_routes)

db_dir1_routes_index <- list()

db_dir1_routes_index[[ length(db_dir1_routes_index) +1L ]] <- 
  
  sapply(wmata_deviations_dir1$RouteID, function(x) x %in% db_dir1_routes)

wmata_deviations_dir0 <- wmata_deviations_dir0[unlist(db_dir0_routes_index),]

wmata_deviations_dir1 <- wmata_deviations_dir1[unlist(db_dir1_routes_index),]

# add both wmata_deviation_dir returns to deviations_lst
        
deviations_lst <- list(wmata_deviations_dir0, wmata_deviations_dir1)

names(deviations_lst) <- c("dir0routes", "dir1routes")

for (i in 1:length(deviations_lst)) {
  
  dir_busses <- list()
  
  for( j in 1:length(unique(deviations_lst[[i]]$RouteID)) ) {
    
    busses <- deviations_lst[[i]][deviations_lst[[i]]$RouteID %in% unique(deviations_lst[[i]]$RouteID)[j], ]
    
    dir_busses[[ length(dir_busses) + 1L ]] <- busses
    
  }
  
  # get route table. route table = "schema"."route"
  
  dir_schema <- names(deviations_lst)[i]
  
  # create lists holding route_ids busses-segment info and max bussses on a given segment 
  
  dir_bus_lst <- list()
  
  max_busses_lst <- list()
  
  # last create a route_id_lst. This will hold all ids for routes which busses were selectd
  
  route_id_lst <- list()
  
  for( j in 1:length(dir_busses) ) {
    
    route_id <- dir_busses[[j]]$RouteID[1]
    
    route_tab <- paste( shQuote( dir_schema , type = 'cmd'), 
                        shQuote( route_id, type = 'cmd'), sep = ".")
    
    # use route tab to get dataframe with seg_id and seg_geom
    
    route_query <- paste("SELECT seg_id, St_AsText(seg_geom) FROM ", route_tab, ";", sep="")
  
    route_df <- fetch(dbSendQuery(connection, route_query))

    # select busses along route of interest
    
    busses <- dir_busses[[j]]

    busses <- SpatialPointsDataFrame(
                    spTransform(SpatialPoints(cbind(busses$Lon,busses$Lat),CRS(wgs84)),CRS(msp)),
                    data = data.frame(Deviation=busses$Deviation,DateTime=busses$DateTime,VehicleID=busses$VehicleID))
  
    bus_lst <- list()
    
    seg_lst <- list()
    
    for( k in 1:length( route_df$st_astext )) {
      
      seg <- readWKT( route_df$st_astext[k] , p4s = msp )
      
      buff_seg <- gBuffer(seg, width=50, capStyle = "FLAT")
      
      points_in_buff <- busses[buff_seg,]
      
      if ( length(points_in_buff) >= 1 ) {
        
        for ( m in  1:length(points_in_buff) ) {
          
          bus_lst[[ length(bus_lst) + 1L ]]  <- c( route_df$seg_id[k], 
                                                   points_in_buff$Deviation[m], 
                                                   as.character(points_in_buff$DateTime)[m],
                                                   as.character(points_in_buff$VehicleID)[m],
                                                   (1:nrow(points_in_buff))[m] )
          
          seg_lst[[ length(seg_lst) + 1L ]] <- route_df$seg_id[k]
          
        }
      }
    }
    
    if ( length(bus_lst) == 0 ) {
      
    } else {
      
      # add route_id to route_id_lst
      
      route_id_lst[[ length(route_id_lst) + 1L ]] <- route_id
      
      # alter date to match db format
      
      # get day character
      
      day_char <- substring(bus_lst[[1]][[3]], 11,11)
      
      # replace day character with space
      
      for ( k in 1:length(bus_lst)) {
        
        bus_lst[[k]][3] <- gsub(day_char," ", bus_lst[[k]][3])
        
      }
      
      # append bus_lst to dir_bus_lst
      
      dir_bus_lst[[ length(dir_bus_lst) + 1L ]] <- bus_lst
      
      # get max number of busses attributed to given seg and append it to max_busses_lst
      
      max_busses <- max(rle(unlist(seg_lst))$lengths)
      
      max_busses_lst[[ length(max_busses_lst) + 1L ]] <- max_busses
      
      # add max_busses number of columns, for both deviations and bus names, to respective lists
      
    }
    
  }
  
  route_id_lst <- unlist(route_id_lst)
  
  deviation_col_name_base <- paste(deviation_time,"_dev", sep="")
  
  busid_col_name_base <- paste(deviation_time,"_bus", sep="")
  
  time_col_name_base <- paste(deviation_time,"_time", sep="")
  
  max_busses <- max(unlist(max_busses_lst))
  
  for ( j in 1:length(route_id_lst) ) {
    
    bus_lst <- unlist(dir_bus_lst[j], recursive=FALSE)
    
    route_id <- route_id_lst[j]
    
    route_tab <- paste( shQuote( dir_schema , type = 'cmd'), 
                        shQuote( route_id, type = 'cmd'), sep = ".")
    
    
    for ( k in 1:max_busses ) {
        
      deviation_col_name <- shQuote(paste(deviation_col_name_base, as.character(k), sep=""), type="cmd")
        
      busid_col_name <- shQuote(paste(busid_col_name_base, as.character(k),sep=""), type="cmd")
        
      time_col_name <- shQuote(paste(time_col_name_base, as.character(k),sep=""), type="cmd")
        
      deviation_col_query <- paste( "ALTER TABLE ", route_tab, 
                                    " ADD ", deviation_col_name,
                                    " int;", sep = "")
        
      busid_col_query <- paste( "ALTER TABLE ", route_tab, 
                                " ADD ", busid_col_name,
                                " int;", sep = "")
        
      time_col_query <- paste( "ALTER TABLE ", route_tab, 
                               " ADD ", time_col_name,
                               " TIMESTAMP;", sep = "")
        
      dbSendQuery( connection, deviation_col_query )
      
      dbSendQuery( connection, busid_col_query )
        
      dbSendQuery( connection, time_col_query )
        
    }  
      
    for ( k in 1:length(bus_lst)) {
      
      deviation_col_name <- shQuote(paste(deviation_col_name_base, as.character(bus_lst[[k]][5]), sep=""), type="cmd")
      
      busid_col_name <- shQuote(paste(busid_col_name_base, as.character(bus_lst[[k]][5]), sep=""), type="cmd")
      
      time_col_name <- shQuote(paste(time_col_name_base, as.character(bus_lst[[k]][5]), sep=""), type="cmd")
      
      deviation_row_query <- paste( "UPDATE ", route_tab, 
                                    " SET ",deviation_col_name, " = ",
                                    bus_lst[[k]][2], 
                                    " where seg_id IN ('", bus_lst[[k]][1] ,
                                    "')",";", sep="")
      
      busid_row_query <- paste( "UPDATE ", route_tab, 
                                " SET ", busid_col_name, " = ",
                                bus_lst[[k]][4], 
                                " where seg_id IN ('", bus_lst[[k]][1] ,
                                "')",";", sep="")
      
      time_row_query <- paste( "UPDATE ", route_tab, 
                               " SET ", time_col_name, " = ",
                               shQuote(bus_lst[[k]][3]), 
                               " where seg_id IN ('", bus_lst[[k]][1] ,
                               "')",";", sep="")
      
      dbSendQuery( connection, deviation_row_query )
      
      dbSendQuery( connection, busid_row_query)
      
      dbSendQuery( connection, time_row_query )    
      
    }

  }  
  
}


  
  
  
  
  
  
  
  
  





