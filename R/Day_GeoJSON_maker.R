
##########################################################################################
# Programmer: Maximus
# Title: Day_GeoJSON_maker.R
# Purpose: take database each day and write to GeoJSON
# Method: 
  # a: connect to temp database
  # b: for each table in each schema, create json for segments. save those to a JSON for said route
  # c: add those route JSONS to direction JSON
  # d: combine both directoins to single JSON and write to /data folder. 

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

# function makes string to write out to geojson. particularly it does the following with a geom_lst, holding geometry for each segment and properties_lst, holding property data for each seg:
  # create list of features held within the geojson
  # for loop over properties_lst
    # generate the correct geometry object holding the geometry array held in geom_lst
    # generate the correct properties object from the objects held in the properties_lst
    # combine these together into a the feature object
    # add the feature variable the features_lst
  # unlist the features_lst
  # save a string of all elements in features_lst to a string, collapsing (separating within the string) each element with a comma
  # return a string that represents a feature collection geojson.

route_geojson_maker <- function(geom_lst,properties_lst) {
  
  features_lst <- list()
  
  for( i in 1:length(properties_lst)) {
    
    geometry <- paste('"geometry": { "type": "LineString",', '"coordinates":', geom_lst[i],'},')
    
    properties <- paste('"properties": {', properties_lst[i] ,'}')

    feature <-  paste('{ "type": "Feature",', geometry, properties, "}")
      
    features_lst[[ length(features_lst) + 1L ]] <- feature
    
  }
  
  features_lst <- unlist(features_lst)
  
  features <- paste(features_lst,collapse=", ")
  
  return(paste('{ "type": "FeatureCollection", "features": [',features,']}'))
  
  
  
}


##########################################################################################

# create list with routes in dir0 and dir1 and iterate over it to create elements of geojson

db_dir0_routes <- fetch(dbSendQuery( connection, 
                                     paste("SELECT table_name FROM information_schema.tables WHERE table_schema = ", 
                                           shQuote("dir0routes"),";",sep="")))[,1]

db_dir1_routes <- fetch(dbSendQuery( connection, 
                                     paste("SELECT table_name FROM information_schema.tables WHERE table_schema = ", 
                                           shQuote("dir1routes"),";",sep="")))[,1]

db_dir_routes <- list(db_dir0_routes, db_dir1_routes)

names(db_dir_routes) <- c("dir0routes","dir1routes")

for( i in 1:length(db_dir_routes)) {
  
  dir_schema <- names(db_dir_routes)[i] 
  
  for ( j in 1:length(db_dir_routes[[i]])) {
    
    route_id <- db_dir_routes[[i]][j]
   
    route_tab <- paste( shQuote( dir_schema , type = 'cmd'), 
                        shQuote( route_id, type = 'cmd'), sep = ".")
    
    route_main_query <- paste("SELECT * FROM ", route_tab, ";", sep="")
    
    route_seg_geom_query <- paste("SELECT ST_AsText(seg_geom) FROM ", route_tab, ";", sep="")
    
    route_df <- fetch(dbSendQuery(connection, route_main_query))
    
    if ( ( TRUE %in% grepl("dev", names(route_df))) == TRUE ) {

      route_segs <- cbind(route_df[,2],fetch(dbSendQuery(connection, route_seg_geom_query)))
      
      route_dev <- cbind(route_df[,2],route_df[grepl("dev", names(route_df))])
      
      route_bus <- cbind(route_df[,2],route_df[grepl("bus", names(route_df))])
      
      route_time <- cbind(route_df[,2],route_df[grepl("time", names(route_df))])
      
      bool_lst <- list()
      
      for ( k in 1:length(names(route_df[grepl("dev", names(route_df))]))) {
        
        if ( k == 1 ) {
          
          bool <- TRUE
          
        } else {
          
          bool <- FALSE
          
        }
        
        bool_lst[[ length(bool_lst) + 1L ]] <- bool
        
      }
      
      bool_lst <- unlist(bool_lst)
      
      deviations_json_elements <- list()
      
      buses_json_elements <- list()
      
      times_json_elements <- list()
      
      for( k in 1:length(route_dev[,1])) {
        
        seg_id_json_value <- paste('"seg_id"',route_df[k,2],sep=" ; ")
        
        for( m in 1:length(names(route_df[grepl("dev", names(route_df))]))) {
          
          if ( route_dev[k,m+1] %in% NA ) {
            
            dev_val <- shQuote("null", type <- "cmd")
            
          } else { dev_val <- route_dev[k,m+1] }
          
          if ( route_bus[k,m+1] %in% NA ) {
            
            bus_val <- shQuote("null", type <- "cmd")
            
          } else { bus_val <- route_dev[k,m+1] }
          
          if ( route_time[k,m+1] %in% NA ) {
            
            time_val <- shQuote("null", type <- "cmd")
            
          } else { time_val <- shQuote(route_time[k,m+1],type="cmd") }
          
          deviations_json_element <- paste(shQuote(names(route_dev)[m+1],type="cmd"),dev_val, sep=" : ")
          
          buses_json_element <- paste(shQuote(names(route_bus)[m+1],type="cmd"),bus_val, sep=" : ")
          
          times_json_element <- paste(shQuote(names(route_time)[m+1],type="cmd"),time_val, sep=" : ")
          
          deviations_json_elements[[ length(deviations_json_elements) + 1L ]] <- deviations_json_element
          
          buses_json_elements[[ length(buses_json_elements) + 1L ]] <- buses_json_element
          
          times_json_elements[[ length(times_json_elements) + 1L ]] <- times_json_element
          
          if ( m == length(names(route_df[grepl("dev", names(route_df))]))) {
            
            deviations_json_elements <- unlist(deviations_json_elements)
            
            buses_json_elements <- unlist(buses_json_elements)
            
            times_json_elements <- unlist(times_json_elements)
            
          }
          
        }
        
      }
      
      properties_lst <- list()
      
      geom_lst <- list()
      
      for ( k in 1:length(as.character(seq_along(deviations_json_elements)[bool_lst])) ) {
        
        seg_geom_coords <- as(spTransform(readWKT(route_segs[k,2],p4s=msp),CRS(wgs84)),"SpatialPointsDataFrame")@coords
        
        seg_geom_coords_lst <- list()
        
        for ( m in 1:length(seg_geom_coords[,1])) {
          
          GeoJSONcoords <- paste("[", paste(seg_geom_coords[m,], collapse = " , "), "]")
          
          seg_geom_coords_lst[[ length(seg_geom_coords_lst) + 1L ]] <- GeoJSONcoords
          
        }
        
        seg_feature_geom <- paste("[", paste(seg_geom_coords_lst, collapse = ", ") ,"]")
        
        seg_id_feature_value <- paste(paste(shQuote("seg_id", type="cmd"),shQuote(route_df[k,2], type="cmd"), sep = " : "),",")
        
        if( k < length(as.character(seq_along(deviations_json_elements)[bool_lst])) ) {
          
          deviations_feature_values <-   paste(
            paste(
              deviations_json_elements[
                as.numeric( as.character(seq_along(deviations_json_elements)[bool_lst])[k]):
                  (as.numeric(as.character(seq_along(deviations_json_elements)[bool_lst])[k+1])-1)],
              collapse=" , "), ",", sep = "")
          
          buses_feature_values <-paste(
            paste(
              buses_json_elements[
                as.numeric( as.character(seq_along(buses_json_elements)[bool_lst])[k]):
                  (as.numeric(as.character(seq_along(buses_json_elements)[bool_lst])[k+1])-1)],
              collapse=" , "), ",", sep = "")
          
          times_feature_values <- paste(
            times_json_elements[
              as.numeric( as.character(seq_along(times_json_elements)[bool_lst])[k]):
                (as.numeric(as.character(seq_along(times_json_elements)[bool_lst])[k+1])-1)],
            collapse=" , ")
        } else {
          
          deviations_feature_values <-   paste(
            paste(
              deviations_json_elements[
                as.numeric( as.character(seq_along(deviations_json_elements)[bool_lst])[k]):
                  length(seq_along(deviations_json_elements))],
              collapse=" , "), ",", sep = "")
          
          buses_feature_values <-paste(
            paste(
              buses_json_elements[
                as.numeric( as.character(seq_along(buses_json_elements)[bool_lst])[k]):
                  length(seq_along(buses_json_elements))],
              collapse=" , "), ",", sep = "")
          
          times_feature_values <- paste(
            times_json_elements[
              as.numeric( as.character(seq_along(times_json_elements)[bool_lst])[k]):
                length(seq_along(times_json_elements))],
            collapse=" , ")
          
        }
        
        properties_lst[[ length(properties_lst) + 1L ]] <- paste(seg_id_feature_value, deviations_feature_values, buses_feature_values, times_feature_values)
        
        geom_lst[[ length(geom_lst) + 1L ]] <- seg_feature_geom
        
      }
      
      route_geojson <- route_geojson_maker(geom_lst,properties_lst)
      
      if ( dir_schema == "dir0routes") {
        
        file_path <- paste("/Users/maxgrossman/github/maxgrossman/dclatebus/data/dir0routes/",route_id,".geojson", sep = "")
        
      } else {
        
        file_path <- paste("/Users/maxgrossman/github/maxgrossman/dclatebus/data/dir1routes/",route_id,".geojson", sep = "")
        
      }
      
      write(route_geojson, file_path)
      
    }
  }
}

  
