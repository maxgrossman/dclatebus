##########################################################################################
# Programmer: Maximus
# Purpose: Create SPDF and geojson of WMATA bus routes and each route's stops. 
         # This will be used for viz later
# Method: 
        # a: call api for route names and stops.
        # b: use forloop to create bus route paths and stops from stop json:
            # here, for each route in the returned JSON I find stops that serve that route
            # two spatial dataframes are created. The first with route as a line.
            # the second as a point file with the stop names

##########################################################################################

setwd("/Users/giscomputerextra2/Desktop/max/github/maxgrossman/dclatebus/data")

### Conditionally install/load needed packages ###

# list of packages used in script
packages <- c( "jsonlite", "rgdal", "rgeos", "raster", "maptools", "devtools" )

# coerce response from installed.packages() to be clean data.frame of packages installed
packages_installed <- as.data.frame( installed.packages()[,c(1,3:4)] )

# remove rownames
rownames( packages_installed ) <- NULL

# use null value found in $Priority, this signifies already being downloaded
packages_installed <- packages_installed[is.na(
  packages._nstalled$Priority),
  1:2,drop=FALSE]

# get true/false index of packages on computer. true here is NOT INSTALLED
packages_index <- packages[!packages %in% as.character(packages.installed$Package)]

# if packages_index length greater than 1, install the packages in _packages_index

if( packages_index < 1 ) { 
  
  install.packages(packages_index)
  
} 

for(package in packages) {
  
  library( package, character.only = TRUE)

} 


### a: call api for routes ###

# get api key

key<-readline( prompt = "Provide API Key: ")

# call routes api from cmd, save to wmata.routes with fromJSON. makes list with many nested lists
wmata_routes <- fromJSON( txt = system( paste(
  'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jRoutes" -H "api_key: ',
  key,' "', sep = "" ) , intern=TRUE ))

# call bus stops api

wmata_stops <- fromJSON( txt = system( paste(
  'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jStops" -H "api_key: ',
  key,' "', sep = "" ) , intern=TRUE ))

# Loop over RouteIDs in wmata.routes to see which bus stops they pass through, getting
# index of those bus stops and then using that index to create spatial points data frame
# with 1) the location of the stop, 2) the name of the stop. write these out to shapefiles

for( route_id in wmata_routes$Routes$RouteID ) { 
    
    # index the wmata_stops to get only those route_id passes through
    routes_index <- grep( route_id , wmata_stops$Stops$Routes )
    
    # grab stop names from wmata_stops wit the routes_index
    stops_nam <- wmata_stops$Stops$Name[ routes_index ]
    
    # make stops_loc, a SpatialPoints using lat,lon of indexed wmata_stops
    stops_loc <- SpatialPoints( cbind( wmata_stops$Stops$Lon[ routes_index],
                                       wmata_stops$Stops$Lat[ routes_index] ),
                                CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    )
    
    # create stops_df with the name of the stops, used next making routes_spdf
    stops_df <- data.frame( nam = stops_nam )

    # make the routes spdf 
    stops_spdf <- SpatialPointsDataFrame( stops_loc, data = stops_df )
    
    # write these out to shapefiles
    writeOGR( obj = stops_spdf, 
              dsn = ".", 
              layer = paste( route_id, "stops", sep = "_" ),
              driver = "ESRI Shapefile"
              )
    
  }




### c: make spdf for direction0 and direction1 lines ###

# get direction0 lat,lng for each in lst.wmata.path to make a dir0.lst.wmata.path

# first subset the lst.wmata.path to only those routes with a direction0 that is not null.

dir0.lst.path <- lst.wmata.path[!sapply(
  seq_along(lst.wmata.path),
  function(i) {
    # an if else statement to return index of items in for those routes with stop geometry objects that aren't null
    if(is.null(lst.wmata.path[[i]]$Direction0$Stops)){} else {
      if(length(lst.wmata.path[[i]]$Direction0$Stops)>1) {i}
      }
    }
  ) %in% "NULL"]               

# make each of those routes into spldf, spatial line data frames

spldf.dir0.lst.path <- sapply(
  dir0.lst.path,
  function(x) 
  {
  # credit StackExchange user fdetsch at tinyurl.com/zdqyewj for how this was figured out. Thanks dude. 
    
  # get matxix of latlon from JSON in lst.wmata.path
  latlog.busLine <- cbind(x$Direction0$Stops$Lon, x$Direction0$Stops$Lat)
    
  # pass latlon.busLine through Line() to make it a line
  line.busLine <- Line(latlog.busLine)
    
  # pass line.busLine through Lines() as a list, giving an ID to it, here the route and direction
  line.busLine <- Lines(list(line.busLine), ID=paste(x$RouteID,x$Direction0$DirectionText,sep="."))
    
  # pass line.busLine through SpatialLines as a list again, set its CRS. here wgs84 is best tinyurl.com/jamjxxl so proj lata
  busLine <- SpatialLines(list(line.busLine),CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    
  # with busLine create a dataframe, keeping both length of route and the name of the route 
  df.busLine <- data.frame(len = sapply(1:length(busLine), function(i) gLength(busLine[i, ])),
                           route = sapply(1:length(busLine), function(i) busLine@lines[[i]]@ID))
    
  # apply ID set when creating line.busLine (on line 67) to give name to df.busLine rows
  rownames(df.busLine) <- sapply(1:length(busLine), function(i) busLine@lines[[i]]@ID)
  
  # make SpatialLinesDataFrame from x that will be returned at x's index in dir0.lst.wmata.path
  SpatialLinesDataFrame(busLine,df.busLine)
  })

# merge this list of slpdfs into one. 
spldf.dir0.paths <- do.call(bind, spldf.dir0.lst.path)

# write out to shapefile
writeOGR(spldf.dir0.paths,".",'dir0.paths',driver="ESRI Shapefile",overwrite_layer = TRUE)
