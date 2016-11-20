##########################################################################################
# Programmer: Maximus
# Purpose: Create SPDF and geojson of WMATA bus routes and each route's stops. 
         # This will be used for viz later
# Method: 
        # a: call api for route names.
        # b: use forloop to get json for for each route, using the route names from a:
        # c: take these jsons and create spdf for route lines and stops, both in include
            # c_0: make line spdf for direction0 lines.
            # c_1: make line spdf for direction1 lines.
##########################################################################################

### Install needed packages, load them ###

# packages are installed conditionally. I only install if not already there

# list of packages used in script
packages <- c("jsonlite","rgdal","rgeos","raster")

# df of packages installed by user 

# coerce response from installed.packages() to be clean data.frame
packages.installed <- as.data.frame(installed.packages()[,c(1,3:4)])

# remove rownames
rownames(packages.installed) <- NULL

# use null in $Priority, returning only those packagse on computer
packages.installed <- packages.installed[is.na(
  packages.installed$Priority),
  1:2,drop=FALSE]

# get true/false index of packages on computer
packages.index <- packages[!packages %in% as.character(packages.installed$Package)]

# install list representing subset list of packages where when indexed by packages.index the returned value is false
install.packages(packages.index)

# load those packages in that subse
for(package in packages.index) {
  library(package, character.only = TRUE)
}

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
