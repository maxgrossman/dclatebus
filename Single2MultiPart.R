##########################################################################################
# Programmer: Maximus
# Title: Route2RouteSegs.R
# Purpose: Split DC Bus Routes into and write back to db as multiline-polygons. 
# Method: 
    # a: Separate DC Bus Route GeoJSON for dir0 (N and E), dir1 (S and W) 
    # b: Iterate via loop with nested loops:
          # 1) Exracting points for each Bus Route
          # 2) For each unique segment in DB representation of each bus route
                # i) buffer endpoints and select points from Bus Route with said buffer
                # ii) take points from Bus Routes selected and generate new line segment and write to DB

##########################################################################################

# install waveyR for cond_packages

install( "waveyR" )

cond_packages( packages = c("rgeos","geojsonio","rgdal") )

# load the DC Bus Routes GeoJSON, separate for dir0 and dir1

bus_routes <- topojson_read('http://opendata.dc.gov/datasets/35738eb6405f4bb0bfdceddb21ac3122_59.geojson?where=&geometry={"xmin":-8579480.865228798,"ymin":4707518.155392194,"xmax":-8568636.361840855,"ymax":4710785.838351382,"spatialReference":{"wkid":102100,"latestWkid":3857}}')

dir0_bus_routes <- bus_routes[as.character(bus_routes$DIRECTION) %in% c("N","E"),]

dir1_bus_routes <- bus_routes[as.character(bus_routes$DIRECTION) %in% c("S","W"),]

##########################################################################################
#TEST#

# Get dataframe of columns

col_n <- ncol(as.data.frame(as(bus_routes[120,], "SpatialPointsDataFrame")))

D5_E.points <- SpatialPointsDataFrame(
                SpatialPoints(
                 as.data.frame(as(bus_routes[120,], "SpatialPointsDataFrame"))[,(col_n - 1):col_n],
                 CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
                ),
                data=as.data.frame(as(bus_routes[120,], "SpatialPointsDataFrame"))[,1:2]
               )

# Select the route from db, and create a list with elements each segment's endpoints 

query_for_needed <- paste("SELECT St_AsText(seg_line) from ", shQuote( "dir0routes" , type = 'cmd'), ".",
                          shQuote( "D5", type = 'cmd'), " ;",sep="")


route_df = fetch(dbSendQuery(connection,query_for_needed),n=-1)

# create list with of buffers around each segments' stops
seg_buffers_df <- list()


for(i in 1:length(df[,1])) {
  line <- readWKT(route_df[i,1])
  line.pnts <- as.data.frame(as(line, "SpatialPointsDataFrame"))[,4:5]
  line.pnts[,6]<-rep("a,)
  line.spatialpnts <- SpatialPointsDataFrame(  
                 SpatialPoints(
                  cbind(as.data.frame(as(line, "SpatialPointsDataFrame"))[,4],
                        as.data.frame(as(line, "SpatialPointsDataFrame"))[,5]),
                  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),
                 data=line@route_df[i,2])
               
  
  line.dfs[[length(line.dfs)+1L]]<- line.pnts
}


