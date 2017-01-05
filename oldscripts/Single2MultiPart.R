##########################################################################################
# Programmer: Maximus
# Title: Route2RouteSegs.R
# Purpose: Split DC Bus Routes into and write back to db as multiline-polygons. 
# Method: 
    # a: Separate DC Bus Route GeoJSON for dir0 (N and E), dir1 (S and W) 
    # b: Extract more accurate bus segments with dir0 and dir1 bus routes:
          # 1) Get list of bus route table names for dir0 & dir1 from db schemas matching $RT_D in bus_routes 
          # 2) Make and loop over list holding dir0 & dir1
              # i) For over each bus route 
                # A) Extract points from bus route
                # B) Query DB and get dataframe with bus segment ids and bus segment linestrings
                # C) Buffer endpoints and select points from bus route points with buffer
                # D) take points from Bus Routes selected and generate new line segment and write to DB

##########################################################################################

# install waveyR for cond_packages and connect to database
library(devtools)
install( "/Users/maxgrossman/github/maxgrossman/waveyR" )
library(waveyR)

cond_packages( c("rgeos","geojsonio","rgdal", "RPostgreSQL","stringr","stringi") )

drv <- dbDriver("PostgreSQL")

connection <- dbConnect( drv = drv,
                         dbname = "wmata_routes",
                         host = "localhost",
                         ##passward = 
                         port = 5432 )

##########################################################################################

### a: subset bus_routes for two dir0 and dir1 ###

# load the DC Bus Routes GeoJSON, separate for dir0 and dir1 and subset to those within db

bus_routes <- topojson_read("http://opendata.dc.gov/datasets/35738eb6405f4bb0bfdceddb21ac3122_59.geojson")

dir0_bus_routes <- bus_routes[as.character(bus_routes$DIRECTION) %in% c("N","E"),]

dir1_bus_routes <- bus_routes[as.character(bus_routes$DIRECTION) %in% c("S","W"),]

bus_route_dir <- c(dir0_bus_routes,dir1_bus_routes)

names(bus_route_dir) <- c("dir0routes","dir1routes")

# create index of routes in database and sub lists to only those routes

bus_route_dir_index <- list()

for( i in 1:length(bus_route_dir) ) {
  
  bus_route_dir_index[[ length(bus_route_dir_index)+1L ]] <- 
    
    sapply(bus_route_dir[[i]]$RT_D, 
      function(x) 
        substr(as.character(x),1,(nchar(as.character(x))-2)) 
          %in% 
        grep("v",fetch(dbSendQuery( connection,
          paste("SELECT table_name FROM information_schema.tables WHERE table_schema = ", 
                shQuote(names(bus_route_dir)[i]),";",sep="")),n=-1)[,1],invert=TRUE,value=TRUE))
  
}

for ( i in 1:length(bus_route_dir)) {
  bus_route_dir[[i]] <- bus_route_dir[[i]][bus_route_dir_index[[i]],]
  bus_route_dir[[i]] <- bus_route_dir[[i]][(order(bus_route_dir[[i]]$RT_D)),]
  bus_route_dir[[i]] <- bus_route_dir[[i]][!duplicated(bus_route_dir[[i]]$RT_D),]
}

##########################################################################################

for ( i in 1:length(bus_route_dir) ) { 
  
  for( j in 1:length(bus_route_dir[[i]]$RT_D) ) {
    
    # generate matching route db name
    
    db_route_name <- substr(
      as.character(bus_route_dir[[i]]$RT_D[j]),
      1,(nchar((as.character(bus_route_dir[[i]]$RT_D[j])))-2))
    
      ## create dataframe of all points within route line
      
      col_n <- ncol(as.data.frame(
        as(bus_routes[bus_routes$RT_D %in% bus_route_dir[[i]]$RT_D[j], ], 
           "SpatialPointsDataFrame")))
      
      bus_route.points <- SpatialPointsDataFrame(
        SpatialPoints(
          as.data.frame(
            as(bus_routes[bus_routes$RT_D %in% bus_route_dir[[i]]$RT_D[j],], 
               "SpatialPointsDataFrame"))[,(col_n - 1):col_n],
          CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),
        data=data.frame(id = as.character(seq(length(as.data.frame(
          as(bus_routes[bus_routes$RT_D %in% bus_route_dir[[i]]$RT_D[j],], 
             "SpatialPointsDataFrame"))[,(col_n - 1)])))))
      
      bus_route.points <- spTransform(bus_route.points,
                                      CRS("+proj=lcc +lat_1=38.3 +lat_2=39.45 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
      
      buff_bus_route.line <- gBuffer(spTransform(bus_routes[bus_routes$RT_D %in% bus_route_dir[[i]]$RT_D[j], ],
                                    CRS("+proj=lcc +lat_1=38.3 +lat_2=39.45 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs")),
                                    50, byid=FALSE)
      
      
      # do the following method twice to increase numbe of points in bus_route.points:
      # 1) find midpoint between zth and z+1th point.
      # 2) find midpoint between zth and midpoint
      # 3) find midpoint between midpoint and z+1th point
      # the reason to do this is to make points much closer to our bus stops
      
      for(z in 1:(length(bus_route.points@coords[,1])-1)) {
          
          a <- bus_route.points@coords[z,]
          
          b <- bus_route.points@coords[z+1,]
          
          mid <- cbind((a[1]+b[1])/2,(a[2]+b[2])/2)
          
          amid <- cbind((a[1]+mid[1])/2,(a[2]+mid[2])/2)
          
          midb <- cbind((mid[1]+b[1])/2,(mid[2]+b[2])/2)
          
          better_seg_points <- rbind(a, amid, mid, midb, b)
          
          row.names(better_seg_points) <- stri_rand_strings(
            length(better_seg_points[,1]), 
            length=5, pattern = "[A-Za-z0-9]")
          
          if (z == 1) {
            
            better_seg_points_fin = 0
            
            better_seg_points_fin = data.frame(rbind(better_seg_points_fin,better_seg_points))
            
            better_seg_points_fin = better_seg_points_fin[better_seg_points_fin[,1] > 0,]
            
          } else {
            
            if ( z < (length(bus_route.points@coords[,1])-1)) {
              
              better_seg_points_fin = rbind(better_seg_points_fin,better_seg_points)
              
            } else {
              
              bus_route.points <- SpatialPoints(rbind(better_seg_points_fin,better_seg_points),
                                                CRS("+proj=lcc +lat_1=38.3 +lat_2=39.45 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
              
              bus_route.points <- bus_route.points[buff_bus_route.line,]
              
              bus_route.ids <- seq(length(bus_route.points@coords[,1]))
              
              bus_route.points <- SpatialPointsDataFrame(bus_route.points,
                                                         data= data.frame(id=bus_route.ids))
              
            }
          }
        }
    
      # Select the route from db, and create a df with each row the line string of a segment
    
      query_for_needed <- paste("SELECT id,St_AsText(seg_line) from ", 
                              shQuote( names(bus_route_dir)[i] , type = 'cmd'), ".",
                              shQuote( db_route_name, type = 'cmd'), " ;",sep="")
  
    
      route_df = fetch(dbSendQuery(connection,query_for_needed),n=-1)
      
      ### b: Extract more accurate points for segments ###
      
      for( k in 1:length(route_df [,1]) ) {
        
        # create spdf of segment bus stops, buffer them, and select bus_route.points within buffer
        
        line <- readWKT(route_df[k,2],p4s=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
        
        line_pnts <- as.data.frame(as(line, "SpatialPointsDataFrame"))[,4:5]
        
        line_pnts$id <- rep(route_df[k,1],length(line_pnts[,1]))
        
        line_spatialpnts <- SpatialPointsDataFrame(  
          SpatialPoints(
            cbind(as.data.frame(as(line, "SpatialPointsDataFrame"))[,4],
                  as.data.frame(as(line, "SpatialPointsDataFrame"))[,5]),
            CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),
          data=data.frame(id = line_pnts$id, seq.id = as.character(seq(length(line_pnts$id))))
        )
        
        line_spatialpnts <- spTransform(line_spatialpnts,
                                        CRS("+proj=lcc +lat_1=38.3 +lat_2=39.45 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
        
        buff_dist = 50
        
        buff_line_spatialpnts <- gBuffer(line_spatialpnts, 50, id=line_spatialpnts$seq.id, byid=TRUE)
        
        # get points within buffer
        
        points_within_buff <- bus_route.points[buff_line_spatialpnts, ]
        
        # while loop to increase buffer distance until more than 1 point selected 
      
        # continue increasing until:
        # 1) more than one point is selected by buffer OR
        # 2) the buffer around  the  first spatial point selects something OR 
        # 3) the buffer around the second spatial point selects something
       
        
        # increase the buffer distance by 10 feet and reselect points. 
        
        while ( length(points_within_buff) <= 1 |
                length(bus_route.points[buff_line_spatialpnts[2,], ]@coords) < 2 | 
                length(bus_route.points[buff_line_spatialpnts[1,], ]@coords) < 2  )
         {
          
          buff_dist = buff_dist + 10
          
          buff_line_spatialpnts <- gBuffer(line_spatialpnts, buff_dist , id=line_spatialpnts$seq.id, byid=TRUE)
          
          points_within_buff <- bus_route.points[buff_line_spatialpnts, ]
          
         }
        
        # declair end point the closets to first point in bus segment
        
        bus_route_startp <- which.min(gDistance(line_spatialpnts[1,],points_within_buff,byid=T))
        
        bus_route_endp <- which.min(gDistance(line_spatialpnts[2,],points_within_buff,byid=T))
        
        # check to see if the selected start and end points are the same. if they are, then use
        # the end point with shorter distance and opposite line_spatialpnts endpoint to make the this_bus_route variable
        
        if( bus_route_startp == bus_route_endp ) {
          
          if( gDistance(line_spatialpnts[1,],points_within_buff) < gDistance(line_spatialpnts[2,],points_within_buff) ) {
            
            this_bus_route <- SpatialPoints(
                                    rbind(line_spatialpnts[2,]@coords,
                                          points_within_buff[bus_route_startp,]@coords),
                                    CRS("+proj=lcc +lat_1=38.3 +lat_2=39.45 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
          } else {
              
            this_bus_route <- SpatialPoints(
                                    rbind(line_spatialpnts[1,]@coords,
                                      points_within_buff[bus_route_endp,]@coords),
                                    CRS("+proj=lcc +lat_1=38.3 +lat_2=39.45 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
              
            }
            
        } else {
  
          # update points_within_buff
          
          points_within_buff <- points_within_buff[bus_route_startp:bus_route_endp,]
          
          ### create sequence of all points from the 1st to last point in points.within.buffer
          
          points_within_buff_index <- seq(from=as.numeric(as.character(points_within_buff@data$id))[1],
                                          to=tail(as.numeric(as.character(points_within_buff@data$id)),n=1))
          
          ### create spatialpoints dataframe of all points in this sequence, project it back to wgs84 
          
          this_bus_route <- spTransform(bus_route.points[as.numeric(as.character(bus_route.points@data$id)) %in% points_within_buff_index,],
                                        CRS("+proj=lcc +lat_1=38.3 +lat_2=39.45 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
  
        }
        
        # get next_route_start for next iteration
        
        ### create wkt line from this_bus_route
        
        wkt_xy_list <- list()
        
        for( m in 1:length(this_bus_route@coords[,1]) ) {
          
          wkt_xy <- paste(as.character(this_bus_route@coords[m,1]),
                          as.character(this_bus_route@coords[m,2]), sep=" ")
          
          wkt_xy_list[[ length( wkt_xy_list ) + 1L ]] <- wkt_xy
          
        }
      
        this_bus_route_linestring <- paste("LINESTRING(",
                                           str_c(unlist(wkt_xy_list),collapse=', '),
                                           ")",sep="")
        
      }
    }
        
        
        if ( k == 1 ) {
          
          # create new column for more precise bus segments first time through
          
          new_seg_query <- paste("ALTER TABLE",
                                 shQuote( names(bus_route_dir)[i] , type = 'cmd'), ".",
                                 shQuote( db_route_name, type = 'cmd'),
                                 "ADD seg_line_b Geometry(LINESTRING);",
                                 sep = " ")
          
          dbSendQuery( connection, new_seg_query )
          
        }
        
        #### use the udpate thang.
        
        linestring_query <- paste("UPDATE ",
                                  shQuote( names(bus_route_dir)[i] , type = 'cmd'), ".",
                                  shQuote( db_route_name, type = 'cmd'), 
                                  " SET seg_line_b = ST_GeomFromText(",
                                  shQuote(this_bus_route_linestring),
                                  ")", " WHERE id IN ","(", route_df[k,1], ")", 
                                  ";", sep = "")
        
        dbSendQuery( connection, linestring_query )
        
        dbClearResult(dbListResults(connection)[[1]])
      }
  }
}

 





