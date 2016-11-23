drv <- dbDriver("PostgreSQL")
system("createdb wmata_test")
con <- dbConnect(drv, dbname = "wmata_test", host = "localhost")

query <- "CREATE SCHEMA test;"

dbSendQuery( con, query )

query1 <- "COMMENT SCHEMA test IS 'SCHEMA for test purpose."

setwd("/Users/giscomputerextra2/Desktop/max/github/maxgrossman/dclatebus/data")

stops16E <- readOGR(".","16E_stops")

dbWriteTable(con, "test", stops16E@data)

dbSendQuery(con, paste("CREATE TEMP TABLE stops_tmp" ,  
                       "(stop_id text PRIMARY KEY,", 
                       "stop_nam text, geom geometry)"))

postgis_insert(con, stops_spdf, "stops_tmp",
               write_cols = c("stop_id", "stop_nam"),
               geom_name = "geom")

# ---------------
library(RPostgreSQL)
library(postGIStools)

# set up connection to the database I want
drv <- dbDriver("PostgreSQL")

connection <- dbConnect( drv = drv,
                         dbname = "wmata_routes",
                         host = "localhost",
                         port = 5432 )

# create dummyRoute table in schema, routesIneed

schema_query <- "CREATE SCHEMA test_routes;"

dbSendQuery(connection,schema_query)

# where we make the df we want
dummyRoute <- readOGR("/Users/giscomputerextra2/Desktop/max/github/maxgrossman/dclatebus/data",
                      "16E_stops")


# write the entire table
dbWriteTable(connection, 
             c( "test_routes", "dummyroute" ),
             dummyRoute_df)

# take geometry in that table and write it correctly as geometry

geom_query <- "ALTER TABLE test_routes.dummyroute ADD COLUMN stop_geom geometry(POINT,4326);"

dbSendQuery(connection,geom_query)

setgeom_query <- "UPDATE test_routes.dummyroute SET stop_geom=ST_SetSRID(ST_MakePoint(x, y), 4326);"

dbSendQuery(connection, setgeom_query)

postgis_insert(con, dummyRoute, "city",
               geom_name = "geom", hstore_name = "datalist")
                         
