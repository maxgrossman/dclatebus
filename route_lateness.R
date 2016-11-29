##########################################################################################
# Programmer: Maximus
# Purpose: Create time stamped dataframe for bus lateness 
# Method: 
# a: connect with database, created in stops_spatializer.R
# b: call api to get deviation from schedule for each route
# c: add results to df, add to named talbe in deviation schema. 

##########################################################################################

### Conditionally install/load needed packages ###

# list of packages used in script
packages <- c( "jsonlite", "RPostgreSQL" )

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

### a: connect with database ###

# get name of database

database_name <- readline( prompt = "Provide name of database to connect to: ")

# set up driver and connect to database

drv <- dbDriver("PostgreSQL")

connection <- dbConnect( drv = drv,
                         dbname = database_name,
                         host = "localhost",
                         port = 5432 )

# create schemas for deviations, dir0 and dir1

dbSendQuery( connection, "CREATE SCHEMA dir0deviations;")

dbSendQuery( connection, "CREATE SCHEMA dir1deviations;")

##########################################################################################

### b: call api for deviations ###

# get api key

key <- readline( prompt = "Provide API Key: ")

# get deviations

wmata_deviations <- fromJSON( txt = system( paste(
  'curl -v -X GET "https://api.wmata.com/Bus.svc/json/jBusPositions" -H "api_key: ',
  key,' "', sep = "" ) , intern=TRUE ))

##########################################################################################

### add results to df, add to named talbe in deviation schema ###

# separate into dfs for each of the two direction

wmata_deviations_dir0 <- wmata_deviations$BusPositions[
  wmata_deviations$BusPositions$DirectionNum %in% 0,] 

wmata_deviations_dir1 <- wmata_deviations$BusPositions[
  wmata_deviations$BusPositions$DirectionNum %in% 1,] 

# get list of routes in the 2 direction schemas from the database and use to subset

# hardcoded, I know. 

dir0tab_query <- "SELECT table_name FROM information_schema.tables 
WHERE table_schema = 'dir0routes';"

dir0tabs <- dbGetQuery( connection,dir0tab_query )$table_name

dir1tab_query <- "SELECT table_name FROM information_schema.tables 
WHERE table_schema = 'dir1routes';"

dir1tabs <- dbGetQuery( connection,dir1tab_query )$table_name

# now subset

wmata_deviations_dir0 <-wmata_deviations_dir0[as.character(wmata_deviations_dir0$RouteID) %in% dir0tabs,]

