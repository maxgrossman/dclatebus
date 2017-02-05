#!/bin/zsh
# 1) copy today's database to temp database. 
# 2) delete original database
# 3) use source db to make next day's database. 
# 4) use the temp database to make geojson of the day with R script.
# 6) take route outputs from daily geojson and create single geojsons for each route dir
# 7) delete the temp database 

cd /Users/maxgrossman/github/maxgrossman/dclatebus

# create the temporary database
createdb -O maxgrossman -T wmata_routes wmata_routes_temp

# drop the database for today
dropdb wmata_routes

# recreate the wmata_route database for next day
createdb -O maxgrossman -T wmata_routes_orig wmata_routes

# make the daily geojsons 

current_date=$(date "+%Y-%m-%d")   
mkdir data/dir0routes/dailygeojson/$current_date         
mkdir data/dir0routes/dailycsv/$current_date         
mkdir data/dir1routes/dailygeojson/$current_date  
mkdir data/dir1routes/dailycsv/$current_date  
Rscript R/Day_GeoJSON_maker.R    