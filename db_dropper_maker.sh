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

Rscript R/Day_GeoJSON_maker.R

# get time stamp and use it for unique filename

current_time=$(date "+%Y.%m.%d")

# create variable names for the two directories 

dir0=dir0routes

dir1=dir1routes

# combine directories and current time for filename

dir0filename=$dir0.$current_time.geojson

dir1filename=$dir1.$current_time.geojson

# merge both directions

geojson-merge data/dir0routes/*.geojson > data/dir0routes/daily/$dir0filename

geojson-merge data/dir1routes/*.geojson > data/dir1routes/daily/$dir1filename

# remove the routes

rm -f data/dir0routes/*.geojson

rm -f data/dir1routes/*.geojson

dropdb wmata_routes_temp



