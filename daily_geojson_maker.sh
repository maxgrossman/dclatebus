#!/bin/zsh
# make daily geojson by
	# running r script making a geojson for each route
	# combining those routes with geojson-merge. thanks mpx!
	# remove route files

usr/local/bin/Rscript Day_GeoJSON_maker.R

cd /Users/maxgrossman/github/maxgrossman/dclatebus

# get time stamp and use it for unique filename

current_time=$(date "+%Y.%m.%d")

dir0=dir0routes

dir1=dir1routes

dir0filename=$dir0.$current_time.geojson

dir1filename=$dir1.$current_time.geojson

geojson-merge data/dir0routes/*.geojson > data/dir0routes/daily/$dir0filename

geojson-merge data/dir1routes/*.geojson > data/dir1routes/daily/$dir1filename

