#!/bin/zsh
# 1) copy today's database to temp database. 
# 2) delete original database
# 3) use source db to make next day's database. 
# 4) use the temp database to make geojson of the day with R script. 
# 5) delete the temp database 

# 1)
createdb -O maxgrossman -T wmata_routes wmata_routes_temp
# 2)
dropdb wmata_routes
# 3)
createdb -O maxgrossman -T wmata_routes_orig wmata_routes
# 4)
usr/local/bin/Rscript db_to_geojson.R
# 5)
dropdb wmata_routes_temp



