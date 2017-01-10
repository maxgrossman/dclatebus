#!/bin/zsh
# write today's database to csv, delete today's database, initialize tomorrow's database

psql -d wmata_routes

\copy (SELECT * FROM "dir0routes") TO 
"/Users/maxgrossman/github/maxgrossman/dclatebus/data/dir0routes.csv" 
DELIMITER ',' CSV HEADER;