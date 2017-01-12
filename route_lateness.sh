#!/bin/zsh
# bash file called via daemon manager (lingon x) to call wmata api every 90 seconds
# The r script it runs grabs bus data, queries their location along bus routes
# and writes bus data, specifically bus id, deviation from schedule, and timestamp
# to database

# run route_lateness
/usr/local/bin/Rscript /Users/maxgrossman/github/maxgrossman/dclatebus/R/route_lateness.R
