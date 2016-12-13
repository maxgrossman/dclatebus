require(mapview)

# Query to db, creating df
query_for_needed <- paste("SELECT St_AsText(seg_line) from ", shQuote( "dir0routes" , type = 'cmd'), ".",
                          shQuote( "10A", type = 'cmd'), " ;",sep="")

rs = dbSendQuery(connection,query_for_needed)

df = fetch(rs,n=-1)

# iterate through df, converting to line dataframe, grabbing then the lat/lng into a dataframe and add to 
# list, then use reduce and merge to make them one big dataframe.

line.dfs <- list()

for(i in 1:length(df[,1])) {
  line <- readWKT(df[i,1])
  line.pts <- as.data.frame(as(line, "SpatialPointsDataFrame"))[,4:5]
  line.dfs[[length(line.dfs)+1L]]<-line.pts
}

line.pts.df <- Reduce(function(x,y) merge(x,y, all=TRUE), line.dfs)

# quickly make map 

# make list where every 2 elements is the same random string so I can group points into 
# a line. Will have to make this a bit more complicated when trying to create lines
# with different lenght lines.

x <- list(); y <- list(); blank_list <- list()

for( i in seq(1:55) ) {
  x <- stri_rand_strings(1, length=2, pattern = "[A-Za-z0-9]")
  y <- x
  blank_list[[length(blank_list)+1L]] <- c(x,y)
}

# Make two lists where lat and lon for nodes along a line are consecutive

x_ <- list()
y_ <- list()

for (i in 1:length(df$a_x)) {
  x_a <- df$a_x[i]
  x_b <- df$b_x[i]
  x_[[length(x_)+1L]]<-c(x_a,x_b)
  y_a <- df$a_y[i]
  y_b <- df$b_y[i]
  y_[[length(y_)+1L]]<-c(y_a,y_b)
}

times_<-list()

for(i in seq(1:55)) {
  times_[[length(times_)+1L]]<-2
}
  


lines_df <- data.frame(y = unlist(y_),
                       x = unlist(x_),
                        group = unlist(blank_list),
                        col = col_,
                        stringsAsFactors = FALSE)

lst_lns <- lapply(unique(unlist(blank_list)), function(i) {
  df_sub <- subset(lines_df, group == i)
  ln <- Line(df_sub[, 2:1])
  Lines(list(ln), ID = i)
})

sln <- SpatialLines(lst_lns, proj4string = CRS("+init=epsg:4326"))

slndf <- SpatialLinesDataFrame(sln, match.ID = FALSE, 
                               data = unique(lines_df[, c("group", "col")]))

mapview(slndf, zcol = "group", color = slndf@data$col,lwd=3)

popupTable(slndf, zcol = c("group"))