
for(k in 1:(length(bus_route.points@coords[,1])-1)) {
  
  a = bus_route.points@coords[k,]
  
  b = bus_route.points@coords[k+1,]
  
  mid = cbind((a[1]+b[1])/2,(a[2]+b[2])/2)
  
  amid = cbind((a[1]+mid[1])/2,(a[2]+mid[2])/2)
  
  midb = cbind((mid[1]+b[1])/2,(mid[2]+b[2])/2)

  better_seg_points = rbind(a=a,amid=amid,mid=mid,midb=midb,b=b)
  
  row.names(better_seg_points) <- seq(length(better_seg_points[,1]))
  
  if (k == 1) {
    
    better_seg_points_fin = 0
    
    better_seg_points_fin = data.frame(rbind(better_seg_points_fin,better_seg_points))
    
    better_seg_points_fin = better_seg_points_fin[better_seg_points_fin[,1] > 0,]
    
  } else {
    
    if ( )
    
    better_seg_points_fin = SpatialPoints(rbind(better_seg_points_fin,better_seg_points),
                                          CRS("+proj=lcc +lat_1=38.3 +lat_2=39.45 +lat_0=37.66666666666666 +lon_0=-77 +x_0=399999.9999999999 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"))
  
  }
}
