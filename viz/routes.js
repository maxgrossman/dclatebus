//method to map lateness
var mapLateness = function(selectedRoute) {

            if(Clicks > 0) {

                routeGeoJson.clearLayers();

            }

            if($(".leaflet-marker-pane img").length == 1) {

                sliderControl.getContainer().innerHTML = ''

                sliderControl._layer = ''

                $('.leaflet-marker-pane').empty()

            } 

            else if(selectedRoute != $( "#mapRoute" ).val()) {

                routeGeoJson.clearLayers()

                latenessInfo.getContainer().innerHTML = ''

            }     

            $.getJSON(selectedRoute, function(data) {

                selectedRoute = data;

                // on load, create geojson with style and interactivity functionality                          
                routeGeoJson = L.geoJson(selectedRoute, {

                    style: function(feature) {

                        return {             

                            "color": feature.properties.dev_col,                            
                            "lineCap": "butt",
                            "opacity": 0.7,
                            "weight": 3          

                        };

                    },

                    //set events that occur when interacting with each routeGeoJson segment
                    onEachFeature: function(feature, layer) {

                        // on segment mouseover, increase size & opacity
                        layer.on('mouseover', function() {

                            this.setStyle({

                                opacity:1,
                                "weight": 4

                            });

                            // handle non-chrome/firefox
                            if (L.Browser.ie && !L.Broswer.opera && !L.Browser.edge) {

                                this.bringToFront();

                            }

                            // send mouseovered segment properties to latenessInfo control
                            latenessInfo.update(this.feature.properties);

                        });

                        // on segment click, zoom to segment bounds, add button to zoom to route extent
                        layer.on('click', function() {

                            if(map.resetRouteView) {}

                            else {

                                map.fitBounds(this.getBounds());
                                map.addControl(new resetRouteView());

                            }
                        });

                        // when mouseout, or hover end, reset segment to original style, reset latenessInfo
                        layer.on('mouseout', function() {

                            routeGeoJson.resetStyle(this);
                            latenessInfo.update();

                        });

                        return routeGeoJson                

                    }}).addTo(map);

                //fit map extent to route extent
                map.fitBounds(routeGeoJson.getBounds())

                //button that allows change from segment extent to route extent 
                var resetRouteView = L.Control.extend({

                        options: {

                            position: 'topleft'

                        },

                        onAdd: function(map) {

                            // when added, create variable holding routeViewdiv and the control. also set innerHTML
                            var routeViewDiv = L.DomUtil.create('div', 'routeView');
                            map.resetRouteView = this;
                            routeViewDiv.innerHTML = "<img src='dc.png'/><p>Click to reset scale</p>"

                            // when routeViewDiv clicked, reset bounds to routeGeoJson's, remove control from map and its corresponding variable
                            $(routeViewDiv).on('click', function () {

                                map.fitBounds(routeGeoJson.getBounds());
                                map.removeControl(this);
                                delete map.resetRouteView;

                            });

                            return routeViewDiv

                        }

                    });

                //take the mouseovered segment and fit it into the latenessInfo div accordingly
                latenessInfo.update = function(props) {

                    // push relevent stats from mouseovered segment to dev_stats list
                    dev_stats = []
                    if(props) {

                        seg_stops = props.seg_stops;
                        dev_stats.push(parseInt(props.deviations_median));
                        dev_stats.push(parseInt(props.deviations_max));
                        dev_stats.push(parseInt(props.deviations_min));

                    }

                    //Since a positive deviation means a bus is late, create strings latness, ontimeness, and earliness
                    $.each(dev_stats, function(i, item) {

                    //Show bus as ahead of schedule and use proper grammar, because it matters.
                    if(item < 0) {

                        if(item/-1 == 1) {

                            dev_stats[i] = (item * -1).toString() + " minute <span class='ahead'>ahead</span> of schedule"

                        } 


                        else { 

                            dev_stats[i] = (item * -1).toString() + " minutes <span class='ahead'><b>ahead</b></span> of schedule"

                        }
                    }

                    //Say it how it is. on time.
                    else if(item == 0) {

                        dev_stats[i] = 'on time'

                    } 

                    //Show bus as behind schedule, still remembering grammar.
                    else {

                        if(item/1 == 1) {

                            dev_stats[i] = item.toString() + " minute <span class='behind'><b>behind</b></span> of schedule"

                        }

                        else { 

                                    dev_stats[i]= item.toString() + " minutes <span class='behind'><b>behind</b></span> schedule"

                        }

                    }

                    });

                    //Add strings to contents
                    var contents = '<h4><b>Bus Lateness</b></h4>' + (props ? '<p><b>Segment : </b> ' + seg_stops + '</p>' + '<p><b>Median Lateness :</b> ' + dev_stats[0] + ' </p>' + '<p><b>Max Lateness : </b>' + dev_stats[1] + ' </p>' + '<p><b>Min Lateness : </b>' + dev_stats[2] + ' </p>' : '<p>Hover over route segment</p>');

                    //NaN in contents means segment's deviation values were null. nulls mean no data was selected from that segment. Reflect that in contents.
                    if(contents.includes("NaN")) {

                        contents = '<h4><b>Bus Lateness</b></h4>' + '<p><b>Segment : </b> ' + props.seg_stops + '</p>' + '<p><i>Data was not collected on this segment</i></p>' + '';

                    }

                    // make contents the innerHTML
                    this._div.innerHTML = contents

                }

                //add latenessInfo to the map
                latenessInfo.addTo(map);

                //return routeGeoJson to be used elsewhere.
                return selectedRoute   

            });
    
    return selectedRoute

}
//method that gets busses from routeGeoJson
var getBusses = function(selectedRoute) {

            $.getJSON(selectedRoute, function(data) {

                selectedRoute = data;

                //object that will hold midpoint of each route segment, the route bus's number and the times of day each bus was at a given segment
                var routeBusPnts = {};

                //add seg_id, coordinates, and properties to routeBusPnts for those segments with busses
                $.each(selectedRoute.features, function(key,val) {

                    $.each(val, function(i,j) { 

                        //rule out segments that did not include busses becuase null deviation indicates no busses were selected 
                        if(val.properties.deviations_min === "null") {} 

                        //add what we want to routeBusPnts for that segment
                        else { 

                            routeBusPnts[val.properties.seg_id] = [val.geometry.coordinates,val.properties] 

                        }
                    })

                })	

                //change coordinates to midpoint of segment. eventually this should be changed to the true location of the bus (we need to chnage some R code.)
                $.each(routeBusPnts, function(i,item){

                    // if # points even, generate a lat,lng between middle two lat, if odd, select the middle lat/lnt
                    if(routeBusPnts[i][0].length % 2 == 0) { 

                        routeBusPnts[i][0] = [(item[0][item[0].length/2][0] + item[0][item[0].length/2 + 1][0])/2,		
                                              (item[0][item[0].length/2][1] + item[0][item[0].length/2 + 1][1])/2]
                    } 

                    else {

                        routeBusPnts[i][0] = [item[0][(item[0].length + 1)/2][0], 
                                             item[0][(item[0].length + 1)/2][1]]
                    }

                    //remove those key,val pairs where key has "dev" and do the same where key has "seg". we done need these.
                    for (key in item[1]) {

                        if(key.match("dev")) {delete item[1][key]} 
                        if(key.match("seg")) {delete item[1][key]} 

                    }

                });

                //list holding each timestamp for which busses were queried in 'route_lateness.R'. these times are held in keys for properties in routeGeoJson
                busTimes = [];

                //add those query times to busTimes
                $.each(routeBusPnts, function(i,item) {

                for(key in item[1]) {

                    // times are saved like: "querytime_someotherstuff", so I grab time stamp, not the other stuff
                    queryTime = key.split("_")[0]
                    if(key.match(queryTime)) {

                        busTimes.push(queryTime)

                    }

                }});

                //extract the unique querytimes since there are duplicates.s                 
                busTimes = $.unique(busTimes);

                //lst of l.markers representing each bus
                busMarkers = [];   

                //icon for the bus. thanks for maki mapbox
                var myIcon = L.icon({

                    iconUrl: 'bus_icon_blk.png',
                    iconSize: [20, 20],
                    popupAnchor: [-3, -76]

                });

                //for each segment in route, find segments with queryTimes that match busTimes[i] and extract the information
                $.each(routeBusPnts, function(i,item) {

                    for(var i = 0; i < busTimes.length; i++) {

                        for(key in item[1]) {

                            if(key.match(busTimes[i])) {

                                //time here is note the timestamp of my api query, but the timestamp attribute of the bus (see DateTime here --> tinyurl.com/zl65cqv)
                                if(key.match("time")) {busTime=item[1][key]} 
                                //Bus's id
                                if(key.match("bus"))  {busId=item[1][key]}

                            }
                        }          
                    }

                    //add marker, using each feature in routeBusPnt's coordinates, myIcon, the time, and title.
                    busMarkers.push(L.marker([item[0][1],item[0][0]], {icon: myIcon , opacity: 1, time: busTime, title:busId, riseOnHover:true}))        

                });

                //create busIds, with bus ids, and fill bussesDropdown with those ids
                var busIds = [];    

                $.each(busMarkers, function(i, item) {

                    busIds.push(item.options.title)

                });

                //get rid of duplicates
                busIds = $.unique(busIds)

                $.each(busIds, function(i, item) {

                    $("#bussesDropdown").append($("<option></option>")
                                        .val(item)
                                        .html(item));

                });

                return busMarkers

            })                         


}
//method that maps busses    
var mapBusses =function(selectedRoute, busMarkers) {

            if(Clicks > 1) {

                routeGeoJson.clearLayers();

            }

            //when previously mapped was bus locations, empty controls, get rid of markers, and add currently seleced markers
            if($(".leaflet-marker-pane img").length == 1) {

                sliderControl.getContainer().innerHTML = ''

                sliderControl._layer = ''

                $('.leaflet-marker-pane').empty()

            }

            else if ($(".leaflet-marker-pane img").length == 0) {

                latenessInfo.getContainer().innerHTML = ''

            }

            //find those markers in busMarkers that have same BusId as that in bussesDropdown and add the markerToMap
            var markersToMap = [];

            $.each(busMarkers, function(i, item) {

                if(item.options.title == $("#bussesDropdown").val() ) {

                    markersToMap.push(item)

                }

            });

            //sort markers in markersToMap by timestamp.
            markersToMap = markersToMap.sort(function(a,b) {

                return new Date(a.options.time).getTime() - new Date(b.options.time).getTime()

            });

            //add markers to map to sliderControl, add sliderControl to map, and start the slider
            sliderControl._layer = L.layerGroup(markersToMap);

            sliderControl.addTo(map);

            sliderControl.startSlider();

            //add route to map, make each segemnt white, and fit extent to bounds of route 
            routeGeoJson = L.geoJson(routeGeoJson, {

                style: {

                    "color": "rgba(200,200,200,1)",                            
                    "lineCap": "round",
                    "weight": 2                        
                }
            })

            routeGeoJson.addTo(map);

            map.fitBounds(routeGeoJson.getBounds())

            return routeGeoJson

} 
        

busRoute.prototype = {
    //method that maps lateness
    mapLateness: function(selectedRoute,routeGeoJson) {
        
        
        if(Clicks > 0) {
            
            routeGeoJson.clearLayers();

        }
        
        if($(".leaflet-marker-pane img").length == 1) {
                            
            sliderControl.getContainer().innerHTML = ''
                        
            sliderControl._layer = ''

            $('.leaflet-marker-pane').empty()

        } 
        
        else if(selectedRoute != $( "#mapRoute" ).val()) {

            routeGeoJson.clearLayers()
    
            latenessInfo.getContainer().innerHTML = ''
        
        }     
        
        $.getJSON(selectedRoute, function(data) {
            
            routeGeoJson = data;

            // on load, create geojson with style and interactivity functionality                          
            routeGeoJson = L.geoJson(routeGeoJson, {

                style: function(feature) {

                    return {             
                                        
                        "color": feature.properties.dev_col,                            
                        "lineCap": "butt",
                        "opacity": 0.7,
                        "weight": 3          
                    
                    };
                                    
                },
                                
                //set events that occur when interacting with each routeGeoJson segment
                onEachFeature: function(feature, layer) {

                    // on segment mouseover, increase size & opacity
                    layer.on('mouseover', function() {

                        this.setStyle({

                            opacity:1,
                            "weight": 4

                        });

                        // handle non-chrome/firefox
                        if (L.Browser.ie && !L.Broswer.opera && !L.Browser.edge) {

                            this.bringToFront();

                        }

                        // send mouseovered segment properties to latenessInfo control
                        latenessInfo.update(this.feature.properties);

});

                    // on segment click, zoom to segment bounds, add button to zoom to route extent
                    layer.on('click', function() {

                        if(map.resetRouteView) {}

                        else {

                            map.fitBounds(this.getBounds());
                            map.addControl(new resetRouteView());

                        }
                    });

                    // when mouseout, or hover end, reset segment to original style, reset latenessInfo
                    layer.on('mouseout', function() {

                        routeGeoJson.resetStyle(this);
                        latenessInfo.update();

});

                                    

                }}).addTo(map);
                            
            //fit map extent to route extent
            map.fitBounds(routeGeoJson.getBounds())
                                                    
            //button that allows change from segment extent to route extent 
            var resetRouteView = L.Control.extend({
                                    
                    options: {

                        position: 'topleft'
        
                    },
                                    
                    onAdd: function(map) {
                                        
                        // when added, create variable holding routeViewdiv and the control. also set innerHTML
                        var routeViewDiv = L.DomUtil.create('div', 'routeView');
                        map.resetRouteView = this;
                        routeViewDiv.innerHTML = "<img src='dc.png'/><p>Click to reset scale</p>"
                                        
                        // when routeViewDiv clicked, reset bounds to routeGeoJson's, remove control from map and its corresponding variable
                        $(routeViewDiv).on('click', function () {
                                        
                            map.fitBounds(routeGeoJson.getBounds());
                            map.removeControl(this);
                            delete map.resetRouteView;

                        });
                                
                        return routeViewDiv

                    }
                    
                });
                            
            //take the mouseovered segment and fit it into the latenessInfo div accordingly
            latenessInfo.update = function(props) {
                                    
                // push relevent stats from mouseovered segment to dev_stats list
                dev_stats = []
                if(props) {

                    seg_stops = props.seg_stops;
                    dev_stats.push(parseInt(props.deviations_median));
                    dev_stats.push(parseInt(props.deviations_max));
                    dev_stats.push(parseInt(props.deviations_min));

                }

                //Since a positive deviation means a bus is late, create strings latness, ontimeness, and earliness
                $.each(dev_stats, function(i, item) {

                //Show bus as ahead of schedule and use proper grammar, because it matters.
                if(item < 0) {

                    if(item/-1 == 1) {

                        dev_stats[i] = (item * -1).toString() + " minute <span class='ahead'>ahead</span> of schedule"

                    } 


                    else { 

                        dev_stats[i] = (item * -1).toString() + " minutes <span class='ahead'><b>ahead</b></span> of schedule"

                    }
                }

                //Say it how it is. on time.
                else if(item == 0) {

                    dev_stats[i] = 'on time'

                } 

                //Show bus as behind schedule, still remembering grammar.
                else {

                    if(item/1 == 1) {

                        dev_stats[i] = item.toString() + " minute <span class='behind'><b>behind</b></span> of schedule"

                    }

                    else { 

                                dev_stats[i]= item.toString() + " minutes <span class='behind'><b>behind</b></span> schedule"

                    }

                }

                });

                //Add strings to contents
                var contents = '<h4><b>Bus Lateness</b></h4>' + (props ? '<p><b>Segment : </b> ' + seg_stops + '</p>' + '<p><b>Median Lateness :</b> ' + dev_stats[0] + ' </p>' + '<p><b>Max Lateness : </b>' + dev_stats[1] + ' </p>' + '<p><b>Min Lateness : </b>' + dev_stats[2] + ' </p>' : '<p>Hover over route segment</p>');

                //NaN in contents means segment's deviation values were null. nulls mean no data was selected from that segment. Reflect that in contents.
                if(contents.includes("NaN")) {

                    contents = '<h4><b>Bus Lateness</b></h4>' + '<p><b>Segment : </b> ' + props.seg_stops + '</p>' + '<p><i>Data was not collected on this segment</i></p>' + '';

                }

                // make contents the innerHTML
                this._div.innerHTML = contents

            }
                                
            //add latenessInfo to the map
            latenessInfo.addTo(map);
                            
            //return routeGeoJson to be used elsewhere.
            return routeGeoJson   

        });
        
        return routeGeoJson;
    },
    //method that gets busses from routeGeoJson
    getBusses: function(routeGeoJson) {
                
        $.getJSON(routeGeoJson, function(data) {
                        
            routeGeoJson = data;
                        
            //object that will hold midpoint of each route segment, the route bus's number and the times of day each bus was at a given segment
            var routeBusPnts = {};
                   
            //add seg_id, coordinates, and properties to routeBusPnts for those segments with busses
            $.each(bussesGeoJson.features, function(key,val) {

                $.each(val, function(i,j) { 

                    //rule out segments that did not include busses becuase null deviation indicates no busses were selected 
                    if(val.properties.deviations_min === "null") {} 

                    //add what we want to routeBusPnts for that segment
                    else { 

                        routeBusPnts[val.properties.seg_id] = [val.geometry.coordinates,val.properties] 

                    }
                })
                                
            })	
                            
            //change coordinates to midpoint of segment. eventually this should be changed to the true location of the bus (we need to chnage some R code.)
            $.each(routeBusPnts, function(i,item){

                // if # points even, generate a lat,lng between middle two lat, if odd, select the middle lat/lnt
                if(routeBusPnts[i][0].length % 2 == 0) { 
                                    
                    routeBusPnts[i][0] = [(item[0][item[0].length/2][0] + item[0][item[0].length/2 + 1][0])/2,		
                                          (item[0][item[0].length/2][1] + item[0][item[0].length/2 + 1][1])/2]
                } 
                                
                else {
                                    
                    routeBusPnts[i][0] = [item[0][(item[0].length + 1)/2][0], 
                                         item[0][(item[0].length + 1)/2][1]]
                }
                     
                //remove those key,val pairs where key has "dev" and do the same where key has "seg". we done need these.
                for (key in item[1]) {
                                    
                    if(key.match("dev")) {delete item[1][key]} 
                    if(key.match("seg")) {delete item[1][key]} 
                                    
                }
                                
            });
  
            //list holding each timestamp for which busses were queried in 'route_lateness.R'. these times are held in keys for properties in routeGeoJson
            busTimes = [];
        
            //add those query times to busTimes
            $.each(routeBusPnts, function(i,item) {
                                
            for(key in item[1]) {
                                    
                // times are saved like: "querytime_someotherstuff", so I grab time stamp, not the other stuff
                queryTime = key.split("_")[0]
                if(key.match(queryTime)) {

                    busTimes.push(queryTime)

                }

            }});
                           
            //extract the unique querytimes since there are duplicates.s                 
            busTimes = $.unique(busTimes);
                            
            //lst of l.markers representing each bus
            busMarkers = [];   
                        
            //icon for the bus. thanks for maki mapbox
            var myIcon = L.icon({
                
                iconUrl: 'bus_icon_blk.png',
                iconSize: [20, 20],
                popupAnchor: [-3, -76]

            });
                            
            //for each segment in route, find segments with queryTimes that match busTimes[i] and extract the information
            $.each(routeBusPnts, function(i,item) {
                                
                for(var i = 0; i < busTimes.length; i++) {
                                        
                    for(key in item[1]) {
                                            
                        if(key.match(busTimes[i])) {
                                            
                            //time here is note the timestamp of my api query, but the timestamp attribute of the bus (see DateTime here --> tinyurl.com/zl65cqv)
                            if(key.match("time")) {busTime=item[1][key]} 
                            //Bus's id
                            if(key.match("bus"))  {busId=item[1][key]}
                                                
                        }
                    }          
                }
                                
                //add marker, using each feature in routeBusPnt's coordinates, myIcon, the time, and title.
                busMarkers.push(L.marker([item[0][1],item[0][0]], {icon: myIcon , opacity: 1, time: busTime, title:busId, riseOnHover:true}))        

            });
                                       
            //create busIds, with bus ids, and fill bussesDropdown with those ids
            var busIds = [];    
                       
            $.each(busMarkers, function(i, item) {
                                
                busIds.push(item.options.title)
                            
            });
                            
            //get rid of duplicates
            busIds = $.unique(busIds)
            
            $.each(busIds, function(i, item) {
                        
                $("#bussesDropdown").append($("<option></option>")
                                    .val(item)
                                    .html(item));
                                
            });
            
            return routeGeoJson
            return busMarkers
                       
        })                         
                    
        
    },
    //method that maps busses    
    mapBusses: function(routeGeoJson, busMarkers) {
        
        if(Clicks > 1) {
            
            routeGeoJson.clearLayers();

        }
        
        //when previously mapped was bus locations, empty controls, get rid of markers, and add currently seleced markers
        if($(".leaflet-marker-pane img").length == 1) {
                            
            sliderControl.getContainer().innerHTML = ''
                        
            sliderControl._layer = ''

            $('.leaflet-marker-pane').empty()

        }
        
        else if ($(".leaflet-marker-pane img").length == 0) {
                                                 
            latenessInfo.getContainer().innerHTML = ''
                            
        }
        
        //find those markers in busMarkers that have same BusId as that in bussesDropdown and add the markerToMap
        var markersToMap = [];

        $.each(busMarkers, function(i, item) {

            if(item.options.title == $("#bussesDropdown").val() ) {

                markersToMap.push(item)

            }
                                
        });
                            
        //sort markers in markersToMap by timestamp.
        markersToMap = markersToMap.sort(function(a,b) {

            return new Date(a.options.time).getTime() - new Date(b.options.time).getTime()

        });
                           
        //add markers to map to sliderControl, add sliderControl to map, and start the slider
        sliderControl._layer = L.layerGroup(markersToMap);
                        
        sliderControl.addTo(map);

        sliderControl.startSlider();
                        
        //add route to map, make each segemnt white, and fit extent to bounds of route 
        routeGeoJson = L.geoJson(routeGeoJson, {

            style: {

                "color": "rgba(200,200,200,1)",                            
                "lineCap": "round",
                "weight": 2                        
            }
        })
                                
        routeGeoJson.addTo(map);

        map.fitBounds(routeGeoJson.getBounds())
                            
        return routeGeoJson
        
    }  
    
    return this.routeGeoJson
    
}
    
    
