## INITIALIZE
library("leaflet")
library("data.table")
library("sp")
library("rgdal")
# library("maptools")
library("KernSmooth")
library("magrittr")
# 
# ## LOAD DATA
# ## Also, clean up variable names, and convert dates
# inurl <- "https://data.cityofchicago.org/api/views/22s8-eq8h/rows.csv?accessType=DOWNLOAD"
# dat <- data.table::fread(inurl) %>% 
#   setnames(., tolower(colnames(.))) %>% 
#   setnames(., gsub(" ", "_", colnames(.))) %>% 
#   .[!is.na(longitude)] %>% 
#   .[ , date := as.IDate(date, "%m/%d/%Y")] %>% 
#   .[]    
# ## MAKE CONTOUR LINES
## Note, bandwidth choice is based on MASS::bandwidth.nrd()
kde <- bkde2D(dfmuertos[ , list(x_long, y_lat)],
              bandwidth=c(.0045, .0068), gridsize = c(100,100))

kde <- bkde2D(dfmuertos[ , list(x_long, y_lat)],
              bandwidth=c(1, 1), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

## Leaflet map with polygons
leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])





## Leaflet map with points and polygons
## Note, this shows some problems with the KDE, in my opinion...
## For example there seems to be a hot spot at the intersection of Mayfield and
## Fillmore, but it's not getting picked up.  Maybe a smaller bw is a good idea?

leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(lng = dat$longitude, lat = dat$latitude,
             radius = .5, opacity = .2, col = "blue")







## Leaflet map with polygons, using Spatial Data Frame
## Initially I thought that the data frame structure was necessary
## This seems to give the same results, but maybe there are some 
## advantages to using the data.frame, e.g. for adding more columns
spgonsdf = SpatialPolygonsDataFrame(Sr = spgons,
                                    data = data.frame(level = LEVS),
                                    match.ID = TRUE)
leaflet() %>% addTiles() %>%
  addPolygons(data = spgonsdf,
              color = heat.colors(NLEV, NULL)[spgonsdf@data$level])
