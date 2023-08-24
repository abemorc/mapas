library("leaflet")
library("data.table")
library("sp")
library("rgdal")
# library("maptools")
library("KernSmooth")
library("raster")
library(tidyverse)

inurl <- "https://data.cityofchicago.org/api/views/22s8-eq8h/rows.csv?accessType=DOWNLOAD"
infile <- "mvthefts.csv"

## LOAD DATA
## Also, clean up variable names, and convert dates
if(!file.exists(infile)){
  download.file(url = inurl, destfile = infile)
}

# ver achivo
dfdat <- data.table::fread(infile)
dfdat

view(dfdat)



dat <- data.table::fread(infile)
setnames(dat, tolower(colnames(dat)))
setnames(dat, gsub(" ", "_", colnames(dat)))
dat <- dat[!is.na(longitude)]
dat[ , date := as.IDate(date, "%m/%d/%Y")]

dat
str(dat)

## Create kernel density output
kde <- bkde2D(dat[ , list(longitude, latitude)],
              bandwidth=c(.0045, .0068), gridsize = c(1000,1000))
# Create Raster from Kernel Density output
KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

#create pal function for coloring the raster
palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)

plot(KernelDensityRaster)
## Leaflet map with raster
leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .8) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Kernel Density of Points")







#set low density cells as NA so we can make them transparent with the colorNumeric function
KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 1)] <- NA

#create pal function for coloring the raster
palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")

## Redraw the map
leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .8) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Kernel Density of Points")









# If you want a binned raster, use the colorBin function rather than the colorNumeric function:
  
palRaster <- colorBin("Spectral", bins = 7, domain = KernelDensityRaster@data@values, na.color = "transparent")

## Leaflet map with raster
leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .8) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Kernel Density of Points")



# To make it smoother,

gridsize = c(1000,1000)


