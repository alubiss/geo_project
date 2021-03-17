
# kriging

library("readxl")
library(dplyr)
library(tidyverse)
library(sf)
library(osmdata)    
#devtools::install_github('osmdatar/osmdata')
library(ggmap)
library(leaflet) 
library(rgdal)
library(GISTools)
library(sp)
library(spdep)
library(spatialreg)
library(corrplot)
library(caret)
library(dismo)
library(fields)
library(gridExtra)
#library(gstat)
library(pgirmess)
library(raster)
library(rasterVis)
library(rgeos)
library(shapefiles)
library(geoR)
library(maptools)
library(spdep)

dane <- read_excel("/Users/alubis/geo_project/dane_semi2.xlsx")
dane$x= as.numeric(dane$x)
dane$y= as.numeric(dane$y)
dane$cena = as.numeric(gsub(",", ".", dane$cena, fixed=TRUE))
dane$N = dane$`wielkosc ruchu`
dane$`liczba gwiazdek` = as.numeric(gsub(",", ".", dane$`liczba gwiazdek`, fixed=TRUE))
dane$`średni spędzany czas (min)`=as.numeric(dane$`średni spędzany czas (min)`)

# dane punktowe
coordinates(firmy) <- ~coords.x1+coords.x2
bubble(firmy, "zatr", maxsize=2.5, main ="Lokalizacja punktów", key.entries=5*c(1,6,30,120,300), alpha=1/2)