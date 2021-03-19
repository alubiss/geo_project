
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
library(rgdal)
library(sp)
library(spatstat)

dane <- read_excel("/Users/alubis/geo_project/dane_semi2.xlsx")
dane$x= as.numeric(dane$x)
dane$y= as.numeric(dane$y)
dane$cena = as.numeric(gsub(",", ".", dane$cena, fixed=TRUE))
dane$N = dane$`wielkosc ruchu`
dane$`liczba gwiazdek` = as.numeric(gsub(",", ".", dane$`liczba gwiazdek`, fixed=TRUE))
dane$`średni spędzany czas (min)`=as.numeric(dane$`średni spędzany czas (min)`)

# dane punktowe
dane.sp <- dane
coordinates(dane.sp) <- c("x","y") 
#proj4string(dane.sp) <- CRS("+proj=longlat +datum=NAD83")
#dane.sp <- spTransform(dane.sp, CRS("+proj=longlat +datum=NAD83"))

bubble(dane.sp, "N", maxsize=2.5, main ="Lokalizacja punktów", key.entries=1000*c(2,4,5,6), alpha=1/2)

# utworzenie zmiennej losowej z rozkładu jednostajnego [0,1] 
dane.sp$r<-runif(dim(dane.sp)[1])
# nowe obiekty są klasy SpatialPointsDataFrame:
input <- dane.sp[dane.sp$r<0.8, ] # wybranie 80% danych na zbiór treningowy 
output <- dane.sp[dane.sp$r>0.8, ] # wybranie 20% danych na zbiór testowy

pow<-readOGR("/Users/alubis/Desktop/OneDrive/mag/Powiaty", "Powiaty")
#pow<-spTransform(pow, CRS("+proj=longlat +datum=NAD83"))
#pow2 =pow[pow@data[["JPT_NAZWA_"]]=="powiat Warszawa",]
pow.df<-as.data.frame(pow) 
pow2<-pow[pow.df$jpt_nazwa_=="powiat Warszawa",]
pow2<-spTransform(pow2, CRS("+proj=longlat +datum=WGS84"))
W<-as(pow2, "owin")

# wykres krigingu
plot(W, main="In-sample and out-of-sample data") 
points(input)
points(output, col="red", add=TRUE) 
legend("left", legend=c("input data", "output data"),col=c("black", "red"), pch=16)

