
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
coordinates(firmy) <- ~coords.x1+coords.x2
bubble(firmy, "zatr", maxsize=2.5, main ="Lokalizacja punktów", key.entries=5*c(1,6,30,120,300), alpha=1/2)

# utworzenie zmiennej losowej z rozkładu jednostajnego [0,1] 
firmy$r<-runif(dim(firmy)[1])
# nowe obiekty są klasy SpatialPointsDataFrame:
input <- firmy[firmy$r<0.8, ] # wybranie 80% danych na zbiór treningowy 
output <- firmy[firmy$r>0.8, ] # wybranie 20% danych na zbiór testowy

woj <-readOGR(".","wojewodztwa") # 16 jedn. 
woj.df<-as.data.frame(woj) 
region<-woj[woj.df$jpt_nazwa_=="lubelskie",] 
region<-spTransform(region, CRS("+proj=longlat +datum=WGS84")) 
W <- as(region, "owin")

# wykres krigingu
plot(W, main="In-sample and out-of-sample data") 
points(input, pch=16)
points(output, col="red", add=TRUE, pch=16) 
legend("left", legend=c("input data", "output data"),col=c("black", "red"), pch=16)

