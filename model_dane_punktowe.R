

dane <- read_excel("/Users/alubis/Desktop/dane_semi2.xlsx")
dane$x= as.numeric(dane$x)
dane$y= as.numeric(dane$y)
dane$cena = as.numeric(gsub(",", ".", dane$cena, fixed=TRUE))
dane$N = dane$`wielkosc ruchu`
dane$`liczba gwiazdek` = as.numeric(gsub(",", ".", dane$`liczba gwiazdek`, fixed=TRUE))
dane$`średni spędzany czas (min)`=as.numeric(dane$`średni spędzany czas (min)`)


dane = pop.df.warsaw %>% dplyr ::  filter(ruch != 0)
coords <- SpatialPoints(dane[,1:2], proj4string=CRS("+proj=longlat +datum=NAD83"))
cont.listw <- nb2listw(knn2nb(knearneigh(coords, k = 6, longlat = TRUE)), style = "W")
formula <- formula(ruch ~ cena + TOT+ stars + czas_avg + min_odl_trunk + min_odl_primary
                   + od_stacji_min + od_centrum)
SDM <- lagsarlm(formula, data=dane, listw=cont.listw, type="mixed", tol.solve=1.0e-20, method="LU")
SAR<-lagsarlm(formula, data=dane, cont.listw, method='LU', quiet = TRUE, tol.solve=1.0e-20)
SEM <- errorsarlm(formula, data=dane, cont.listw, method='LU', quiet = TRUE, tol.solve=1.0e-20)
SAC <- sacsarlm(formula, data=dane, listw=cont.listw, method="LU", tol.solve=1.0e-20)
SDEM <- errorsarlm(formula, data=dane, listw=cont.listw, zero.policy=TRUE, method="LU")

model.SDM.p<-predict.sarlm(SDM)
model.SDEM.p<-predict.sarlm(SDEM)
#model.SAC.p<-predict.sarlm(SAC)
model.SEM.p<-predict.sarlm(SEM)
model.SAR.p<-predict.sarlm(SAR)

library(Metrics)
vec<-c("model.SDM.p", "model.SDEM.p", "model.SEM.p", "model.SAR.p") 
n=length(vec)
metrics<-matrix(0, nrow=n, ncol=5)
rownames(metrics)<-vec
colnames(metrics)<-c("bias", "bias%", "MAE","MAPE", "RMSE")
for(i in 1:n){
  metrics[i,1]<-bias(dane$ruch, get(vec[i])) 
  metrics[i,2]<-percent_bias(dane$ruch, get(vec[i])) 
  metrics[i,3]<-mae(dane$ruch, get(vec[i]))
  metrics[i,4]<-mape(dane$ruch, get(vec[i]))
  metrics[i,5]<-rmse(dane$ruch, get(vec[i]))}
metrics

mean(dane$ruch)
