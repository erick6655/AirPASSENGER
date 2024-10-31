#1
rm(list = ls())
install.packages("readxl")
install.packages("TTR")

#2
library(readxl)
library(TTR)
library(ggplot2)  
library(urca)
library(forecast)
library(TSstudio)
library(MASS)

#3
AirPassengers 
pasajerosAviones <- AirPassengers
ts.plot(pasajerosAviones)

#4 VA FOTO
autoplot(pasajerosAviones, main="Número de Pasajeros Aéreos (1949-1960)", ylab="Número de Pasajeros", xlab="Año")
pasajerosAviones_l1 <- lag(pasajerosAviones, -1)

#5

D <- pasajerosAviones - pasajerosAviones_l1 #resta de series
C <- pasajerosAviones/pasajerosAviones_l1 # division de series 

CVD <- sd(D)/mean(D)
CVC <- sd(C)/mean(C)
print(ifelse(CVC > CVD, "CVC > CVD --> Aplica Modelo Aditivo", "CVC < CVD --> Aplica Modelo Multiplicativo"))


#6 ##### ENTONCES APLICAMOS MULTIPLICATIVO
#va foto

ts_decompose <- decompose(pasajerosAviones, type="multiplicative")
plot(ts_decompose)
autoplot(pasajerosAviones, series = "Serie Original") + 
  ggtitle("Número de Pasajeros Aéreos (1949-1960)") + 
  labs(x = "Tiempo", y = "Número de Pasajeros")

#7Desestacionalizamos la serie original # VA FOTO
serie_desestacionalizada <- pasajerosAviones/ts_decompose$seasonal
plot(serie_desestacionalizada)


#8
#Si deseo puedo juntar la serie original con la serie desestacionalizada
tsdata <- cbind(pasajerosAviones, serie_desestacionalizada)
plot(tsdata)

#9 QUEREMOS VERLOS JUNTOSUNO ENCIMA DEL OTRO 
autoplot(pasajerosAviones, series = "Serie Original") + 
  autolayer(serie_desestacionalizada, series = "Serie desestacionalizada") + 
  ggtitle("Pasajeros Aéreos") + 
  labs(x = "Tiempo", y = "Número de Pasajeros")

#10 VER SERIE DESESTACIONALIZADA VA FOTO
acf(serie_desestacionalizada, lag.max = 50, plot = T, ylim=range(-1,1))

