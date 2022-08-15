##Librería para carga de datos
library(readr)
setwd("C:/Users/USUARIO/Desktop/2021-ii/Oceano")
h <-read.table("Altura_din.txt",
                 header = T,
                 sep = "",           
                 dec = ".")
h

#Convertimos a dataframe
h <- data.frame(h)
#Borramos columna innecesaria
h <- h[,-2]
h
#Cambiamos nombres de columnas
colnames(h)<- c("Fecha","Dyn")
#Establecemos columna de fecha
h[,1] <- as.Date(h[,1],format="%d/%m/%Y")
#Vemos si la columna es de tipo fecha
str(h)
#LIbrería para manipular datos
library(tidyverse)

#Completado epico de fecha Xd
h_c <- h %>% 
  complete(Fecha = seq.Date(min(Fecha),max(Fecha),by="day"))

#Convirtiendo a dataframe(mejor uso)
h_c <- data.frame(h_c)

#Cmabiando la data nula a NA
h_c[h_c ==  -9.99] <- NA

#Primera grafica
plot(h_c)

#Valor de la media-- posible linea base
mean(h_c[,2],na.rm=T)
