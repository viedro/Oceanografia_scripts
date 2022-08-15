#################################
###Librería para manejo de datos argoFloats
library(argoFloats)
#Libreria para ploteo de mapas oceanográficos
library(oce)
#se escoge el flotador, codigo de 7 digitos
ID0 <- '6902907'
#descaga de datos del flotador
if (!exists("index0")){
  index0 <- subset(getIndex(), ID=ID0)}

#seleccion de longitudes
lon <- index0[["longitude"]]
#seleccion de latitudes
lat <- index0[["latitude"]]
#total de ciclos
cycle <- index0[['cycle']]
#Fechas
t <- index0[["date"]]


#par(mar=c(2,2,1,1))
#colores de acuedo al tiempo
cm <- colormap(t, col=oceColorsJet)

#Se crea la paleta de colores, las fechas etsraan en formato año-mes
drawPalette(colormap=cm, tformat="%Y-%m", pos=3)
#Graficado con batimetria
plot(index0, bathymetry=TRUE)
#Puntos del flotador

points(lon, lat, pch=21, cex=1, bg=cm$zcol, col="black", lwd=1.2)
#Lineas de union de puntos
lines(lon, lat)

sub <- seq(1, length(lon), by=10)
#Numero de ciclo cada 5 puntos
text(lon[sub], lat[sub], cycle[sub], cex=2/3, pos=1)
#Codigo de flotador en la grafica
mtext(paste("Float", ID0), adj=0)

#Titulo
title("Recorrido ArgoFloat  (2018-2022) 6902907",xlab = "Longitud (º)", ylab = "Latitud (º)")

#subtitulo
title(sub="Fuente: Elaboración propia con datos Argo Floats",adj=0.9,cex.sub=0.8)
