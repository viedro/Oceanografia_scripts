library(tidyverse)  
#coleccion de paquetes
library(lubridate)
library(plotly)
library(ggthemes)
library(cowplot)
library(reshape2)
library(MBA)
library(mgcv)
library(marmap)
library(FNN)
library(metR)
library(cachem)
library(memoise)
library(ncdf4)
library(curl)
library(RCurl)
library(stringi)
library(data.table)
library(reshape2)
library(compare)
library(dplyr)
library(chron)
library(raster)
library(rvest)
library(lattice)
library(RColorBrewer)
library(stringr)
library(tidyr)

#temperatura (boya)
setwd("C:/Users/USUARIO/Desktop/2021-ii/Oceano")
library(ncdf4)
library(openxlsx)
n=nc_open("t9n140w_dy_dat.cdf")
TEMP=ncvar_get(n, varid="T_20")
depth=ncvar_get(n, varid="depth")



row.names(TEMP)=depth
time=ncvar_get(n,varid="time")
FECHA=as.Date("1988-05-31") + 1:length(time)
TEMP<- as.data.frame(t(TEMP))
TEMP=cbind(FECHA,TEMP)




#cambiar los NA  por espacios en blancos, SOLO usar si pasaremos a excel
#TEMP=TEMP%>%separate(FECHA, sep="-", into = c("AÑO", "MES", "DIA"))
#TEMP[is.na(TEMP)]= ""
#write.xlsx(TEMP,"TEMPERATURA 0°N 110°W.xlsx")
#TEMP=as_tibble(TEMP)

#glimpse(TEMP)

#columnas character to numeric
df=TEMP
sapply(df,class)
cols <- names(df)[2:14]
df[cols] <- lapply(df[cols], as.numeric)

str(df)
#pivot table
df=df %>%
  group_by(month = month(FECHA), day = day(FECHA)) %>%
  summarise_if(is.numeric, mean , na.rm=TRUE)

#df$X8=NULL

#seccion historica
df[1:2]=NULL
df <- data.frame(df)
prof=as.numeric(dim(df)[2])

HISTORICO=melt(df, id=0)

colnames(HISTORICO)=c("PROFUNDIDAD","TEMPERATURA")
HISTORICO$PROFUNDIDAD<-stri_replace_first_regex(HISTORICO$PROFUNDIDAD, "[X]", "")
HISTORICO$PROFUNDIDAD=as.numeric(HISTORICO$PROFUNDIDAD)



HISTORICO$DIA=rep(1:366,prof)
HISTORICO=na.omit(HISTORICO)
col_order <- c("DIA","PROFUNDIDAD","TEMPERATURA")
HISTORICO <- HISTORICO[, col_order]


grilla<- mba.surf(HISTORICO, no.X = 365, no.Y = 365, extend = T)
mba=grilla$xyz.est
rownames(mba$z)=mba$x
colnames(mba$z)=mba$y
PROMEDIO=reshape2::melt(mba$z, varnames = c('x', 'y'))
PROMEDIO=na.omit(PROMEDIO)

#ESCALA DE COLORES
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")

View(PROMEDIO)
#GRAFICO
ggplot(data = PROMEDIO, aes(x =x, y = y)) +
  geom_raster(aes(fill = value)) +
  geom_contour_fill(aes(z = value), binwidth = 2, colour = "black", alpha = 0.2)+ 
  geom_text_contour(aes(z = value),stroke = 0.1,skip=0,check_overlap=T)+
  geom_contour(aes(z = value), breaks = 2, colour = "black") +
  scale_fill_gradientn(colours = rev(ODV_colours))+
  labs(y = "Profundidad (m)", x = "Mes", fill = "Temp. (°C)") +
  scale_y_reverse(breaks = seq(0,500,25),limits = c(400, 0))+
  scale_x_continuous(breaks=c(15.5,45.5,75.5,106,136.5,167,197.5,228.5,259,289.5,320,350.5),labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","sep","oct","Nov","Dic"))+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+ggtitle("Promedio Histórico Temperatura \n  Subsuperficial (1988 - 2019) 9ºN 140ºW") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+theme(panel.border = element_blank(),
panel.background = element_blank())
  
                                                                                                            
h_c_d22  

#FILTRANDO solo 1 AÑO
sub= subset(TEMP, FECHA>="2000-01-01" & FECHA<="2000-12-31" )
sub[1]= NULL

profund=as.numeric(dim(sub)[2])    #numero de profundidades


#FORMATO SECCION (data frame) AYUDA
library(openxlsx)
library(stringi)
library(stringr)
SECCION=melt(sub, id=0)
colnames(SECCION)=c("PROFUNDIDAD","TEMPERATURA")
SECCION$PROFUNDIDAD<-stri_replace_first_regex(SECCION$PROFUNDIDAD, "[X]", "")
SECCION$PROFUNDIDAD=as.numeric(SECCION$PROFUNDIDAD)
SECCION$DIA=rep(1:366,profund)
SECCION=na.omit(SECCION)
col_order <- c("DIA","PROFUNDIDAD","TEMPERATURA")
SECCION2 <- SECCION[, col_order]



#interpolacion
#Numero de pixeles
mba.int <- mba.surf(SECCION2, no.X = 365, no.Y = 365, extend = T)
mba=mba.int$xyz.est
rownames(mba$z)=mba$x
colnames(mba$z)=mba$y
df=melt(mba$z, varnames = c('x', 'y'))

#ESCALA DE COLORES
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")

#GRAFICO
ggplot(data = df, aes(x = x, y = y)) +
  geom_raster(aes(fill = value)) +
  geom_contour_fill(aes(z = value), binwidth = 2, colour = "black", alpha = 0.2)+ 
  geom_text_contour(aes(z = value),stroke = 0.1,skip=0,check_overlap=T,size=5)+
  geom_contour(aes(z = value), breaks = 2, colour = "black") +
  scale_fill_gradientn(colours = rev(ODV_colours))+
  labs(y = "Profundidad (m)", x = "Mes", fill = "Temp. (°C)") +
  scale_y_reverse(breaks = seq(0,400,50),limits=c(400,0))+
  scale_x_continuous(breaks=c(15.5,45.5,75.5,106,136.5,167,197.5,228.5,259,289.5,320,350.5),labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","sep","oct","Nov","Dic"))+ggtitle("Temperatura Subsuperficial 2000 \n 9ºN 140ºW") +labs(caption="Fuente: Elaboración propia con datos de la NOAA") +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+theme(panel.border = element_blank(),
                                                                  panel.background = element_blank())

##############################################
###############################################
#############2022


#FILTRANDO solo 1 AÑO
sub= subset(TEMP, FECHA>="2013-01-01" & FECHA<="2013-12-31" )
sub[1]= NULL
View(sub)
profund=as.numeric(dim(sub)[2])    #numero de profundidades


#FORMATO SECCION (data frame) AYUDA
library(openxlsx)
library(stringi)
library(stringr)
SECCION=melt(sub, id=0)
colnames(SECCION)=c("PROFUNDIDAD","TEMPERATURA")
SECCION$PROFUNDIDAD<-stri_replace_first_regex(SECCION$PROFUNDIDAD, "[X]", "")
SECCION$PROFUNDIDAD=as.numeric(SECCION$PROFUNDIDAD)
SECCION$DIA=rep(1:365,profund)
SECCION=na.omit(SECCION)
col_order <- c("DIA","PROFUNDIDAD","TEMPERATURA")
SECCION2 <- SECCION[, col_order]



#interpolacion
#Numero de pixeles
mba.int <- mba.surf(SECCION2, no.X = 365, no.Y = 365, extend = T)
mba=mba.int$xyz.est
rownames(mba$z)=mba$x
colnames(mba$z)=mba$y
df=melt(mba$z, varnames = c('x', 'y'))

#ESCALA DE COLORES
ODV_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19", "#0db5e6", "#7139fe", "#d16cfa")

#GRAFICO
ggplot(data = df, aes(x = x, y = y)) +
  geom_raster(aes(fill = value)) +
  geom_contour_fill(aes(z = value), binwidth = 2, colour = "black", alpha = 0.2)+ 
  geom_text_contour(aes(z = value),stroke = 0.1,skip=0,check_overlap=T,size=5)+
  geom_contour(aes(z = value), breaks = 2, colour = "black") +
  scale_fill_gradientn(colours = rev(ODV_colours))+
  labs(y = "Profundidad (m)", x = "Mes", fill = "Temp. (°C)") +
  scale_y_reverse(breaks = seq(0,400,50),limits=c(400,0))+
  scale_x_continuous(breaks=c(15.5,45.5,75.5,106,136.5,167,197.5,228.5,259,289.5,320,350.5),labels=c("Abr","May","Jun","Jul","May","Jun","Jul","Ago","sep","oct","Nov","Dic"))+ggtitle("Temperatura Subsuperficial 2013 \n 9ºN 140ºW") +labs(caption="Fuente: Elaboración propia con datos de la NOAA")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+theme(panel.border = element_blank(),panel.background = element_blank())
sub
#########################
####Anomalia 

#temperatura (boya)
setwd("C:/Users/USUARIO/Desktop/2021-ii/Oceano")
library(ncdf4)
library(openxlsx)
n=nc_open("t9n140w_dy_dat.cdf")
TEMP=ncvar_get(n, varid="T_20")
depth=ncvar_get(n, varid="depth")

row.names(TEMP)=depth
time=ncvar_get(n,varid="time")
FECHA=as.Date("1988-05-31") + 1:length(time)
TEMP<- as.data.frame(t(TEMP))
TEMP=cbind(FECHA,TEMP)

#columnas character to numeric
df=TEMP
sapply(df,class)
cols <- names(df)[2:14]
df[cols] <- lapply(df[cols], as.numeric)

str(df)
#pivot table
df=df %>%
  group_by(month = month(FECHA), day = day(FECHA)) %>%
  summarise_if(is.numeric, mean , na.rm=TRUE)

#df$X8=NULL

#seccion historica
df[1:2]=NULL
df <- data.frame(df)
prof=as.numeric(dim(df)[2])





#FILTRANDO solo 1 AÑO
sub= subset(TEMP, FECHA>="2013-01-01" & FECHA<="2013-12-31")
sub[1]= NULL
View(df)
str(sub)
str(df)
profund=as.numeric(dim(sub)[2])
#df <- df[-(214:366),]
#anom <- sub-df[-(1:91),]
anom <- sub-df[-60,]
View(anom)

SECCION=melt(anom, id=0)
colnames(SECCION)=c("PROFUNDIDAD","TEMPERATURA")
SECCION$PROFUNDIDAD<-stri_replace_first_regex(SECCION$PROFUNDIDAD, "[X]", "")
SECCION$PROFUNDIDAD=as.numeric(SECCION$PROFUNDIDAD)
SECCION$DIA=rep(1:365,profund)
SECCION=na.omit(SECCION)
col_order <- c("DIA","PROFUNDIDAD","TEMPERATURA")
SECCION2 <- SECCION[, col_order]



#interpolacion
#Numero de pixeles
mba.int <- mba.surf(SECCION2, no.X = 365, no.Y = 365, extend = T)
mba=mba.int$xyz.est
rownames(mba$z)=mba$x
colnames(mba$z)=mba$y
df=melt(mba$z, varnames = c('x', 'y'))

#ESCALA DE COLORES
ODV_colours <- c('#fdae61','#ffffbf','#abd9e9', '#2c7bb6')
#ODV_colours <- c('#d7191c','#fdae61','#ffffbf','#abd9e9', '#2c7bb6')               

#GRAFICO
ggplot(data = df, aes(x = x, y = y)) +
  geom_raster(aes(fill = value)) +
  geom_contour_fill(aes(z = value), binwidth = 2, colour = "black", alpha = 0.2)+ 
  geom_text_contour(aes(z = value),stroke = 0.1,skip=0,check_overlap=T,size=5)+
  geom_contour(aes(z = value), breaks = 2, colour = "black") +
  scale_fill_gradientn(colours = rev(ODV_colours))+
  labs(y = "Profundidad (m)", x = "Mes", fill = "Temp. (°C)") +
  scale_y_reverse(breaks = seq(0,400,50),limits=c(400,0))+
  scale_x_continuous(breaks=c(15.5,45.5,75.5,106,136.5,167,197.5,228.5,259,289.5,320,350.5),labels=c("Abr","May","Jun","Jul","May","Jun","Jul","Ago","sep","oct","Nov","Dic"))+
  ggtitle("Anomalía Temperatura Subsuperficial \n 2022 9ºN 140ºW") +labs(caption="Fuente: Elaboración propia con datos de la NOAA")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+theme(panel.border = element_blank(),
                                                                  panel.background = element_blank())

library(RColorBrewer)
