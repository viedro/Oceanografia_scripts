##Librería para carga de datos

library(readr)


setwd("C:/Users/Usuario/Desktop/2021-ii/Oceano/Boya")
vx <-read.table("Vientos.txt",
               header = T,
               sep = "",           
               dec = ".")
vx

#Convertimos a dataframe
vx <- data.frame(vx)
#Borramos columna innecesaria
vx <- vx[,-c(2,5:8)]
vx
#Cambiamos nombres de columnas
colnames(vx)<- c("Fecha","UWND","VWND")

View(vx)

str(vx)
#Establecemos columna de fecha
vx[,1] <- as.Date(vx[,1],format="%d/%m/%Y")
#Vemos si la columna es de tipo fecvxa
str(vx)

#LIbrería para manipular datos
library(tidyverse)


#Completado epico de fechas y datos Xd
vx_c <- vx %>% 
  complete(Fecha = seq.Date(min(Fecha),max(Fecha),by="day"))

#Convirtiendo a dataframe(mejor uso)
vx_c <- data.frame(vx_c)

#write.csv(vx_c,"vientos_completo.csv")

#Cmabiando la data nula a NA
vx_c[vx_c ==  -99.9] <- NA

#Primera grafica
plot(vx_c[,1:2])
str(dat_men)
#Estadisticos
e_uv=NULL
for (k in 2:ncol(dat_men)){
  min<- min(dat_men[,k],na.rm=T)
  max <- max(dat_men[,k],na.rm=T)
  sd <- sd(dat_men[,k],na.rm=T)
  media <- mean(dat_men[,k],na.rm=T)
  mediana <- median(dat_men[,k],na.rm=T)
  ag <- rbind(min,max,sd,media,mediana)
  e_uv <- cbind(e_uv,ag)}

e_uv<- data.frame(e_uv)
names(e_uv) <-c("UWND","VWND")

e_uv
View(vx_c)
#Histograma de los datos
hist(dat_men$UWND,col="gray48",main = "Histograma UWND -VWND", ylab = "Frecuencia",xlab="Velocidad",density = 50)
hist(dat_men$VWND, add = TRUE, col =rgb(1, 0, 0, alpha = 0.3),density=50)
legend("topright", c("UWND", "VWND"),col = c("gray48", rgb(1, 0, 0, alpha = 0.3)),lty = 1,lwd=5)


#Valor de la media-- posible linea base
media_u <-mean(vx_c[,2],na.rm=T)
media_v <-mean(vx_c[,3],na.rm=T)

#Gráfico en brutomejorado ggplot -- lineas
ggplot(data=vx_c, aes(x=Fecha))+geom_line(aes(y=UWND),colour="dodgerblue2")+ylab("Altura dinámica")+ggtitle("Viento Zonal Datos en Bruto")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+guides(color = guide_legend(override.aes = list(size = 4)))+scale_x_date(date_labels = '%Y',date_breaks='3 years')+geom_hline(yintercept = media_u,colour="red")+coord_flip()
getwd()

#write.csv(h_c, "data_completada.csv")
###########################################
#####Promedio mensual de cada año
library(xts)
library(zoo)

vx_mes_xts <- xts(vx_c[,2],order.by = vx_c[,1])
str(vx_mes_xts)
vx_mes<- apply.monthly(vx_mes_xts,FUN=mean,na.rm=T)

fecha <- seq.Date(as.Date("1988-05-31"),as.Date("2022-05-31"),by="month")
data_mes <- data.frame(fecha,coredata(vx_mes))
colnames(data_mes)<-c("Fecha","UWND")
ggplot(data=data_mes, aes(x=Fecha))+geom_line(aes(y=UWND),colour="dodgerblue2")+ylab("Altura dinámica")+ggtitle("Altura dinámica Promedio Mensual-Anual")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+guides(color = guide_legend(override.aes = list(size = 4)))+scale_x_date(date_labels = '%Y',date_breaks='5 years')+geom_hline(yintercept = media_u,colour="red")+coord_flip()



################Promedio mensual de todos los años
dia <- as.numeric(format(vx_c$Fecha,'%d'))
mes <- as.numeric(format(vx_c$Fecha,'%m'))
año <- as.numeric(format(vx_c$Fecha,'%Y'))

vx_c_d <- data.frame(vx_c,dia,mes,año)

str(vx_c_d)

#Eliminando datos en estudio

vx_c_d20 <- vx_c_d[vx_c_d$año == 2020,]
vx_c_d97 <- vx_c_d[vx_c_d$año == 1997,]
vx_c_d98 <- vx_c_d[vx_c_d$año == 1998,]
vx_c_d10 <- vx_c_d[vx_c_d$año == 2010,]
vx_c_d22 <- vx_c_d[vx_c_d$año == 2022,]
vx_c_d13 <- vx_c_d[vx_c_d$año == 2013,]

vx_c_d<- vx_c_d[vx_c_d$año <= 2019,]


View(vx_c_d)
#Datos 

#####IBucle promedio historico zonal
w=NULL
for (j in 1:12){
  for (i in 1:31){
    med <-vx_c_d$UWND[vx_c_d$mes== j & vx_c_d$dia ==i]
    med <- mean(na.omit(med))
    med=data.frame(med)
    w=rbind(w,med)
  }
}

#####Bucle Promedio Histórico meridional
q=NULL
for (j in 1:12){
  for (i in 1:31){
    med <-vx_c_d$VWND[vx_c_d$mes== j & vx_c_d$dia ==i]
    med <- mean(na.omit(med))
    med=data.frame(med)
    q=rbind(q,med)
  }
}
q <- data.frame(q)
q <- q[!is.nan(q$med),]


w <- data.frame(w)
w <- w[!is.nan(w$med),]


Dias <- 1:366
dat_men <- data.frame(Dias,w,q)
colnames(dat_men) <- c("Días","UWND","VWND")
#write.csv(dat_men, "vientos_prom_historico.csv")


###Grafica basica
plot(dat_men)

###Grafica fina --- Viento zonal: Promedio historico

ggplot(data=dat_men, aes(x=Días))+geom_bar(aes(y=UWND),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+ggtitle("Viento zonal Promedio Histórico \n (1988-2019) 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 1, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 1, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 1, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 1, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 1, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 1,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 1, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -6, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -6, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 1, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 1, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 1, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-8,2,1))


###Grafica fina --- viento meridional
ggplot(data=dat_men, aes(x=Días))+geom_bar(aes(y=VWND),colour="dodgerblue2",stat='identity',size=1)+ylab("Vwnd (m/s)")+ggtitle("Viento meridional Promedio histórico Mensual-Diario \n (1988-2019) 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 3, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 3, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 3,stat = "unique", label = "Mar",colour="red",size=4)+geom_text(data = NULL, x = 106, y = 3, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 3, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 3, label = "Jun",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 3, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -5, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -5, label = "Sep",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 289.5, y = 3, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 3,stat = "unique", label = "Nov",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 3,stat = "unique", label = "Dic",colour="red",size=4)+scale_y_continuous(breaks = seq(-6,4,1))


#####Grafica vientos 2020

vx_c_d20$Dias <- 1:366
str(vx_c_d20)

ggplot(data=vx_c_d20, aes(x=Dias))+geom_bar(aes(y=VWND),colour="dodgerblue2",stat='identity',size=1,fill="dodgerblue2")+ylab("Vwnd (m/s)")+xlab("Días")+ggtitle("Viento meridional Promedio Mensual-Diario \n 2020 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 3, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 3, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 3,stat = "unique", label = "Mar",colour="red",size=4)+geom_text(data = NULL, x = 106, y = 3, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 3, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 3, label = "Jun",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 3, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -5, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -5, label = "Sep",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 289.5, y = 3, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 3,stat = "unique", label = "Nov",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 3,stat = "unique", label = "Dic",colour="red",size=4)


##Grafica 97 zonal 
vx_c_d97<- vx_c_d[vx_c_d$año == 1997,]
Dias97 = 1:365
vx_c_d97 <- data.frame(vx_c_d97,Dias97)
str(vx_c_d97)
ggplot(data=vx_c_d97, aes(x=Dias97))+geom_bar(aes(y=UWND),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento zonal 1997 Mensual-Diario \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 4, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 4, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 4, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 4, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 4, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 4,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 4, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -8, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -8, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 4, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 4, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 4, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,6,1))


###97 --- viento meridional
ggplot(data=vx_c_d97, aes(x=Dias97))+geom_bar(aes(y=VWND),colour="dodgerblue2",stat='identity',size=1)+ylab("Vwnd (m/s)")+xlab("Días")+ggtitle("Viento meridional 1997 Mensual-Diario \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 7, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 7, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 7,stat = "unique", label = "Mar",colour="red",size=4)+geom_text(data = NULL, x = 106, y = 7, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 7, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 7, label = "Jun",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 7, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 7, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 7, label = "Sep",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 289.5, y = 7, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 7,stat = "unique", label = "Nov",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 7,stat = "unique", label = "Dic",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,7,1))

############Anomalia ZONAL 97

vx_c_dc97 <- data.frame(dat_men[-60,],vx_c_d97 )
anom <- vx_c_dc97$UWND.1- vx_c_dc97$UWND

Dias <- 1:365
View(anom)
anom<- data.frame(anom,Dias)


ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Anomalía Viento zonal 1997 \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 6, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 6, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 6, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 6, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 6, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 6,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 6, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 6, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 6, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 6, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 6, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 6, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,7,1),limits=c(-6,6.5))


#############################333

############Anomalia meridional 97

vx_c_dc97 <- data.frame(dat_men[-60,],vx_c_d97 )
anom <- vx_c_dc97$VWND.1- vx_c_dc97$VWND

Dias <- 1:365
View(anom)
anom<- data.frame(anom,Dias)


ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),colour="dodgerblue2",stat='identity',size=1)+ylab("VWND (m/s)")+xlab("Días")+ggtitle("Anomalía Viento meridional 1997 Mensual-Diario \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 5.5, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 5.5, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 5.5, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 5.5, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 5.5, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 5.5,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 5.5, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 5.5, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 5.5, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 5.5, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 5.5, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 5.5, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,6,1),limits=c(-6,5.5))

###################Comparación zonal
vx_c_dc97 <- data.frame(dat_men[-60,],vx_c_d97 )

ggplot(data=vx_c_dc97, aes(x=Dias97))+geom_bar(aes(y=UWND,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=UWND.1,fill="1997"),stat='identity',size=1,alpha=0.5)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento zonal Histórico vs 1997 \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 4, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 4, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 4, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 4, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 4, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 4,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 4, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -8, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -8, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 4, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 4, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 4, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,6,1))+theme(legend.position = "top")

###Comparacion meridional
vx_c_dc97 <- data.frame(dat_men[-60,],vx_c_d97 )

ggplot(data=vx_c_dc97, aes(x=Dias97))+geom_bar(aes(y=VWND,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=VWND.1,fill="1997"),stat='identity',size=1,alpha=0.5)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento Meridional Histórico vs 1997 \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 4, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 4, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 4, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 4, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 4, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 4,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 4, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -8, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -8, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 4, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 4, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 4, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,7,1))


##Grafica 98 zonal 
vx_c_d98<- vx_c_d[vx_c_d$año == 1998,]
Dias98 = 1:365
vx_c_d98 <- data.frame(vx_c_d98,Dias98)
str(vx_c_d98)
ggplot(data=vx_c_d98, aes(x=Dias98))+geom_bar(aes(y=UWND),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento zonal 1998 Mensual-Diario \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 4, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 4, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 4, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 4, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 4, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 4,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 4, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -8, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -8, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 4, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 4, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 4, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,5,1))






















###98 --- viento meridional
ggplot(data=vx_c_d98, aes(x=Dias98))+geom_bar(aes(y=VWND),colour="dodgerblue2",stat='identity',size=1)+ylab("Vwnd (m/s)")+xlab("Días")+ggtitle("Viento meridional 1998 Mensual-Diario \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 5, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 10, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 10,stat = "unique", label = "Mar",colour="red",size=4)+geom_text(data = NULL, x = 106, y = 10, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 10, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 10, label = "Jun",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 10, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 10, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 10, label = "Sep",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 289.5, y = 10, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 10,stat = "unique", label = "Nov",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 10,stat = "unique", label = "Dic",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,8,1),limits=c(-8,10))

############Anomalia ZONAL 98

vx_c_dc98 <- data.frame(dat_men[-60,],vx_c_d98)
anom <- vx_c_dc98$UWND.1- vx_c_dc98$UWND

Dias <- 1:365
View(anom)
anom<- data.frame(anom,Dias)


ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Anomalía Viento zonal 1998 \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 5, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 5, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 5, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 5, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 5, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 5,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 5, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 225.5, y = -5, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -5, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = -5, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = -5, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = -5, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,8,1),limits=c(-5,5))


############Anomalia meridional 98

vx_c_dc98 <- data.frame(dat_men[-60,],vx_c_d98 )
anom <- vx_c_dc98$VWND.1- vx_c_dc98$VWND

Dias <- 1:365
View(anom)
anom<- data.frame(anom,Dias)


ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),colour="dodgerblue2",stat='identity',size=1)+ylab("VWND (m/s)")+xlab("Días")+ggtitle("Anomalía Viento meridional 1998 \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 7, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 7, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 7, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 7, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 7, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 7,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 7, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -5, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -5, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 7, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 7, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 7, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,8,1),limits=c(-5,7))


########################################

Dias22 <- 1:214


str(vx_c_d22)
##Grafica 2022 zonal 
vx_c_d22<- vx_c_d[vx_c_d$año == 2022,]
Dias22 = 1:214
vx_c_d22 <- data.frame(vx_c_d22,Dias22)
str(vx_c_d22)
ggplot(data=vx_c_d22, aes(x=Dias22))+geom_bar(aes(y=UWND),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento zonal 2022 Mensual-Diario \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 2, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 2, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 2, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 2, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 2, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 2,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 2, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -8, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -8, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 2, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 2, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 2, label = "Dic",stat = "unique",colour="red",size=4)






















###2022 --- viento meridional
ggplot(data=vx_c_d22, aes(x=Dias22))+geom_bar(aes(y=VWND),colour="dodgerblue2",stat='identity',size=1)+ylab("Vwnd (m/s)")+xlab("Días")+ggtitle("Viento meridional 2022 Mensual-Diario \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 10, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 10, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 10,stat = "unique", label = "Mar",colour="red",size=4)+geom_text(data = NULL, x = 106, y = 10, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 10, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 10, label = "Jun",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 10, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 10, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 10, label = "Sep",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 289.5, y = 10, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 10,stat = "unique", label = "Nov",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 10,stat = "unique", label = "Dic",colour="red",size=4)+scale_y_continuous(limits=c(-10,10))

############Anomalia ZONAL 2022



dat_men22 <- dat_men[-60,]
vx_c_dc22 <- data.frame(dat_men22[1:214,],vx_c_d22)
anom <- vx_c_dc22$UWND.1- vx_c_dc22$UWND

Dias <- 1:214
View(anom)
anom<- data.frame(anom,Dias)

Dias <- 1:214
View(anom)
anom<- data.frame(anom,Dias)



Dias <- 1:365
View(anom)
anom<- data.frame(anom,Dias)


ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Anomalía Viento zonal 2022 \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y=4.5, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y=4.5, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y=4.5, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y=4.5, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y=4.5, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y=4.5,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y=4.5, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y=4.5, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y=4.5, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y=4.5, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y=4.5, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y=4.5, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks=seq(-4,5,1),limits=c(-3.5,4.5))


############Anomalia meridional 2022

anom <- vx_c_dc22$VWND.1- vx_c_dc22$VWND
anom<- data.frame(anom,Dias)

ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),colour="dodgerblue2",stat='identity',size=1)+ylab("VWND (m/s)")+xlab("Días")+ggtitle("Anomalía Viento meridional 2022 Mensual-Diario \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 5, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 5, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 5, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 5, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 5, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 5,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 5, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 5, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 5, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 5, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 5, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 5, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks=seq(-9,9,1))









###################Comparacion
vx_c_dc22

ggplot(data=vx_c_dc22, aes(x=Dias22))+geom_bar(aes(y=UWND,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=UWND.1,fill="2022"),stat='identity',size=1,alpha=0.5)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento zonal Histórico vs 2022 \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 1, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 1, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 1, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 1, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 1, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 1,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 1, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -8, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -8, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 1, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 1, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 1, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-8,5,1))



ggplot(data=vx_c_dc22, aes(x=Dias22))+geom_bar(aes(y=VWND,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=VWND.1,fill="2022"),stat='identity',size=1,alpha=0.5)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento Meridional Histórico vs 2022 \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 4, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 4, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 4, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 4, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 4, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 4,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 4, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 4, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 4, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 4, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 4, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 4, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,7,1))





##############################################
################################################
################################################





##Grafica 2010 zonal 
vx_c_d10<- vx_c_d[vx_c_d$año == 2010,]
Dias10 = 1:365
vx_c_d10 <- data.frame(vx_c_d10,Dias10)
str(vx_c_d10)
ggplot(data=vx_c_d10, aes(x=Dias10))+geom_bar(aes(y=UWND),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento zonal 2010 Mensual-Diario \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 1, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 1, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 1, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 1, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 1, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 1,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 1, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 1, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 1, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 1, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 1, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 1, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,8,1),limits=c(-8,1))






















###98 --- viento meridional
ggplot(data=vx_c_d98, aes(x=Dias98))+geom_bar(aes(y=VWND),colour="dodgerblue2",stat='identity',size=1)+ylab("Vwnd (m/s)")+xlab("Días")+ggtitle("Viento meridional 1998 Mensual-Diari0 \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 10, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 10, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 10,stat = "unique", label = "Mar",colour="red",size=4)+geom_text(data = NULL, x = 106, y = 10, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 10, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 10, label = "Jun",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 10, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 10, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 10, label = "Sep",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 289.5, y = 10, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 10,stat = "unique", label = "Nov",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 10,stat = "unique", label = "Dic",colour="red",size=4)

############Anomalia ZONAL2010

vx_c_dc10 <- data.frame(dat_men[-60,],vx_c_d10)
anom <- vx_c_dc10$UWND.1- vx_c_dc10$UWND

Dias <- 1:365
View(anom)
anom<- data.frame(anom,Dias)


ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Anomalía Viento zonal 2010 \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 4, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 4, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 4, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 4, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 4, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 4,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 4, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 4, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 4, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 4, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 4, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 4, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks=seq(-3,5,1))


############Anomalia meridional 98

vx_c_dc10 <- data.frame(dat_men[-60,],vx_c_d10 )
anom <- vx_c_dc10$VWND.1- vx_c_dc10$VWND

Dias <- 1:365
View(anom)
anom<- data.frame(anom,Dias)


ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),colour="dodgerblue2",stat='identity',size=1)+ylab("VWND (m/s)")+xlab("Días")+ggtitle("Anomalía Viento meridional 2010 \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 6, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 6, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 6, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 6, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 6, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 6,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 6, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 6, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 6, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 6, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 6, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 6, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks=seq(-5,8,1))

                                                      #####Compararcion

###################Comparación zonal
vx_c_dc10 <- data.frame(dat_men[-60,],vx_c_d10 )

ggplot(data=vx_c_dc10, aes(x=Dias10))+geom_bar(aes(y=UWND,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=UWND.1,fill="2010"),stat='identity',size=1,alpha=0.5)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento zonal Histórico vs 2010 \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 1, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 1, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 1, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 1, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 1, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 1,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 1, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -8, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -8, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 1, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 1, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 1, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-8,1,1))


###############


vx_c_dc10 <- data.frame(dat_men[-60,],vx_c_d10)

ggplot(data=vx_c_dc10, aes(x=Dias10))+geom_bar(aes(y=VWND,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=VWND.1,fill="2010"),stat='identity',size=1,alpha=0.5)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento Meridional Histórico vs 2010 \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 4, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 4, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 4, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 4, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 4, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 4,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 4, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -8, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -8, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 4, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 4, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 4, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,7,1))






##################2013


##Grafica 2010 zonal 
vx_c_d13<- vx_c_d[vx_c_d$año == 2013,]
Dias13 = 1:365
vx_c_d13 <- data.frame(vx_c_d13,Dias13)
str(vx_c_d10)
ggplot(data=vx_c_d10, aes(x=Dias10))+geom_bar(aes(y=UWND),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento zonal 2010 Mensual-Diario \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 1, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 1, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 1, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 1, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 1, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 1,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 1, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 1, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 1, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 1, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 1, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 1, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,8,1),limits=c(-8,1))






















###98 --- viento meridional
ggplot(data=vx_c_d98, aes(x=Dias98))+geom_bar(aes(y=VWND),colour="dodgerblue2",stat='identity',size=1)+ylab("Vwnd (m/s)")+xlab("Días")+ggtitle("Viento meridional 1998 Mensual-Diari0 \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 10, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 10, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 10,stat = "unique", label = "Mar",colour="red",size=4)+geom_text(data = NULL, x = 106, y = 10, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 10, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 10, label = "Jun",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 10, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 10, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 10, label = "Sep",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 289.5, y = 10, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 10,stat = "unique", label = "Nov",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 10,stat = "unique", label = "Dic",colour="red",size=4)

############Anomalia ZONAL2010

vx_c_dc10 <- data.frame(dat_men[-60,],vx_c_d10)
anom <- vx_c_dc10$UWND.1- vx_c_dc10$UWND

Dias <- 1:365
View(anom)
anom<- data.frame(anom,Dias)


ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Anomalía Viento zonal 2010 \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 4, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 4, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 4, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 4, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 4, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 4,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 4, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 4, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 4, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 4, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 4, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 4, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks=seq(-3,5,1))


############Anomalia meridional 98

vx_c_dc10 <- data.frame(dat_men[-60,],vx_c_d10 )
anom <- vx_c_dc10$VWND.1- vx_c_dc10$VWND

Dias <- 1:365
View(anom)
anom<- data.frame(anom,Dias)


ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),colour="dodgerblue2",stat='identity',size=1)+ylab("VWND (m/s)")+xlab("Días")+ggtitle("Anomalía Viento meridional 2010 \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 6, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 6, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 6, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 6, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 6, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 6,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 6, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 6, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 6, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 6, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 6, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 6, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks=seq(-5,8,1))

#####Compararcion

###################Comparación zonal
vx_c_dc13 <- data.frame(dat_men[-60,],vx_c_d13 )

ggplot(data=vx_c_dc13, aes(x=Dias13))+geom_bar(aes(y=UWND,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=UWND.1,fill="2013"),stat='identity',size=1,alpha=0.5)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento zonal Histórico vs 2013 \n 9ºN 140ºW")+coord_flip()+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 1, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 1, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 1, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 1, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 1, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 1,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 1, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -8, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -8, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 1, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 1, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 1, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-8,8,1))


###############

#COMPARACION 2013 meridional
vx_c_dc13 <- data.frame(dat_men[-60,],vx_c_d13)

ggplot(data=vx_c_dc13, aes(x=Dias13))+geom_bar(aes(y=VWND,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=VWND.1,fill="2013"),stat='identity',size=1,alpha=0.5)+ylab("UWND (m/s)")+xlab("Días")+ggtitle("Viento Meridional Histórico vs 2013 \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 4, label = "Ene",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 4, label = "Feb",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 4, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 4, label = "Abr",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 4, label = "May",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 4,stat = "unique", label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 4, label = "Jul",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = -4, label = "Ago",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 259, y = -4, label = "Sep",colour="red",stat = "unique",size=4)+geom_text(data = NULL, x = 289.5, y = 4, label = "Oct",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 4, label = "Nov",stat = "unique",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 4, label = "Dic",stat = "unique",colour="red",size=4)+scale_y_continuous(breaks = seq(-9,7,1))
