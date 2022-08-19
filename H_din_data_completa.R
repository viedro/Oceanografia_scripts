##Librería para carga de datos

library(readr)

setwd("C:/Users/Usuario/Desktop/2021-ii/Oceano/Boya")

h <-read.table("Altura_din.txt",
                 header = T,
                 sep = "",           
                 dec = ".")
h

View(h)

#Convertimos a dataframe
h <- data.frame(h)
head(h)
#Borramos columna innecesaria
h <- h[,-2]


h
#Cambiamos nombres de columnas
colnames(h)<- c("Fecha","Dyn")


str(h)
#Establecemos columna de fecha
h[,1] <- as.Date(h[,1],format="%d/%m/%Y")
#Vemos si la columna es de tipo fecha
str(h)

#LIbrería para manipular datos
library(tidyverse)  

h<-data.frame(h)

#Completado epico de fechas y datos Xd
h_c <- h %>% 
  complete(Fecha = seq.Date(min(Fecha),max(Fecha),by="day"))

#Convirtiendo a dataframe(mejor uso)
h_c <- data.frame(h_c)
str(h_c)
#Cmabiando la data nula a NA
h_c[h_c ==  -9.99] <- NA

#Primera grafica
plot(h_c)

summary(h_c)
sd(h_c)
#Valor de la media-- posible linea base
media <-mean(h_c[,2],na.rm=T)
library(plotly)
#Gráfico en brutomejorado ggplot -- lineas
ggplot(data=h_c, aes(x=Fecha))+geom_line(aes(y=Dyn),colour="dodgerblue2")+ylab("Altura dinámica")+ggtitle("Altura dinámica Datos en Bruto")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+guides(color = guide_legend(override.aes = list(size = 4)))+scale_x_date(date_labels = '%Y',date_breaks='3 years')+geom_hline(yintercept = media,colour="red")
getwd()

#write.csv(h_c, "data_completada.csv")
###########################################
#####Promedio mensual de cada año
library(xts)
library(zoo)

h_mes_xts <- xts(h_c[,2],order.by = h_c[,1])
str(h_mes_xts)
h_mes<- apply.monthly(h_mes_xts,FUN=mean,na.rm=T)

fecha <- seq.Date(as.Date("1988-05-31"),as.Date("2022-05-31"),by="month")
data_mes <- data.frame(fecha,coredata(h_mes))
colnames(data_mes)<-c("Fecha","Dyn")
ggplot(data=data_mes, aes(x=Fecha))+geom_line(aes(y=Dyn),colour="dodgerblue2")+ylab("Altura dinámica")+ggtitle("Altura dinámica Promedio Mensual-Anual")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+guides(color = guide_legend(override.aes = list(size = 4)))+scale_x_date(date_labels = '%Y',date_breaks='5 years')+geom_hline(yintercept = media,colour="red")


################Promedio mensual de todos los años

h_c.Dyn.ts=ts(h_c$Dyn, start=c(1989,1), frequency = 365)

h_c <- h_c[-(1:215),]

library(xts)
plot(decompose(season_ts))
season_ts <- na.locf(h_c.Dyn.ts)

install.packages("fUnitRoots")
library("fUnitRoots")
urkpssTest(season_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)

count_ma = ts(na.omit(h_c), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_apartamento <- seasadj(decomp)
plot(decomp)






library(forecast)


ajuste <- auto.arima(y = h_c.Dyn.ts)

summary(ajuste)


predicciones <- forecast(ajuste)

library(plotly)

ggplotly(autoplot(predicciones))
ga <- predicciones[ ,tail(seq_along(predicciones), 900)]

autoplot(predicciones)+scale_x_continuous(limits=c(as,4))


dia <- as.numeric(format(h_c$Fecha,'%d'))
mes <- as.numeric(format(h_c$Fecha,'%m'))
año <- as.numeric(format(h_c$Fecha,'%Y'))

View(h_c)
h_c_d <- data.frame(h_c,dia,mes,año)
str(h_c_d)

h_c_d22 <- h_c_d[h_c_d$año == 2022,]
h_c_d13 <- h_c_d[h_c_d$año == 2013,]
h_c_d <- h_c_d[h_c_d$año <= 2019,]



#####Intento 1 -------- no es :v
q=NULL
for (i in 1:12){
  me <-h_c_d$Dyn[h_c_d$mes ==i]
  me <- mean(na.omit(me))
  me=data.frame(me)
  q=rbind(q,me)
  }
q

#####Intento 2 ------ este si es xddd
w=NULL
for (j in 1:12){
  for (i in 1:31){
    med <-h_c_d$Dyn[h_c_d$mes== j & h_c_d$dia ==i]
    med <- mean(na.omit(med))
    med=data.frame(med)
    w=rbind(w,med)
  }
}

w <- data.frame(w)
w <- w[!is.nan(w$med),]

Dias <- 1:366

dat_men <- data.frame(Dias,w)
colnames(dat_men) <- c("Dias","Dyn")
str(dat_men)
###Grafica basica
plot(dat_men)

###Grafica fina

ggplot(data=dat_men, aes(x=Dias))+geom_line(aes(y=Dyn),colour="dodgerblue2",size=1)+ylab("Altura dinámica")+ggtitle("Altura dinámica Promedio Histórico Mensual-Diario \n (1988-2019) 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_hline(yintercept = media,colour="red")+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 102, label = "Ene",colour="red",size=4)+geom_text(data = NULL, x = 45.5, y = 102, label = "Feb",colour="red",size=4)+geom_text(data = NULL, x = 75.5, y = 102, label = "Mar",colour="red",size=4)+geom_text(data = NULL, x = 106, y = 102, label = "Abr",colour="red",size=4)+geom_text(data = NULL, x = 136.5, y = 102, label = "May",colour="red",size=4)+geom_text(data = NULL, x = 167, y = 102, label = "Jun",colour="red",size=4)+geom_text(data = NULL, x = 197.5, y = 102, label = "Jul",colour="red",size=4)+geom_text(data = NULL, x = 228.5, y = 102, label = "Ago",colour="red",size=4)+geom_text(data = NULL, x = 259, y = 102, label = "Sep",colour="red",size=4)+geom_text(data = NULL, x = 289.5, y = 102, label = "Oct",colour="red",size=4)+geom_text(data = NULL, x = 320, y = 102, label = "Nov",colour="red",size=4)+geom_text(data = NULL, x = 350.5, y = 102, label = "Dic",colour="red",size=4)+xlab("Días")

ggplotly(graph)

###Grafica fina barras
ggplot(data=dat_men, aes(x=Dias))+geom_bar(aes(y=Dyn),colour="dodgerblue2",stat='identity',size=1)+ylab("Altura dinámica")+ggtitle("Altura dinámica Promedio Histórico Mensual-Diario \n (1988-2019) 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 105, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 105, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 105, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 105, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 105, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 105, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 105, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 105, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 105, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 105, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 105, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 105, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+coord_cartesian(ylim = c(90, 105))+scale_y_continuous(breaks = seq(90,105,2))
str(dat_men)
#ggplot(data=dat_men, aes(x=Dias))+geom_bar(aes(y=UWND),colour="dodgerblue2",stat='identity',size=1)+ylab("UWND (m/s)")+ggtitle("Viento zonal Promedio histórico Mensual-Diaria \n (1988-2022) 9ºN 140ºW")+coord_flip()


#Estadisticos
e_h_c=NULL
for (k in 2:ncol(h_c)){
  min<- min(dat_men[,k],na.rm=T)
  max <- max(dat_men[,k],na.rm=T)
  sd <- sd(dat_men[,k],na.rm=T)
  media <- mean(dat_men[,k],na.rm=T)
  mediana <- median(dat_men[,k],na.rm=T)
  ag <- rbind(min,max,sd,media,mediana)
  e_h_c<- cbind(e_h_c,ag)}

e_h_c<- data.frame(e_h_c)
e_h_c

hist(dat_men$Dyn,col="gray48",main = "Histograma HDyn", ylab = "Frecuencia",xlab="Altura Dinámica",density = 50)
hist(vx_c$VWND, add = TRUE, col =rgb(1, 0, 0, alpha = 0.3),density=50)
legend("topright", c("UWND", "VWND"),col = c("gray48", rgb(1, 0, 0, alpha = 0.3)),lty = 1,lwd=5)


#########  1997
h_c_d97 <- h_c_d[h_c_d$año == 1997,]

str(h_c_d97)
Dias97 <- 1:365

h_c_d97 <- data.frame(h_c_d97,Dias97)

###Grafica fina barras
ggplot(data=h_c_d97, aes(x=Dias97))+geom_bar(aes(y=Dyn),colour="dodgerblue2",stat='identity',size=1)+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n (1997) 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 106, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 106, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 106, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 106, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 106, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 106, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 106, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 106, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 106, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 106, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 106, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 106, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+coord_cartesian(ylim = c(80,106))+scale_y_continuous(breaks = seq(80,104,2))

###Comparacion- barras

h_c_dc <- data.frame(dat_men[-60,],h_c_d97)

str(h_c_dc)
ggplot(data=h_c_dc, aes(x=Dias97))+geom_bar(aes(y=Dyn,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=Dyn.1,fill="1997"),stat='identity',size=1,alpha=0.5)+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n (Histórico vs 1997) 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill=" ")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 106, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 106, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 106, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 106, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 106, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 106, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 106, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 106, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 106, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 106, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 106, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 106, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+coord_cartesian(ylim = c(80,106))+scale_y_continuous(breaks = seq(80,104,2))

#Anomalia 97
anom <- h_c_dc$Dyn.1- h_c_dc$Dyn
Dias <- 1:365

anom<- data.frame(anom,Dias)

ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),stat='identity',size=1,color="dodgerblue2")+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n Anomalía 1997 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill=" ")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 4, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 4, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 4, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 4, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 4, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 4, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 4, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 4, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 4, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 4, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 4, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 4, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+scale_y_continuous(breaks = seq(-17.5,15,2.5),limits=c(-17.5,4))

h_c_dc <- data.frame(dat_men[-60,],h_c_d98)





##########98
View(h_c_d98)
h_c_d98 <- h_c_d[h_c_d$año == 1998,]

str(h_c_d98)
Dias98 <- 1:365

h_c_d98 <- data.frame(h_c_d98,Dias98)

###Grafica fina barras
ggplot(data=h_c_d98, aes(x=Dias98))+geom_bar(aes(y=Dyn),colour="dodgerblue2",stat='identity',size=1)+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n (1998) 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 124, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 124, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 124, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 124, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 124, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 124, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 124, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 124, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 124, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 124, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 124, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 124, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+coord_cartesian(ylim = c(83,124))+scale_y_continuous(breaks = seq(84,124,4))


#############Comparación 
h_c_dc98 <- data.frame(dat_men[-60,],h_c_d98)


ggplot(data=h_c_dc98, aes(x=Dias98))+geom_bar(aes(y=Dyn,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=Dyn.1,fill="1998"),stat='identity',size=1,alpha=0.5)+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n Histórico vs 1998 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 124, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 124, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 124, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 124, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 124, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 124, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 124, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 124, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 124, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 124, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 124, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 124, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+coord_cartesian(ylim = c(83,124))+scale_y_continuous(breaks = seq(84,124,4))





#Anomalia 98

h_c_dc98 <- data.frame(dat_men[-60,],h_c_d98)
anom <- h_c_dc98$Dyn.1- h_c_dc98$Dyn
Dias <- 1:365


anom<- data.frame(anom,Dias)

ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),stat='identity',size=1,color="dodgerblue2")+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n Anomalía 1998 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill=" ")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 25, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 25, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 25, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 25, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 25, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 25, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 25, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 25, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 25, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 25, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 25, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 25, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+scale_y_continuous(breaks = seq(-10,25,2.5),limits = c(-10,25))






#############################################
##########2010
View(h_c_d10)
h_c_d10 <- h_c_d[h_c_d$año == 2010,]

str(h_c_d10)
Dias13 <- 1:365
h_c_d13 <- data.frame(h_c_d13,Dias13)
h_c_d10 <- data.frame(h_c_d10,Dias10)

###Grafica fina barras
ggplot(data=h_c_d10, aes(x=Dias10))+geom_bar(aes(y=Dyn),colour="dodgerblue2",stat='identity',size=1)+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n (2010) 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 113, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 113, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 113, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 113, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 113, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 113, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 113, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 113, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 113, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 113, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 113, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 113, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+coord_cartesian(ylim = c(84,113))+scale_y_continuous(breaks = seq(84,113,4))

########Comparacion 2010

h_c_dc10 <- data.frame(dat_men[-60,],h_c_d10)


ggplot(data=h_c_dc10, aes(x=Dias10))+geom_bar(aes(y=Dyn,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=Dyn.1,fill="2010"),stat='identity',size=1,alpha=0.5)+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n Histórico vs 2010 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 111, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 111, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 111, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 111, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 111, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 111, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 111, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 111, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 111, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 111, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 111, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 111, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+coord_cartesian(ylim = c(84,111))+scale_y_continuous(breaks = seq(84,111,4))


####
h_c_d13 <- data.frame(h_c_d13,Dias13)

h_c_dc13 <- data.frame(dat_men[-60,],h_c_d13)


ggplot(data=h_c_dc13, aes(x=Dias13))+geom_bar(aes(y=Dyn,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=Dyn.1,fill="2013"),stat='identity',size=1,alpha=0.5)+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n Histórico vs 2013 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 105, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 105, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 105, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 105, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 105, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 105, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 105, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 105, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 105, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 105, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 105, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 105, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+coord_cartesian(ylim = c(75,105))+scale_y_continuous(breaks = seq(80,105,4))

############3
h_c_dc10 <- data.frame(dat_men[-60,],h_c_d10)
anom <- h_c_dc13$Dyn.1- h_c_dc13$Dyn
Dias <- 1:365

anom<- data.frame(anom,Dias)

ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),stat='identity',size=1,color="dodgerblue2")+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n Anomalía 2013 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill=" ")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 2.5, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 2.5, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 2.5, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 2.5, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 2.5, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 2.5, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 2.5, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 2.5, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 2.5, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 2.5, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 2.5, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 2.5, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+scale_y_continuous(breaks = seq(-22.5,10,2.5),limits=c(-22.5,2.5))














#Anomalia 2010

h_c_dc10 <- data.frame(dat_men[-60,],h_c_d10)
anom <- h_c_dc10$Dyn.1- h_c_dc10$Dyn
Dias <- 1:365

anom<- data.frame(anom,Dias)

ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),stat='identity',size=1,color="dodgerblue2")+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n Anomalía 2010 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill=" ")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 10, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 10, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 10, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 10, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 10, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 10, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 10, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 10, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 10, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 10, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 10, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 10, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+scale_y_continuous(breaks = seq(-12.5,10,2.5),limits=c(-11,10))#+ scale_y_continuous(limits = c(0, 110))

###############2022

View(h_c_d22)

str(h_c_d22)

Dias22 <- 1:366

h_c_d22 <- data.frame(h_c_d22,Dias22)

###Grafica fina barras
ggplot(data=h_c_d22, aes(x=Dias22))+geom_bar(aes(y=Dyn),colour="dodgerblue2",stat='identity',size=1)+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n (2022) 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 91, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 91, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 91, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 91, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 91, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 91, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 91, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 91, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 91, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 91, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 91, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 91, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+coord_cartesian(ylim = c(76,91))+scale_y_continuous(breaks = seq(76,92,2))

########################3
Comparacion


########Comparacion 2022
str(h_c_d22)
h_c_dcom22<- h_c_d22
str(men)
str(m22)
men <- dat_men[-60,]
m22 <-c(h_c_dc22$Dyn.1,rep(NA,151))
dias <- 1:365
compa22<- data.frame(men,m22,dias)

ggplot(data=compa22, aes(x=Dias))+geom_bar(aes(y=Dyn,fill = "Histórico"),stat='identity',size=1,alpha=0.5)+geom_bar(aes(y=m22,fill="2022"),stat='identity',size=1,alpha=0.5)+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n Histórico vs 2022 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 106, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 106, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 106, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 106, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 106, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 106, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 106, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 106, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 106, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 106, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 106, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 106, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+coord_cartesian(ylim = c(75,106))+scale_y_continuous(breaks = seq(8,106,4))



#Anomalia 2022

dat_men22 <- dat_men[-60,]
h_c_dc22 <- data.frame(dat_men22[1:214,],h_c_d22)
anom <- h_c_dc22$Dyn.1- h_c_dc22$Dyn

Dias <- 1:214
View(anom)
anom<- data.frame(anom,Dias)

ggplot(data=anom, aes(x=Dias))+geom_bar(aes(y=anom),stat='identity',size=1,color="dodgerblue2")+ylab("Altura dinámica")+ggtitle("Altura dinámica Mensual-Diario \n Anomalía 2022 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",fill=" ")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+scale_x_continuous(limit = c(0,366), breaks=c(0,31,60,91,121,152,182,213,244,274,305,335,366))+geom_text(data = NULL, x = 15.5, y = 2, label = "Ene",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 45.5, y = 2, label = "Feb",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 75.5, y = 2, label = "Mar",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 106, y = 2, label = "Abr",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 136.5, y = 2, label = "May",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 167, y = 2, label = "Jun",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 197.5, y = 2, label = "Jul",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 228.5, y = 2, label = "Ago",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 259, y = 2, label = "Sep",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 289.5, y = 2, label = "Oct",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 320, y = 2, label = "Nov",colour="red",size=4,stat = "unique")+geom_text(data = NULL, x = 350.5, y = 2, label = "Dic",colour="red",size=4,stat = "unique")+xlab("Días")+scale_y_continuous(breaks = seq(-22.5,0,2.5),limits=c(-21,2))#+ scale_y_continuous(limits = c(0, 110))

