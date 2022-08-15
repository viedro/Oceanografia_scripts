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
setwd("C:/Users/USUARIO/Desktop/2021-ii/Oceano/Boya")
library(ncdf4)
library(openxlsx)
n=nc_open("iso9n140w_dy.cdf")
TEMP=ncvar_get(n, varid="ISO_6")
depth=ncvar_get(n, varid="depth")

time=ncvar_get(n,varid="time")
FECHA=as.Date("1988-05-30") + 1:length(time)
TEMP<- as.data.frame(TEMP)
TEMP=cbind(FECHA,TEMP)
##################
año <- as.numeric(format(TEMP$FECHA,'%Y'))
TEMP <- data.frame(TEMP,año)

T22 <- TEMP[TEMP$año == 2022,]
T13 <- TEMP[TEMP$año == 2013,]
T10 <- TEMP[TEMP$año == 2022,]
T11 <- TEMP[TEMP$año == 2013,]
T97 <- TEMP[TEMP$año == 2022,]
T98 <- TEMP[TEMP$año == 2013,]
TH <- TEMP[TEMP$año <= 2019,]

TH <- TH[,-3]

################3
#pivot table
TH=TH %>%
  group_by(month = month(FECHA), day = day(FECHA)) %>%
  summarise_if(is.numeric, mean , na.rm=TRUE)


plot(TH)

str(TH)

TH <- TH[,c(-1,-2)]

TH <- data.frame(1:366,TH)
colnames(TH) <- c("Dia","TH")

ggplot(data=TH, aes(x=Dia))+geom_line(aes(y=TH),colour="dodgerblue2")+ylab("Profundidad (m)")+ggtitle("Isoterma 20ºC Promedio Histórico Mensual-Diario \n (1988-2019) 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+guides(color = guide_legend(override.aes = list(size = 4)))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+xlab("Meses")+
  scale_x_continuous(breaks=c(15.5,45.5,75.5,106,136.5,167,197.5,228.5,259,289.5,320,350.5),labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","sep","oct","Nov","Dic"))+coord_cartesian(ylim = c(85, 50))+scale_y_continuous(breaks = seq(50,100,5))

###############
sub13= subset(TEMP, FECHA>="2013-01-01" & FECHA<="2013-12-31" )
sub[1]= NULL

sub913_c <- data.frame(TH[-60,],sub13)

str(sub97_c)

+geom_bar(aes(y=Dyn,fill = "Histórico"),stat='identity',size=1,alpha=0.5)

ggplot(data=sub97_c, aes(x=Dia))+geom_line(aes(y=TH,col="Histórico"),stat="identity")+geom_line(aes(y=TEMP,col="2013"),stat="identity")+ylab("Profundidad (m)")+ggtitle("Isoterma 20ºC Histórico vs 2013 Mensual-Diario \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",col="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+guides(color = guide_legend(override.aes = list(size = 4)))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+xlab("Meses")+ylim(rev(range(TH$TH)))+
  scale_x_continuous(breaks=c(15.5,45.5,75.5,106,136.5,167,197.5,228.5,259,289.5,320,350.5),labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","sep","oct","Nov","Dic"))+coord_cartesian(ylim = c(90, 30))+scale_y_continuous(breaks = seq(30,90,5))


##################2010
sub10= subset(TEMP, FECHA>="2010-01-01" & FECHA<="2010-12-31" )
sub10[1]= NULL

sub10_c <- data.frame(TH[-60,],sub10)

str(sub10_c)


ggplot(data=sub10_c, aes(x=Dia))+geom_line(aes(y=TH,col="Histórico"),stat="identity")+geom_line(aes(y=TEMP,col="2010"),stat="identity")+ylab("Profundidad (m)")+ggtitle("Isoterma 20ºC Histórico vs 2010 Mensual-Diario \n 9ºN 140ºW")+theme_light()+labs(caption="Fuente: Elaboración propia con datos de la NOAA",col="")+theme (plot.title = element_text(family="Comic Sans MS", size=rel(1.5),vjust=0.1,face="bold.italic",color="gray48",lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48",size=rel(1.3))) +theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+theme(plot.title = element_text(hjust = 0.5))+guides(color = guide_legend(override.aes = list(size = 4)))+geom_vline(xintercept = 31,colour="darkgreen")+geom_vline(xintercept = 60,colour="darkgreen")+geom_vline(xintercept = 91,colour="darkgreen")+geom_vline(xintercept = 121,colour="darkgreen")+geom_vline(xintercept = 152,colour="darkgreen")+geom_vline(xintercept = 182,colour="darkgreen")+geom_vline(xintercept = 213,colour="darkgreen")+geom_vline(xintercept = 244,colour="darkgreen")+geom_vline(xintercept = 274,colour="darkgreen")+geom_vline(xintercept = 305,colour="darkgreen")+geom_vline(xintercept = 335,colour="darkgreen")+xlab("Meses")+ylim(rev(range(TH$TH)))+
  scale_x_continuous(breaks=c(15.5,45.5,75.5,106,136.5,167,197.5,228.5,259,289.5,320,350.5),labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","sep","oct","Nov","Dic"))+coord_cartesian(ylim = c(90, 30))+scale_y_continuous(breaks = seq(30,90,5))
