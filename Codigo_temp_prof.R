##Librería para carga de datos

library(readr)

setwd("C:/Users/Usuario/Desktop/2021-ii/Oceano/Boya")
t <-read.table("temp_data_incompleta.txt",
               header = T,
               sep = "",           
               dec = ".")
t

#Convertimos a dataframe
t <- data.frame(t)
head(t)

#Borramos columnas innecesarias
t <- t[,-c(2,14,15)]

t
#Cambiamos nombres de columnas
names(t)<- c("Fecha","T1","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13")

str(t)

T2=rep(NA,10673)
T3=rep(NA,10673)
str(t)


t <- data.frame(t[,1:2],T2,T3,t[,3:12])
str(t)
#############################################
#Inclusion de datos extraidos
t2 <-read.table("temp_datos_faltantes.txt",
               header = F,
               sep = "",           
               dec = ".")

#Eliminacion de columnas inutiles
t2<- t2[,-c(2,16,17)]
#Cambio de nombre de columnas
names(t2)<- c("Fecha","T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13")


#Buscamos en donde esta el corte de los datos extraidos
which(t$Fecha == "26/09/2005")

#Separamos los datos en dos partes
tc1 <-t[1:6188,]


tc2 <-t[6189:nrow(t),]


###Ahora incluimos los datos extraidos y corregidos entre los dos cortes
t <- rbind(tc1,t2,tc2)
t <- data.frame(t)
str(t)
#Establecemos columna de fecha
t[,1] <- as.Date(t[,1],format="%d/%m/%Y")
#Vemos si la columna es de tipo fecha
str(t)
#LIbrería para manipular datos
library(tidyverse)

#Completado epico de fechas y datos Xd
t_c <- t %>% 
  complete(Fecha = seq.Date(min(Fecha),max(Fecha),by="day"))

str(t_c)
#Convirtiendo a dataframe(mejor uso)
t_c <- data.frame(t_c)

#Cmabiando la data nula a NA
t_c[t_c ==  -9.999] <- NA

View(t_c)
########
ini=NULL
for (k in 2:ncol(t_c)){
  min<- min(t_c[,k],na.rm=T)
  max <- max(t_c[,k],na.rm=T)
  sd <- sd(t_c[,k],na.rm=T)
  prom <- mean(t_c[,k],na.rm=T)
  ag <- rbind(min,max,sd,prom)
  ini <- cbind(ini,ag)}

ini <- data.frame(ini)
names(ini) <-c("T0","T10","T13","T20","T40","T60","T80","T100","T120","T140","T180","T300","T500")
#write.csv(t_c, "temp_completo.csv")
#Grafica basica
plot(t_c[,c(1,2)])


#Promedio histórico
#Extraccion de dias meses y años
dia <- as.numeric(format(t_c$Fecha,'%d'))
mes <- as.numeric(format(t_c$Fecha,'%m'))
año <- as.numeric(format(t_c$Fecha,'%Y'))


############################3
#Union de vectores al dataframe principal
t_c_d <- data.frame(t_c,dia,mes,año)
str(t_c_d)

#Eliminando datos en estudio
t_c_d22 <- t_c_d[t_c_d$año == 2022,]
t_c_d98 <- t_c_d[t_c_d$año == 1998,]
t_c_d10 <- t_c_d[t_c_d$año == 2010,]

#####################experimento
t_c_d97 <- t_c_d[t_c_d$año == 1997,]

t_c_d <- t_c_d[t_c_d$año <=2019,]
#######################
#####Historico
t_c_d


q=NULL
for (j in 1:12){
  for (i in 1:31){
    med <- t_c_d$VWND[t_c_d$mes== j & t_c_d$dia ==i]
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

######################Mejor forma

hist <- data.frame(T1 = numeric(),
                      T2 = numeric(),
                      T3= numeric(),
                      T4= numeric(),
                      T5= numeric(),
                      T6= numeric(),
                      T7= numeric(),
                      T8= numeric(),
                      T9= numeric(),
                      T10= numeric(),
                      T11= numeric(),
                      T12= numeric(),
                      T13= numeric())

##########################Promedios historicos 
#Bucle doble que recorrera el dataframe y colocara la media en clada fila- columna
a <- 1
for (j in 2:14){
  for (i in 1:12){
    for (k in 1:31){
      datos <-t_c_d[,j][t_c_d$mes== i & t_c_d$dia ==k]
      hist[a,j-1] <- mean(na.omit(datos))
    }
  }
  a <- a+1
}
hist
str(dataset)

View(t_c_d_his)
######################No sirve

t_c_d_his=t_c_d %>%
  group_by(month = month(Fecha), day = day(Fecha)) %>%
  summarise_if(is.numeric, mean , na.rm=TRUE)












###################################
###################################
###################################
#Creando data.frame vacio donde se llenaran los promedios historicos
dataset <- data.frame(T1 = numeric(),
                      T2 = numeric(),
                      T3= numeric(),
                      T4= numeric(),
                      T5= numeric(),
                      T6= numeric(),
                      T7= numeric(),
                      T8= numeric(),
                      T9= numeric(),
                      T10= numeric(),
                      T11= numeric(),
                      T12= numeric(),
                      T13= numeric())

##########################Promedios historicos 
#Bucle doble que recorrera el dataframe y colocara la media en clada fila- columna
for (j in 2:14){
  for (i in 1:12){
    datos <-t_c_d[,j][t_c_d$mes== i]
    dataset[i,j-1] <- mean(na.omit(datos))
  }
}

dataset 
str(dataset)

#Cambiando nombres a columnas
names(dataset)<- c("T0","T10","T13","T20","T40","T60","T80","T100","T120","T140","T180","T300","T500")
###Otra forma, se tendria que hacer uno por uno o con un bucle, pero no es conveniente
#t_c$Year<-format(t_c$Fecha,format="%y")
#t_c
#aggregate(T1~Month,t_c,mean)

######
#Creando vector de meses
Meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

###Union de meses y promedios
t_fin <- data.frame(Meses,dataset)

write.csv(t_fin, "temp_prom_historico.csv")
#Pivoteo para agrupacion de datos
t_fin <- t_fin %>% 
  pivot_longer(!Meses, names_to = "Profundidad", values_to = "Temperatura")

#Extracción de T
t_fin$Profundidad <- gsub('T', '', t_fin$Profundidad, fixed = T)
#Conversión a numerico de la columna profundidad
t_fin$Profundidad<- as.numeric(t_fin$Profundidad)

#Conversión a factor de la columna Meses
t_fin$Meses <- factor(t_fin$Meses,levels=Meses)

str(t_fin)

#Librería para escala de colores
library(RColorBrewer)

#Grafica fina en ggplot(tautocrona)
as<-ggplot(data=t_fin, aes(x=Temperatura,y=Profundidad,group=Meses,color=Meses))+geom_point()+ylim(500,0)+geom_path(stat = "identity")+ylab("Profundidad (m)")+xlab("Temperatura (ºC)")+
  theme_light()+ggtitle("Promedio Histórico Temperatura Subsuperficial \n (1988 -2019)") +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(caption="Fuente: Elaboración propia con datos de la NOAA",color="Meses:")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5),
                                   face="bold.italic",
                                   vjust=0.1, 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))+guides(color = guide_legend(override.aes = list(size = 3)))+scale_color_brewer(palette='Paired') 

#Grafica interactiva
library(plotly)

ggplotly(as)



