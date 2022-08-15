##Mandejo de datos geográficos
library(sp)
library(sf)
library(purrr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(readxl)

#Libreria para ploteo de mapas oceanograficos
library(ggOceanMaps)

basemap("barentssea",limits = c(-170,-30, -30, 30), bathymetry = T)+
ylab("Latitud (º)")+xlab("Longitud (º)")+ggtitle("Localización Boya 9ºN 140ºW")+
labs(caption="Fuente de datos: Paquete ggOceanMaps(R) \n Procesamiento: Elaboración propia")+
theme(plot.title = element_text(hjust = 0.5))+guides(fill = guide_legend(title = "Profundidad (m)"))+
geom_vline(xintercept = -140,colour="red")+geom_hline(yintercept = 9,colour="red")+
geom_point(aes(x = -140, y = 9),shape=9, color = "red",size=4)+annotation_scale(location = "br")+
annotation_north_arrow(location = "tr", which_north = "true")+coord_sf(xlim=c(-170, -30), ylim=c(-30, 30))
