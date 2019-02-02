rm(list = ls())

# Objetivo ----------------------------------------------------------------
#Mostrar los valores de uniqueness y redundancy a traves del paisaje sin palmas

#Cargar paquetes
library(tidyverse)
library(rgdal)
library(raster)
library(ggsn)
library(rworldmap)
library(rworldxtra)
library(ggthemes)

#Cargar data
data_fdiver_sinpalmas <- read.csv("data/clean/resultados_csv/data_redundancy_sinpalmas.csv") 
head(data_fdiver_sinpalmas)  
dim(data_fdiver_sinpalmas)  
str(data_fdiver_sinpalmas)  

## Agregar mapa mundial
world<-getMap(resolution = "high")

##Elegir mapa de Costa Rica
costa_rica <- c("Costa Rica") #, "Panama", "Nicaragua"
costa_rica <- world[world@data$ADMIN %in% costa_rica,]

#Ver si coord. ref es diferente
costa_rica
world


##Cambiando el coord. ref
proj4string(costa_rica)<-proj4string(world)


# Mapas -------------------------------------------------------------------

# Mapa base de Costa Rica -------------------------------------------------

mapacostarica <- ggplot()+
  
  #Mapa de Costa Rica
  geom_polygon(data = costa_rica,
               aes(x=long,y=lat,group=group),
               fill="grey",colour="black")


# Mapa Fdis  ----------------------------------------------------

(mapa_fdis_sinpalmas <-  mapacostarica + 
   
   #Se agregan los parcelas
   geom_point(data=data_fdiver_sinpalmas,
              alpha=0.6,position = position_jitter(width=0.04, height=0.04),
              aes(x=longitude,y=latitude,
                  colour=fdis,shape=forest_type))+
   #Mapa
   theme_bw()+
   coord_quickmap()+
   ylim(8,11.3)+
   xlim(-86,-82.5)+
   guides(colour=guide_legend(tittle="Tipo de Bosque"))+
   labs(colour = "FDis sin palmas", shape = "Tipo de Bosque")+
   scale_color_gradient(low="yellow", high="red")+
   labs(colour = "FDis sin palmas")+
   theme(panel.grid.major = element_line(linetype = "blank"), 
         panel.grid.minor = element_line(linetype = "blank") 
         
   ))

# Mapa FDiv ---------------------------------------------------------------

(mapa_fdiv_sinpalmas <- mapacostarica  +
   
   #Se agregan los parcelas
   geom_point(data=data_fdiver_sinpalmas,
              alpha=0.6,position = position_jitter(width=0.04, height=0.04),
              aes(x=longitude,y=latitude,
                  colour=fdiv,shape=forest_type))+
   #Mapa
   theme_bw()+
   coord_quickmap()+
   ylim(8,11.3)+
   xlim(-86,-82.5)+
   guides(colour=guide_legend(tittle="Tipo de Bosque"))+
   labs(colour = "FDiv sin palmas", shape = "Tipo de Bosque")+
   scale_color_gradient(low="yellow", high="red")+
   labs(colour = "FDiv sin palmas")+
   theme(panel.grid.major = element_line(linetype = "blank"), 
         panel.grid.minor = element_line(linetype = "blank") 
   ))


# Mapa Feve ---------------------------------------------------------------

(mapa_feve_sinpalmas <- mapacostarica  +
   
   #Se agregan los parcelas
   geom_point(data=data_fdiver_sinpalmas,
              alpha=0.6,position = position_jitter(width=0.04, height=0.04),
              aes(x=longitude,y=latitude,
                  colour=feve,shape=forest_type))+
   #Mapa
   theme_bw()+
   coord_quickmap()+
   ylim(8,11.3)+
   xlim(-86,-82.5)+
   guides(colour=guide_legend(tittle="Tipo de Bosque"))+
   labs(colour = "FEve sin palmas", shape = "Tipo de Bosque")+
   scale_color_gradient(low="yellow",  high="red")+
   labs(colour = "FEve sin palmas")+
   theme(panel.grid.major = element_line(linetype = "blank"), 
         panel.grid.minor = element_line(linetype = "blank") 
   ))

