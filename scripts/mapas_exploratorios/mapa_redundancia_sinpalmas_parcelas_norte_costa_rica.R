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
data_redundancy_sinpalmas <- read.csv("data/clean/resultados_csv/data_redundancy_sinpalmas.csv") 
head(data_redundancy_sinpalmas)  
dim(data_redundancy_sinpalmas)  
str(data_redundancy_sinpalmas)  

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


# Mapa redundancy  ----------------------------------------------------

(mapa_redundancy_sinpalmas <-  mapacostarica + 
   
   #Se agregan los parcelas
   geom_point(data=data_redundancy_sinpalmas,
              alpha=0.6,position = position_jitter(width=0.04, height=0.04),
              aes(x=longitude,y=latitude,
                  colour=redundancy,shape=forest_type))+
   #Mapa
   theme_bw()+
   coord_quickmap()+
   ylim(8,11.3)+
   xlim(-86,-82.5)+
   guides(colour=guide_legend(tittle="Tipo de Bosque"))+
   labs(colour = "Redundancia sin palmas", shape = "Tipo de Bosque")+
   scale_color_gradient(low="yellow", high="red")+
   labs(colour = "Redundancia sin palmas")+
   theme(panel.grid.major = element_line(linetype = "blank"), 
         panel.grid.minor = element_line(linetype = "blank") 
         
   ))

# Mapa uniqueness ---------------------------------------------------------------

(mapa_fdiv_sinpalmas <- mapacostarica  +
   
   #Se agregan los parcelas
   geom_point(data=data_redundancy_sinpalmas,
              alpha=0.6,position = position_jitter(width=0.04, height=0.04),
              aes(x=longitude,y=latitude,
                  colour=U,shape=forest_type))+
   #Mapa
   theme_bw()+
   coord_quickmap()+
   ylim(8,11.3)+
   xlim(-86,-82.5)+
   guides(colour=guide_legend(tittle="Tipo de Bosque"))+
   labs(colour = "Uniqueness sin palmas", shape = "Tipo de Bosque")+
   scale_color_gradient(low="yellow", high="red")+
   labs(colour = "Uniqueness sin palmas")+
   theme(panel.grid.major = element_line(linetype = "blank"), 
         panel.grid.minor = element_line(linetype = "blank") 
   ))

# Mapa Rao ---------------------------------------------------------------

(mapa_rao_sinpalmas <- mapacostarica  +
   
   #Se agregan los parcelas
   geom_point(data=data_redundancy_sinpalmas,
              alpha=0.6,position = position_jitter(width=0.04, height=0.04),
              aes(x=longitude,y=latitude,
                  colour=Q,shape=forest_type))+
   #Mapa
   theme_bw()+
   coord_quickmap()+
   ylim(8,11.3)+
   xlim(-86,-82.5)+
   guides(colour=guide_legend(tittle="Tipo de Bosque"))+
   labs(colour = "Rao sin palmas", shape = "Tipo de Bosque")+
   scale_color_gradient(low="yellow", high="red")+
   labs(colour = "Rao sin palmas")+
   theme(panel.grid.major = element_line(linetype = "blank"), 
         panel.grid.minor = element_line(linetype = "blank") 
   ))
