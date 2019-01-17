rm(list = ls())

# Objetivo ----------------------------------------------------------------
#Mostrar los valores de CWM a traves del paisaje

#Cargar paquetes
library(tidyverse)
library(rgdal)
library(raster)
library(ggsn)
library(rworldmap)
library(rworldxtra)
library(ggthemes)

#Cargar data
data_cwm <- read.csv("data/clean/data_cwm_coord.csv") 
head(data_cwm)  
dim(data_cwm)  
str(data_cwm)  


#Mapa preliminar de las parcelas 
(prelim_map2 <- ggplot(data_cwm,
                       aes(x=longitude,y=latitude,
                           colour=forest_type))+
    geom_point(shape="square")+
    theme_bw()
)


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

###Agregar parcelas al mapa de Costa Rica

(mapa<-ggplot()+
    
    #Mapa de Costa Rica
    geom_polygon(data = costa_rica,
                 aes(x=long,y=lat,group=group),
                 fill="grey",colour="black")+
    
    #Se agregan los parcelas
    geom_point(data=data_cwm,
               aes(x=longitude,y=latitude,
                   colour=forest_type))+
    #Mapa
    theme_bw()+
    coord_quickmap()+
    ylim(8,11.5)+
    xlim(-86,-82.5)+
   ylab("latitude")+
   xlab("longitude")+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
  theme(axis.text = element_text(size = 15))+
  theme(axis.title = element_text(size = 30)) +labs(size =45)+ 
   theme(legend.title = element_text(size = 45))+
  theme(legend.text = element_text(size = 30))
)


#Mapa cmw area foliar

colnames(data_cwm)
str(data_cwm)

cwm_af<-data_cwm %>% 
  filter(traits %in% "af")

dim(cwm_af)
head(cwm_af)
summary(cwm_af)
mid<-522324

(mapa<-ggplot()+
    
    #Mapa de Costa Rica
    geom_polygon(data = costa_rica,
                aes(x=long,y=lat,group=group),
                fill="grey",colour="black")+
    
    #Se agregan los parcelas
    geom_point(data=cwm_af,
               aes(x=longitude,y=latitude,
                   colour=cwm))+
    #Mapa
    theme_bw()+
    #coord_quickmap()+
    ylim(8,11.5)+
    xlim(-86,-82.5)+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    labs(colour = "CWM", shape = "Tipo de Bosque")+
    scale_color_gradient2(low="blue", mid="green", high="red")
  
  )

