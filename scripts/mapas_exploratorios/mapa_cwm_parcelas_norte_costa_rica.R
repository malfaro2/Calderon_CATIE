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

(mapa_parcelas<-ggplot()+
    
    #Mapa de Costa Rica
    geom_polygon(data = costa_rica,
                 aes(x=long,y=lat,group=group),
                 fill="grey",colour="black")+
    
    #Se agregan los parcelas
    geom_point(data=data_cwm,alpha=0.6,
               position = position_jitter(width=0.05, height=0.01),
               aes(x=longitude,y=latitude,
                   colour=forest_type))+
    #Mapa
   theme_bw()+
   coord_quickmap()+
   ylim(8,11.3)+
   xlim(-86,-82.5)+
   guides(colour=guide_legend(tittle="Tipo de Bosque"))

+labs(colour = "Tipo de bosque")
)


# Mapa base de costa rica -------------------------------------------------

mapa_costarica <- ggplot()+
  geom_polygon(data = costa_rica,
               aes(x=long,y=lat,group=group),
               fill="grey",colour="black") 


# Mapa cmw area foliar ----------------------------------------------------

(mapa_af<- mapa_costarica +
    
    #Se agregan los parcelas
    geom_point(data=data_cwm,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude,
                   
                   colour=cwm_af,shape=forest_type))+
    #Mapa
    theme_bw()+
    coord_quickmap()+
    ylim(8,11.3)+
    xlim(-86,-82.5)+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    labs(colour = "CWM", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "CWM Area Foliar")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank") 
    
))


# Mapa cmw area foliar especifica -----------------------------------------

(mapa_afe<-  mapa_costarica+
    
    #Se agregan los parcelas
    geom_point(data=data_cwm,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude,
                   
                   colour=cwm_afe,shape=forest_type))+
    #Mapa
    theme_bw()+
    coord_quickmap()+
    ylim(8,11.3)+
    xlim(-86,-82.5)+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    labs(colour = "CWM", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "CWM Area Foliar especifica")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank") 
    ))

# Mapa cmw cfms -----------------------------------------------------------

(mapa_cfms<- mapa_costarica+
    
    #Se agregan los parcelas
    geom_point(data=data_cwm,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude,
                   
                   colour=cwm_cfms,shape=forest_type))+
    #Mapa
    theme_bw()+
    coord_quickmap()+
    ylim(8,11.3)+
    xlim(-86,-82.5)+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    labs(colour = "CWM", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow",  high="red")+
    labs(colour = "CWM contenido foliar de materia seca")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
          panel.grid.minor = element_line(linetype = "blank") 
))



# Mapa cmw densidad de madera ---------------------------------------------

(mapa_dm <- mapa_costarica+
    
    #Se agregan los parcelas
    geom_point(data=data_cwm,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude,
                   
                   colour=cwm_dm,shape=forest_type))+
    #Mapa
    theme_bw()+
    coord_quickmap()+
    ylim(8,11.3)+
    xlim(-86,-82.5)+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    labs(colour = "CWM", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow",  high="red")+
    labs(colour = "CWM densidad de madera")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
          panel.grid.minor = element_line(linetype = "blank") 
    ))


# Mapa cmw Nitrogeno ------------------------------------------------------

(mapa_n<- mapa_costarica+
    
    #Se agregan los parcelas
    geom_point(data=data_cwm,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude,
                   
                   colour=cwm_n,shape=forest_type
               ))+
    #Mapa
    theme_bw()+
    coord_quickmap()+
    ylim(8,11.3)+
    xlim(-86,-82.5)+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    labs(colour = "CWM", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow",  high="red")+
    labs(colour = "CWM Nitrogeno foliar")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
          panel.grid.minor = element_line(linetype = "blank") 
          ))

# Mapa cmw Fosforo --------------------------------------------------------

(mapa_p<- mapa_costarica+
    
    #Se agregan los parcelas
    geom_point(data=data_cwm,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude,
                   
                   colour=cwm_p,shape=forest_type
               ))+
    #Mapa
    theme_bw()+
    coord_quickmap()+
    ylim(8,11.3)+
    xlim(-86,-82.5)+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    labs(colour = "CWM", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow",  high="red")+
    labs(colour = "CWM Fosforo foliar")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
          panel.grid.minor = element_line(linetype = "blank") 
          ))
