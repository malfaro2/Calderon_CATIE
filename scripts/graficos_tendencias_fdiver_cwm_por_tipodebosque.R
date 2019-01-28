rm(list = ls())


# Objetivo ----------------------------------------------------------------
#Mostrar las tendencias de los indices fdis, feve, fdiv y cwm segun tipo de
#bosque

#Cargar data sin palmas

data_fdiver_sinpalmas <- read.csv("data/clean/data_fdiversity_sinpalmas_coord.csv") 
data_cwm_sinplamas <- read.csv("data/clean/data_cwm_sinpalmas_coord.csv") 

#Ordenar factores para ver la tendencia en el ggplot
data_fdiver_sinpalmas$forest_type_ord <-  factor(data_fdiver_sinpalmas$forest_type, 
                                                 levels=c("P. macroloba","Q. paraensis","Foothills"))


#fdis
ggplot(data=data_fdiver_sinpalmas,aes(x=plot, y=fdis, color=forest_type))+
  geom_point()+
  facet_grid(~forest_type_ord)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#feve
ggplot(data=data_fdiver_sinpalmas,aes(x=plot, y=feve, color=forest_type))+
  geom_point()+
  facet_grid(~forest_type_ord)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#fdiv
ggplot(data=data_fdiver_sinpalmas,aes(x=plot, y=fdiv, color=forest_type))+
  geom_point()+
  facet_grid(~forest_type_ord)+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
