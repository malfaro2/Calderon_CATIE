rm(list = ls())

#Cargar paquetes
library(vegan)
library(dplyr)
library(FD)

# Cargar data -------------------------------------------------------------
source("scripts/data_cleaning_for_loops.R")

#Data coordenadas
xy_plot <- read.csv("data/raw/data_posicion_parcelas.csv")

#Eliminar columnas
xy_plot <- xy_plot %>% 
  dplyr::select(-c(CRTM_90_X,CRTM_90_Y))

#Abundancia relativa
dabund_relativa[1,]

#Preliminar 
names(dabund_relativa[1,dabund_relativa[1,]>0])
(plot1 <- deff_clean[names(dabund_relativa[1,dabund_relativa[1,]>0]),])
a<-dbFD(plot1)$FDis
a

#Poner en una lista 
#Crear una lista por cada parcela
list<-split(dabund_relativa, row.names(dabund_relativa))
length(list)
#str(list)

#Crear una lista vacia
plot_fdis_eff<-list()


#Loop Fdis
for (i in seq_along(names(list))) {
  
  #Rasgos funcioanles de las especies presentes de cada parcela con
  #una abundancia mayor a 0 
  fdis<- dbFD(deff_clean[names(list[[i]][,list[[i]]>0]),])$FDis
  
  #Nombres de cada parcela
  plot<- unique(row.names(list[[i]][,list[[i]]>0]))
  
  #datos con parcela y composicion de especies 
  data<-data.frame(plot,fdis)
  
  #Resultado
  plot_fdis_eff[[i]] <- data
}


#convertir lista a data.frame
data_fdis<- plyr::ldply (plot_fdis_eff, data.frame)
head(data_fdis)
head(xy_plot)

#agregar las coordenadas a los datos
data_fdis_coord<-left_join(data_fdis,xy_plot)
dim(data_fdis_coord)
head(data_fdis_coord)


#Remover data sets que no son utiles
rm(dabund_clean,dabund_relativa,data,data_cwm,deff_clean,
  list,plot1,sp_cwm_eff,xy_plot)

#write.csv(data_fdis_coord,"C:data/clean/data_fdis_coord.csv")



