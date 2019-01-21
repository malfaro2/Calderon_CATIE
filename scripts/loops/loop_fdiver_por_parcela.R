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

b <- dbFD(plot1)$FRic
b
#Poner en una lista 
#Crear una lista por cada parcela
list<-split(dabund_relativa, row.names(dabund_relativa))
length(list)
#str(list)

#Crear una lista vacia
plot_fd_eff<-list()


#Loop Functional diversity indices
for (i in seq_along(names(list))) {
  
  #Rasgos funcioanles de las especies presentes de cada parcela con
  #una abundancia mayor a 0 
  fdis <- dbFD(deff_clean[names(list[[i]][,list[[i]]>0]),])$FDis
  feve <- dbFD(deff_clean[names(list[[i]][,list[[i]]>0]),])$FEve
  fric <- dbFD(deff_clean[names(list[[i]][,list[[i]]>0]),])$FRic
  
  #Nombres de cada parcela
  plot<- unique(row.names(list[[i]][,list[[i]]>0]))
  
  #datos con parcela y composicion de especies 
  data<-data.frame(plot,fdis,feve,fric)
  
  #Resultado
  plot_fd_eff[[i]] <- data
}

plot_fd_eff[[1]]

#convertir lista a data.frame
data_fdiv<- plyr::ldply (plot_fd_eff, data.frame)
head(data_fdiv)
head(xy_plot)

#agregar las coordenadas a los datos
data_fdiv_coord<-left_join(data_fdiv,xy_plot)
dim(data_fdiv_coord)
head(data_fdiv_coord)


#Remover data sets que no son utiles
rm(dabund_clean,dabund_relativa,data,data_fdiv,deff_clean,
   list,plot1,plot_fd_eff,xy_plot)


write.csv(data_fdiv_coord,"C:data/clean/data_fdiversity_coord.csv")



