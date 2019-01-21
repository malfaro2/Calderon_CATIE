rm(list = ls())

#Cargar paquetes
library(vegan)
library(dplyr)


# Cargar data -------------------------------------------------------------
source("scripts/data_cleaning_for_loops.R")
xy_plot <- read.csv("data/raw/data_posicion_parcelas.csv")

#Ekiminar columnas
xy_plot <- xy_plot %>% 
  dplyr::select(-c(CRTM_90_X,CRTM_90_Y))

# Remover datos que no van a ser utilizados -------------------------------
rm(dabund_clean,dabund_relativa,deff_clean)



#Abundancia relativa
dabund_relativa_sinpalmas[1,]

#Preliminar 
names(dabund_relativa_sinpalmas[1,dabund_relativa_sinpalmas[1,]>0])

(plot1 <- deff_clean_sinpalmas[names(
  dabund_relativa_sinpalmas[1,dabund_relativa_sinpalmas[1,]>0]),])

#Poner en una lista 

#Crear una lista por cada parcela
list<-split(dabund_relativa_sinpalmas, row.names(dabund_relativa_sinpalmas))

#Cantidad de parcelas 127
length(list)
#str(list)

#Crear una lista vacia
sp_cwm_eff_sinpalmas<-list()

#Loop 
for (i in seq_along(names(list))) {
  
  #Rasgos funcioanles de las especies presentes de cada parcela con
  #una abundancia mayor a 0 
  traits<- deff_clean_sinpalmas[names(list[[i]][,list[[i]]>0]),]
  
  # calcular CWM por abundancia relativa en cada parcela 
  cwm <- apply(traits, 2, weighted.mean)
  
  #Nombres de cada parcela
  plot<- unique(row.names(list[[i]][,list[[i]]>0]))
  
  #Nombres de los traits
  traits<- unique(colnames(deff_clean_sinpalmas))
  
  #datos con parcela y composicion de especies 
  data<-data.frame(cwm,plot,traits)
  
  #Resultado
  sp_cwm_eff_sinpalmas[[i]] <- data
  #print(sp_comp)
}

#convertir lista a data.frame
data_cwm_sinpalmas<- plyr::ldply (sp_cwm_eff_sinpalmas, data.frame)
head(data_cwm_sinpalmas)
head(xy_plot)

#agregar las coordenadas a los datos
data_cwm_coord_sinpalmas<-left_join(data_cwm_sinpalmas,xy_plot, by="plot")
dim(data_cwm_coord_sinpalmas)
head(data_cwm_coord_sinpalmas)

#Remover data sets que no son utiles
rm(dabund_clean_sinpalmas,dabund_relativa_sinpalmas,data,data_cwm_sinpalmas,
   deff_clean_sinpalmas,list,plot1,sp_cwm_eff_sinpalmas,xy_plot)

#Salvar data set
write.csv(data_cwm_coord_sinpalmas,
"C:data/clean/data_cwm_coord_sinpalmas.csv")



