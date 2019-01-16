rm(list = ls())

#Cargar paquetes
library(vegan)


# Cargar data -------------------------------------------------------------
source("scripts/data_cleaning_for_loops.R")


#Abundancia relativa
dabund_relativa[1,]

#Preliminar 
names(dabund_relativa[1,dabund_relativa[1,]>0])
(plot1 <- deff_clean[names(dabund_relativa[1,dabund_relativa[1,]>0]),])


#Crear una lista por cada parcela
list<-split(dabund_relativa, row.names(dabund_relativa))
length(list)

#Crear una lista vacia
sp_effecttraits<-list()

#Loop 
for (i in seq_along(names(list))) {
  
  #Rasgos funcioanles de las especies presentes de cada parcela con
  #una abundancia mayor a 0 
  traits<- deff_clean[names(list[[i]][,list[[i]]>0]),]
  
  # calcular CWM por abundancia relativa en cada parcela 
  cwm <- apply(traits, 2, weighted.mean)
  
  #Nombres de cada parcela
  plot<- unique(row.names(list[[i]][,list[[i]]>0]))
  
  #datos con parcela y composicion de especies 
  data<-data.frame(cwm,plot)
  
  #Resultado
  sp_effecttraits[[i]] <- data
  #print(sp_comp)
}


#Prueba 











