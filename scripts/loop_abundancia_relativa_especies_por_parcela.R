rm(list = ls())

# Cargar script con datos limpios -----------------------------------------
source("scripts/data_cleaning_for_loops.R")


# Loop abundancia relativa ------------------------------------------------

#Este loop funciona para conocer la abundancia relativa cada de especie en 
#cada una de las 127 parcelas. 
#Data:parcelas en las filas y las especies en las columnas

#Paquetes
library(vegan)

#Covertir cantidad de individuos por especie a abundancia relativa
dabund_clean
dabund_relativa<-decostand(dabund_clean,method = "total",MARGIN = 1)
dabund_relativa

# 1-Separar el ser de datos en una lista por parcela
lista<-split(dabund_relativa, row.names(dabund_relativa))

# 2-Crear una lista para almacenar la abundancia relativa de especies de cada 
# parcela
sp_abundrelat<-list()

#Loop
for (i in seq_along(names(lista))) {
  
  #especies de cada parcela con una abundancia relativa mayor a 0
  abundrelat<-lista[[i]][,lista[[i]]>0]
  
  #Nombre de cada parcela
  plot<- row.names(lista[[i]][,lista[[i]]>0])
  
  #datos con parcela y abundancia relativa de especies
  data<-data.frame(abundrelat,plot)
  
  #Resultado
  sp_abundrelat[[i]] <- data
  #print(sp_comp)
}

#Prueba
#Todos los codigos deberian dar el mismo resultado
sp_abundrelat[12]
sum(lista[[12]][,lista[[12]]>0])


lista[[12]][,lista[[12]]>0]
sum(lista[[12]][,lista[[12]]>0])

#Abundancia relativa de la parcela 12
dabund_relativa[12,dabund_relativa[12,]>0]
rowSums(dabund_relativa[12,dabund_relativa[12,]>0])
