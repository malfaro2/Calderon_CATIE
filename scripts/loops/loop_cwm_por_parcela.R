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

#Abundancia relativa
dabund_relativa[1,]

#Preliminar 
names(dabund_relativa[1, dabund_relativa[1, ] > 0])
(plot1 <- deff_clean[names(dabund_relativa[1,dabund_relativa[1,] > 0]),])


#Poner en una lista 

#Crear una lista por cada parcela
list <- split(dabund_relativa, row.names(dabund_relativa))
length(list)
#str(list)

#Crear una lista vacia
sp_cwm_eff <- list()

traits <- list()
completa <- list()
#Loop

# Loop para traer lista con parcelas y su seleccion de especies con
# abundancia mayor a cero
for (i in seq_along(list)) {
  #Rasgos funcioanles de las especies presentes de cada parcela con
  #una abundancia mayor a 0 
  nombres_parcelas <- names(list[i])
  traits[[nombres_parcelas]] <- deff_clean[names(list[[i]][ , list[[i]] > 0]),]
  
}

# Loop para aplicar funcion de weighted mean en cada uno de las parcelas
for (parcela in seq_along(traits)) {
  a <- weighted.mean(traits[[parcela]], dabund_clean)
  print(a)
}
  
  
   # for (parcela in )
   # weighted.mean(traits, dabund_clean)

   # calcular CWM por abundancia relativa en cada parcela
   # cwm <- apply(traits, 2, (weighted.mean))
   # 
   # #Nombres de cada parcela
   # plot <- unique(row.names(list[[i]][,list[[i]]>0]))
   # 
   # #Nombres de los traits
   # traits <- unique(colnames(deff_clean))
  
   #datos con parcela y composicion de especies
   # data <- data.frame(traits)
  
   #Resultado
   # sp_cwm_eff[[i]] <- data
   #print(sp_comp)
}
sp_cwm_eff
sp_cwm_eff[[1]]


#convertir lista a data.frame
data_cwm <- plyr::ldply(sp_cwm_eff, data.frame)
head(data_cwm)
head(xy_plot)

#agregar las coordenadas a los datos
data_cwm_coord<-left_join(data_cwm,xy_plot)
dim(data_cwm_coord)
head(data_cwm_coord)


#Remover data sets que no son utiles
rm(dabund_clean,dabund_relativa,data,data_cwm,deff_clean,
  list,plot1,sp_cwm_eff,xy_plot)

write.csv(data_cwm_coord,
"C:data/clean/data_cwm_coord.csv")



