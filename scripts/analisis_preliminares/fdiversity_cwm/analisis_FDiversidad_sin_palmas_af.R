rm(list = ls())

# Objetivo ----------------------------------------------------------------
#Calcular los indices de diversidad funcional FDis, FDiv, FEve y CWM para 
#todas las 127 parcelas del norte de Costa Rica exceptuando las especies 
#de palmas y el rasgo funiconal area foliar, esto mediante  el paquete FD. 

#Para el calculo de los indices se siguio el libro Functional and 
#Phylogenetic Ecology in R. pp80

#Paquetes
library(FD)
library(tidyverse)

#Cargar datos
source("scripts/data_cleaning/data_cleaning_for_loops.R")
xy_plot <- read.csv("data/raw/data_posicion_parcelas.csv")

#Eliminar columnas
xy_plot <- xy_plot %>% 
  select(-c("CRTM_90_X","CRTM_90_Y"))


# remover data que no se va a utilizar ------------------------------------
rm(dabund_clean,dabund_relativa,deff_clean,dabund_clean_sinpalmas)

# Cleaning data sets ------------------------------------------------------
#Las especies en abundancias y rasgos tienen que tener el mismo orden para
# que la funcion dbFD corra

#Ver que las especies sean las mismas en ambos data sets
n1 <- data.frame(colnames(dabund_relativa_sinpalmas))
colnames(n1) <- "especie" 

n2 <- data.frame(row.names(deff_clean_sinpalmas))
colnames(n2) <- "especie" 

anti_join(n1,n2, by="especie")

#Ordenando los nombres

deff_clean_sinpalmas <- deff_clean_sinpalmas[order(row.names(deff_clean_sinpalmas)),]

dabund_relativa_sinpalmas <- dabund_relativa_sinpalmas[,order(colnames(dabund_relativa_sinpalmas))]

# Indices de diversidad ---------------------------------------------------

#Calcular indices de diversidad

indices_sinpalmas <- dbFD(deff_clean_sinpalmas[colnames(dabund_relativa_sinpalmas),],
                dabund_relativa_sinpalmas, w.abun = T,stand.x = T)


(fdiv <- data.frame(indices_sinpalmas$FDiv))
fdiv$plot <- row.names(fdiv)
colnames(fdiv) <- c("fdiv","plot")
rownames(fdiv) <- c()
fdiv

(feve <- data.frame(indices_sinpalmas$FEve))
feve$plot <- row.names(feve)
colnames(feve) <- c("feve","plot")
rownames(feve) <- c()
feve

(fdis <- data.frame(indices_sinpalmas$FDis))
fdis$plot <- row.names(fdis)
colnames(fdis) <- c("fdis","plot")
rownames(fdis) <- c()
fdis

data_indices_sinpalmas <- cbind.data.frame(fdiv[,1],feve[,1],fdis[,1],fdiv[,2])

colnames(data_indices_sinpalmas)<- c("fdiv","feve","fdis","plot")

data_indices_sinpalmas <- left_join(xy_plot, data_indices_sinpalmas, by="plot")

#Community Weight means
(cwm_sinpalmas <- data.frame(indices_sinpalmas$CWM))

cwm_sinpalmas$plot <- row.names(cwm_sinpalmas)
rownames(cwm_sinpalmas) <- c()
cwm_sinpalmas
colnames(cwm_sinpalmas) <- c("cwm_afe","cwm_cfms","cwm_dm","cwm_n","cwm_p","plot")
data_cwm_sinpalmas <- (left_join(xy_plot,cwm_sinpalmas,by="plot"))

#Eliminando data sets
rm(dabund_relativa_sinpalmas,indices_sinpalmas,cwm_sinpalmas,
   deff_clean_sinpalmas,fdis,feve,fdiv,n1,n2,xy_plot)

#Salvando datos
write.csv(data_cwm_sinpalmas,
          "C:data/clean/data_cwm_sinpalmas_coord.csv")
write.csv(data_indices_sinpalmas,
          "C:data/clean/data_fdiversity_sinpalmas_coord.csv")
