rm(list = ls())

# Objetivo ----------------------------------------------------------------
#Calcular los indices de diversidad funcional FDis, FDiv, FEve y CWM para 
#todas las 127 parcelas del norte de Costa Rica mediante el paquete FD. 

#Para el calculo de los indices se siguio el libro Functional and 
#Phylogenetic Ecology in R. pp80
 
#Paquetes
library(FD)
library(tidyverse)

#Cargar datos
#source("scripts/data_cleaning/data_cleaning_for_loops.R")
#xy_plot <- read.csv("data/raw/data_posicion_parcelas.csv")

#Eliminar columnas
xy_plot <- xy_plot %>% 
  select(-c("CRTM_90_X","CRTM_90_Y"))


# remover data que no se va a utilizar ------------------------------------
rm(dabund_clean_sinpalmas,dabund_relativa_sinpalmas,deff_clean_sinpalmas)


# Cleaning data sets ------------------------------------------------------
#Las especies en abundancias y rasgos tienen que tener el mismo orden para
# que la funcion dbFD corra

#Ver que las especies sean las mismas en ambos data sets
n1 <- data.frame(colnames(dabund_clean))
colnames(n1) <- "especie" 

n2 <- data.frame(row.names(deff_clean))
colnames(n2) <- "especie" 

anti_join(n1,n2, by="especie")


#Ordenando los nombres
deff_clean2 <- deff_clean[order(row.names(deff_clean)),]

dabund_clean2 <- dabund_clean[,order(colnames(dabund_clean))]


# Indices de diversidad ---------------------------------------------------

#Calcular indices de diversidad

indices <- dbFD(deff_clean2[colnames(dabund_relativa),],
     dabund_relativa, w.abun = T,stand.x = T)


(fdiv <- data.frame(indices$FDiv))
fdiv$plot <- row.names(fdiv)
colnames(fdiv) <- c("fdiv","plot")
rownames(fdiv) <- c()
fdiv


(feve <- data.frame(indices$FEve))
feve$plot <- row.names(feve)
colnames(feve) <- c("feve","plot")
rownames(feve) <- c()
feve

(fdis <- data.frame(indices$FDis))
fdis$plot <- row.names(fdis)
colnames(fdis) <- c("fdis","plot")
rownames(fdis) <- c()
fdis


data_indices <- cbind.data.frame(fdiv[,1],feve[,1],fdis[,1],fdiv[,2])


colnames(data_indices)<- c("fdiv","feve","fdis","plot")
data_indices <- left_join(xy_plot, data_indices, by="plot")


#Community Weight means
(cwm <- data.frame(indices$CWM))
cwm$plot <- row.names(cwm)
rownames(cwm) <- c()
cwm
colnames(cwm) <- c("cwm_af","cwm_afe","cwm_cfms","cwm_dm","cwm_n","cwm_p","plot")
data_cwm <- (left_join(xy_plot,cwm,by="plot"))

#Eliminando data sets
rm(cwm,dabund_clean,dabund_clean2,dabund_relativa, 
   deff_clean,deff_clean2,fdis,feve,fdiv,indices,n1,n2,xy_plot).

#Salvando datos
write.csv(data_cwm,
          "C:data/clean/data_cwm_coord.csv")
write.csv(data_indices,
          "C:data/clean/data_fdiversity_coord.csv")
