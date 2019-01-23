rm(list = ls())

# Objetivo ----------------------------------------------------------------
#Calcular los indices de diversidad funcional FDis, FDiv, FEve y CWM para 
#todas las 127 parcelas del norte de Costa Rica mediante el paquete FD


#Paquetes
library(FD)
library(tidyverse)

#Cargar datos
source("scripts/data_cleaning_for_loops.R")
xy_plot <- read.csv("data/raw/data_posicion_parcelas.csv")


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

#calcular distancias euclideas para los rasgos effecto
eucl_deff <- dist(deff_clean2,method="euclidean")

#calcular indices de diversidad

#Weight por abundancia relativa

dbFD(x=eucl_deff, a=dabund_relativa, calc.FDiv = T)

b <- dbFD(x=deff_clean2, a=dabund_relativa, calc.FDiv = T)
b
list(b$FEve,b$FDis,b$FDiv)







