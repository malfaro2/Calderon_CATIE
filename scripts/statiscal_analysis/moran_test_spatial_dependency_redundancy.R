rm(list=ls())


# Objetivo -----------------------------------------------------------------
#El objetivo de este script es ver si los valores de redundancia  
#de las 127 parcelas tienen alguna estructura de dependecia espacial

#El codigo fue tomado de 
#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/



# Cargar paquetes ---------------------------------------------------------
library(ape)


# Cargar datos ------------------------------------------------------------
data_redundancy <- read.csv("data/resultados_csv/data_redundancy.csv", 
                                      header=T)
head(data_redundancy, n=10)


# Matrix of inverse distance weights --------------------------------------
#In the matrix, entries for pairs of points that are close together 
#are higher than for pairs of points that are far apart

#Se utiliza las variables latitude y longitude
redundancy_dists <- as.matrix(dist(cbind(data_redundancy$longitude, 
                                         data_redundancy$latitude)))

redundancy_dists_inv <- 1/redundancy_dists

diag(redundancy_dists_inv) <- 0

#La matriz debe quedar con 0s en la diagonal
redundancy_dists_inv[1:5, 1:5]


# Moran´s I ---------------------------------------------------------------
#Null hypothesis <--- No spatial autocorrelation

#Redundancy sin palmas
Moran.I(data_redundancy$redundancy, redundancy_dists_inv)

#Uniqueness sin palmas
Moran.I(data_redundancy$U,  redundancy_dists_inv)

#Rao sin palmas
Moran.I(data_redundancy$Q,  redundancy_dists_inv)
