rm(list=ls())


# Objetivo -----------------------------------------------------------------
#El objetivo de este script es ver si los valores de redundancia sin palmas 
#de las 127 parcelas tienen alguna estructura de dependecia espacial

#El codigo fue tomado de 
#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/



# Cargar paquetes ---------------------------------------------------------
library(ape)


# Cargar datos ------------------------------------------------------------
data_redundancy_sinpalmas <- read.csv("data/resultados_csv/data_redundancy_sinpalmas.csv", 
                                      header=T)
head(data_redundancy_sinpalmas, n=10)


# Matrix of inverse distance weights --------------------------------------
#In the matrix, entries for pairs of points that are close together 
#are higher than for pairs of points that are far apart

#Se utiliza las variables latitude y longitude
redundancy_dists <- as.matrix(dist(cbind(data_redundancy_sinpalmas$longitude, 
                                         data_redundancy_sinpalmas$latitude)))

redundancy_dists_inv <- 1/redundancy_dists

diag(redundancy_dists_inv) <- 0

#La matriz debe quedar con 0s en la diagonal
redundancy_dists_inv[1:5, 1:5]


# Moran´s I ---------------------------------------------------------------
#Null hypothesis <--- No spatial autocorrelation

#Redundancy sin palmas
Moran.I(data_redundancy_sinpalmas$redundancy, redundancy_dists_inv)

#Uniqueness sin palmas
Moran.I(data_redundancy_sinpalmas$U,  redundancy_dists_inv)

#Rao sin palmas
Moran.I(data_redundancy_sinpalmas$Q,  redundancy_dists_inv)



