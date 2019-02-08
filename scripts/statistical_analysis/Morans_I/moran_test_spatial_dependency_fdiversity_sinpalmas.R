rm(list=ls())

# Objetivo -----------------------------------------------------------------
#El objetivo de este script es ver si los valores de feve, fdis y fric 
#sin palmas de las 127 parcelas tienen alguna estructura de dependecia espacial

#El codigo fue tomado de 
#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/


# Cargar paquetes ---------------------------------------------------------
library(ape)

# Cargar datos ------------------------------------------------------------
data_fdiver_sinpalmas <- read.csv("data/resultados_csv/data_fdiversity_sinpalmas_coord.csv",header=T)
head(data_fdiver_sinpalmas, n=10)

# Matrix of inverse distance weights --------------------------------------
#In the matrix, entries for pairs of points that are close together 
#are higher than for pairs of points that are far apart

#Se utiliza las variables latitude y longitude
fdiver_dists_sp <- as.matrix(dist(cbind(data_fdiver_sinpalmas$longitude, 
                                        data_fdiver_sinpalmas$latitude)))

fdiver_dists_inv_sp <- 1/fdiver_dists_sp

diag(fdiver_dists_inv_sp) <- 0

#La matriz debe quedar con 0s en la diagonal
fdiver_dists_inv_sp[1:5, 1:5]


# Moran´s I ---------------------------------------------------------------
#Null hypothesis <--- No spatial autocorrelation

#fdiv sin palmas
Moran.I(data_fdiver_sinpalmas$fdiv, fdiver_dists_inv_sp)

#feve sin palmas
Moran.I(data_fdiver_sinpalmas$feve, fdiver_dists_inv_sp)

#Fdis sin palmas
Moran.I(data_fdiver_sinpalmas$fdis, fdiver_dists_inv_sp)





