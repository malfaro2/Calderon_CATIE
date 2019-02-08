rm(list=ls())

# Objetivo -----------------------------------------------------------------
#El objetivo de este script es ver si los valores de feve, fdis y fric 
#de las 127 parcelas tienen alguna estructura de dependecia espacial

#El codigo fue tomado de 
#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/


# Cargar paquetes ---------------------------------------------------------
library(ape)

# Cargar datos ------------------------------------------------------------
data_fdiver <- read.csv("data/resultados_csv/data_fdiversity_coord.csv",header=T)
head(data_fdiver, n=10)

# Matrix of inverse distance weights --------------------------------------
#In the matrix, entries for pairs of points that are close together 
#are higher than for pairs of points that are far apart

#Se utiliza las variables latitude y longitude
fdiver_dists <- as.matrix(dist(cbind(data_fdiver$longitude, 
                                     data_fdiver$latitude)))

fdiver_dists_inv <- 1/fdiver_dists

diag(fdiver_dists_inv) <- 0

#La matriz debe quedar con 0s en la diagonal
fdiver_dists_inv[1:5, 1:5]


# Moran´s I ---------------------------------------------------------------
#Null hypothesis <--- No spatial autocorrelation

# fdiversity  Fdiv
Moran.I(data_fdiver$fdiv, fdiver_dists_inv)

# fdiversity Feve
Moran.I(data_fdiver$feve, fdiver_dists_inv )

# fdiversity fdis
Moran.I(data_fdiver$fdis, fdiver_dists_inv )





