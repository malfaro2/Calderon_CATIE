rm(list=ls())


# Objetivo ----------------------------------------------------------------
#El objetivo de este script es desarrollar el ejemplo encontrado en
#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
#el cual hace la prueba de Moran para detectar dependecias espaciales. 


# Cargar paquetes ---------------------------------------------------------
library(ape)


# Cargar datos ------------------------------------------------------------
ozone <- read.table("https://stats.idre.ucla.edu/stat/r/faq/ozone.csv", sep=",", header=T)
head(ozone, n=10)


# Matrix of inverse distance weights --------------------------------------
#In the matrix, entries for pairs of points that are close together 
#are higher than for pairs of points that are far apart

#Se utiliza las variables latitude y longitude
ozone.dists <- as.matrix(dist(cbind(ozone$Lon, ozone$Lat)))

ozone.dists.inv <- 1/ozone.dists

diag(ozone.dists.inv) <- 0

#La matriz debe quedar con 0s en la diagonal
ozone.dists.inv[1:5, 1:5]


# Moran´s I ---------------------------------------------------------------

Moran.I(ozone$Av8top, ozone.dists.inv)

#Null hypothesis <--- No spatial autocorrelation





