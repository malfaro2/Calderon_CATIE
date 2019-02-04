rm(list=ls())


# Objetivo ----------------------------------------------------------------
#El objetivo de este script es desarrollar el ejemplo encontrado en
#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
#el cual hace la prueba de Moran para detectar dependecias espaciales. 


# Cargar paquetes ---------------------------------------------------------
library(ape)


# Cargar datos ------------------------------------------------------------
data_cwm <- read.csv("data/resultados_csv/data_cwm_coord.csv", header=T)
head(data_cwm, n=10)


# Matrix of inverse distance weights --------------------------------------
#In the matrix, entries for pairs of points that are close together 
#are higher than for pairs of points that are far apart

#Se utiliza las variables latitude y longitude
cwm_dists <- as.matrix(dist(cbind(data_cwm$longitude, data_cwm$latitude)))

cwm_dists_inv <- 1/cwm_dists

diag(cwm_dists_inv) <- 0

#La matriz debe quedar con 0s en la diagonal
cwm_dists_inv[1:5, 1:5]


# Moran´s I ---------------------------------------------------------------
#Null hypothesis <--- No spatial autocorrelation

#CWM af
Moran.I(data_cwm$cwm_af, cwm_dists_inv)

#CWM AFE
Moran.I(data_cwm$cwm_afe, cwm_dists_inv)

#CWM cfms
Moran.I(data_cwm$cwm_cfms, cwm_dists_inv)

#CWM DM
Moran.I(data_cwm$cwm_dm, cwm_dists_inv)

#CWM N
Moran.I(data_cwm$cwm_n, cwm_dists_inv)

#CWM P
Moran.I(data_cwm$cwm_p, cwm_dists_inv)

