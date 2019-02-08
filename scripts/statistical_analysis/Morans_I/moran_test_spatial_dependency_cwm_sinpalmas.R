rm(list=ls())

# Objetivo -----------------------------------------------------------------
#El objetivo de este script es ver si los valores de cwm sin palmas 
#de las 127 parcelas tienen alguna estructura de dependecia espacial

#El codigo fue tomado de 
#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/


# Cargar paquetes ---------------------------------------------------------
library(ape)

# Cargar datos ------------------------------------------------------------
data_cwm_sinpalmas <- read.csv("data/resultados_csv/data_cwm_sinpalmas_coord.csv",header=T)
head(data_cwm_sinpalmas, n=10)

# Matrix of inverse distance weights --------------------------------------
#In the matrix, entries for pairs of points that are close together 
#are higher than for pairs of points that are far apart

#Se utiliza las variables latitude y longitude
cwm_dists_sp <- as.matrix(dist(cbind(data_cwm_sinpalmas$longitude, 
                                            data_cwm_sinpalmas$latitude)))

cwm_dists_inv_sp <- 1/cwm_dists_sp

diag(cwm_dists_inv_sp) <- 0

#La matriz debe quedar con 0s en la diagonal
cwm_dists_inv_sp[1:5, 1:5]


# Moran´s I ---------------------------------------------------------------
#Null hypothesis <--- No spatial autocorrelation

#cwm afe sin palmas
Moran.I(data_cwm_sinpalmas$cwm_afe, cwm_dists_inv_sp)

#cwm cfms sin palmas
Moran.I(data_cwm_sinpalmas$cwm_cfms, cwm_dists_inv_sp)

#cwm dm sin palmas
Moran.I(data_cwm_sinpalmas$cwm_dm, cwm_dists_inv_sp)

#cwm n sin palmas
Moran.I(data_cwm_sinpalmas$cwm_n, cwm_dists_inv_sp)

#cwm p sin palmas
Moran.I(data_cwm_sinpalmas$cwm_p, cwm_dists_inv_sp)



