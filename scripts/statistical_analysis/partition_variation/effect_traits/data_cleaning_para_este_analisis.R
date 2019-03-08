rm(list = ls())

# Variation Partitioning - incorporating spatial processes ----------------

# Objetivo ----------------------------------------------------------------
# Se realizará el procedimiento de partition variation para cwm 
#2 categorias de data ambiental, buscando el poder explicativo de cada una
#como variables predictoras de cambios en la comunidad.

#Este ejemplo fue tomado de www.hiercourse.com/docs/Rnotes_multivariate.pdf

# Paquetes ----------------------------------------------------------------
library(tidyverse)


# Cargar datos ------------------------------------------------------------

# Data cwm  ---------------------------------------------------------------

data_cwm <- read.csv("data/resultados_csv/data_cwm_coord.csv")
data_cwm <- data_cwm %>% 
  select(-c(1:4))

head(data_cwm)
dim(data_cwm)
str(data_cwm)

#write.csv(data_cwm,
#          "scripts/statistical_analysis/partition_variation/data/data_cwm.csv")



# Data Variables ambientales ----------------------------------------------
data_environmet <- read.csv("data/raw/data_enviroment_worldclim.csv")
head(data_environmet)


data_carac_suelos <- data_environmet %>% 
  select(5:9)

head(data_carac_suelos)

#write.csv(data_carac_suelos,
#          "scripts/statistical_analysis/partition_variation/data/data_carac_suelos.csv")

data_carac_quimico <- data_environmet %>% 
  select(10:14)
head(data_carac_quimico)

#write.csv(data_carac_quimico,
#          "scripts/statistical_analysis/partition_variation/data/data_carac_quimico.csv")

data_carac_topo <- data_environmet %>% 
  select(15:17)
head(data_carac_topo)

#write.csv(data_carac_topo,
#          "scripts/statistical_analysis/partition_variation/data/data_carac_topo.csv")

data_carac_clima <- data_environmet %>% 
  select(18:23)
head(data_carac_clima)

#write.csv(data_carac_clima,
#          "scripts/statistical_analysis/partition_variation/data/data_carac_clima.csv")


# Data Coordenadas de las parcelas ----------------------------------------
#Extraer data coordenadas y quitarlas del data set data_cwm
#Y transformarlas a log 10 usar CRTM

data_coor_parcelas <- data_environmet %>% 
  select(CRTM_90_X,CRTM_90_Y)

data_coor_parcelas <- log(data_coor_parcelas,10)


#write.csv(data_coor_parcelas,
#                   "scripts/statistical_analysis/partition_variation/data/data_coor_parcelas.csv")


head(data_coor_parcelas)
dim(data_coor_parcelas)

#Eliminar columnas innecesari