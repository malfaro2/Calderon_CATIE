rm(list = ls())

# Variation Partitioning - incorporating spatial processes ----------------

# Objetivo ----------------------------------------------------------------
# Se realizará el procedimiento de partition variation para los indices de 
#fdis feve fdiv

#Este ejemplo fue tomado de www.hiercourse.com/docs/Rnotes_multivariate.pdf

# Paquetes ----------------------------------------------------------------
library(vegan)
library(adespatial)

# Cargar datos ------------------------------------------------------------

# Data redundancy-Rao-Uniqueness ------------------------------------------

data_redundancy <- 
  read.csv("scripts/statistical_analysis/partition_variation/data/data_redundancy.csv")

# Data Variables ambientales ----------------------------------------------
data_environmet_topo <- 
  read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_topo.csv")

data_environmet_parcelas <- 
  read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_quimico.csv")

data_environmet_clima <- 
  read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_clima.csv")


# Data log coordenadas de las parcelas  -----------------------------------

data_coor_parcelas <- 
  read.csv("scripts/statistical_analysis/partition_variation/data/data_coor_parcelas.csv")
