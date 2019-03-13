rm(list = ls())

# Argumentos de la funcion ------------------------------------------------

#comm	A matrix or a data frame of N plots × S species containing the 
#abundance or incidence (0/1) of all species in the in plots. 
#Columns are species and plots are rows

#dis	An object of class 'dist' containing the functional distances 
#among species

#tol	A tolerance threshold (a value less than tol is considered as null)

#abundance	A logical. If TRUE abundance data are used when available; 
#if FALSE incidence (0/1) data are used.

#   -----------------------------------------------------------------------

# Objetivo ----------------------------------------------------------------
#El objetivo de este script es calcular el uniqueness y redundancy para los 
#datos de response traits sin palmas de las 127 parcelas

#LOS RASGOS UTILIZADOS PARA EL ANALISIS ES 


# Paquetes ----------------------------------------------------------------
library(tidyverse)
library(FD)

# Cargar data -------------------------------------------------------------

#Response traits

dresp_sp <- read.csv("data/clean/dresp_clean_sinpalmas.csv", 
                  header = T, row.names = 1)
str(dresp_sp)
#Se elimina columnas inncesarias y las palmas
names(dresp_sp)

dresp_sp <- dresp_sp %>%
  select(-c(familia,especie)) 

head(dresp_sp)
dim(dresp_sp)

#Cargar data sin palmas
dabund_relativa_sp <- read.csv("data/clean/dabund_relativa_sinpalmas.csv", 
                               row.names = 1, header = T)

head(dabund_relativa_sp)
dim(dabund_relativa_sp)

# cargar funcion uniqueness -----------------------------------------------
source("scripts/functions/function_functional_redundancy_original.R")



# comm --------------------------------------------------------------------
#Data parcelas por especies
#En las Columnas deben ir las especies y las parcelas en las filas 

comm <- dabund_relativa_sp

# dis ---------------------------------------------------------------------
#Data rasgos: se deben convertir objeto dis
#dis: An object of class 'dist' containing the functional distances among 
#species

#Transformar traits a distancias
dist <- gowdis(dresp_sp) 

#Transformar distancias a un rango de entre 0 y 1
dist_rescaled <- dist / max(dist)

# Funcion -----------------------------------------------------------------

unique_resptraits_sp <- uniqueness(comm, dist_rescaled, abundance=TRUE)

#Extraer medidas de comunidad
(medidas_redundancia_resptraits_sp <- unique_resptraits_sp$red)

#Agregar medida de redundancia

(medidas_redundancia_resptraits_sp <- medidas_redundancia_resptraits_sp %>% 
    rownames_to_column("plot") %>% 
    mutate(redundancy = 1- U))


#write.csv(medidas_redundancia_resptraits_sp,
#          "data/resultados_csv/data_redundancy_resptraits_sinpalmas.csv")
