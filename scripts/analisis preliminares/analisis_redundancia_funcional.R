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
#datos completos de las 127 parcelas

#LOS RASGOS UTILIZADOS PARA EL ANALISIS ES AFE,DM,CFMS,N,P

# Cargar data -------------------------------------------------------------

source("scripts/data_cleaning_for_loops.R")
xy_plot <- read.csv("data/raw/data_posicion_parcelas.csv")

#Eliminar columnas 
xy_plot <- xy_plot %>% 
  select(-c(CRTM_90_X,CRTM_90_Y))

# cargar funcion uniqueness -----------------------------------------------
source("scripts/functions/function_functional_redundancy_original.R")

#Eliminar data sets que no se van a utilizar
rm(dabund_clean_sinpalmas,dabund_relativa_sinpalmas,deff_clean_sinpalmas)

# comm --------------------------------------------------------------------
#Data parcelas por especies
#En las Columnas deben ir las especies y las parcelas en las filas 

comm<- dabund_clean

# dis ---------------------------------------------------------------------
#Data rasgos: se deben convertir objeto dis
#dis: An object of class 'dist' containing the functional distances among 
#species

#Transformar traits a distancias
dist <- vegdist(deff_clean, method = "euclidean") 

#Transformar distancias a un rango de entre 0 y 1
dist_rescaled <- dist / max(dist)
summary(dist_rescaled)

# Funcion -----------------------------------------------------------------

unique <- uniqueness(comm, dist_rescaled, abundance=TRUE)

#Extraer medidas de comunidad
(medidas_redundancia <- unique$red)

#Agregar medida de redundancia
(medidas_redundancia <- medidas_redundancia %>% 
    mutate(plot=row.names(medidas_redundancia)) %>% 
    mutate(redundancy = 1- U))

#Data full con coordenadas de cada parcela
data_redundancy <- left_join(medidas_redundancia, xy_plot, by="plot")


#write.csv(data_redundancy,"data/clean/resultados_csv/data_redundancy_sinpalmas.csv")