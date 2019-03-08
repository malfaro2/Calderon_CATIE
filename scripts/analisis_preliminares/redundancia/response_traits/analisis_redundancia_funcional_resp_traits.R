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
#datos de response traits de las 127 parcelas

#LOS RASGOS UTILIZADOS PARA EL ANALISIS ES AFE,DM,CFMS,N,P


# Paquetes ----------------------------------------------------------------
library(tidyverse)

# Cargar data -------------------------------------------------------------

#Response traits

dresp <- read.csv("data/raw/response_traits/data_respose_traits_final.csv", 
                  header = T)
str(dresp)
head(dresp)

dresp <- dresp %>% 
  select(-c(familia,especie)) %>% 
  column_to_rownames("X")

head(dresp)


# Data abundancia ---------------------------------------------------------

dabund_relativa <- read.csv("data/clean/dabund_relativa.csv", 
                               row.names = 1, header = T)
head(dabund_relativa)

# cargar funcion uniqueness -----------------------------------------------
source("scripts/functions/function_functional_redundancy_original.R")


#Eliminar especies en abundancia relativa
dabund_rela_clean <- dabund_relativa %>% 
  select(-c(ABARAD ,APEIME ,BALIEL ,BROSGU ,BROSLA,
            CASEAR ,CECRIN ,CECROB ,COJOCO ,COUMMA,
            CYNORE ,DIALGU ,ENTESC ,GRIACA ,GUETSP,
            INGAAC ,INGAAE ,INGAAL ,INGACH ,INGAJI,
            INGALE ,INGAMO ,INGAPE ,INGASE ,LICNAF,
            LICNKA ,LICNSA ,LICNSP ,MICRME ,OCHRPY,
            PACHAQ ,PODOGU ,POURBI ,POURMI ,POUTBE,
            POUTCU ,POUTDU ,PSYCPA ,TAPIGU ,TOVOWE))

#Ordenar los nombres 
target <- colnames(dabund_rela_clean) 
dresp <- dresp[match(target, row.names(dresp)),]

dim(dabund_rela_clean)
dim(dresp)


# comm --------------------------------------------------------------------
#Data parcelas por especies
#En las Columnas deben ir las especies y las parcelas en las filas 

comm <- dabund_rela_clean

# dis ---------------------------------------------------------------------
#Data rasgos: se deben convertir objeto dis
#dis: An object of class 'dist' containing the functional distances among 
#species

#Transformar traits a distancias
dist <- gowdis(dresp) 

#Transformar distancias a un rango de entre 0 y 1
dist_rescaled <- dist / max(dist)
summary(dist_rescaled)

# Funcion -----------------------------------------------------------------

unique_resptraits <- uniqueness(comm, dist_rescaled, abundance=TRUE)

#Extraer medidas de comunidad
(medidas_redundancia_resptraits <- unique_resptraits$red)

#Agregar medida de redundancia
(medidas_redundancia_resptraits <- medidas_redundancia_resptraits %>% 
    rownames_to_column("plot") %>% 
    mutate(redundancy = 1- U))
  

#write.csv(medidas_redundancia_resptraits,"data/resultados_csv/data_redundancy_resptraits.csv")
