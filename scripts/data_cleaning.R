rm(list = ls())
dir()
dir("data/raw/effect_traits")


# Cargar paquetes ---------------------------------------------------------
library(tidyverse)


# Cargar data -------------------------------------------------------------
deff<- read.csv("data/raw/effect_traits/data_effect_traits.csv")
head(deff)



# Data cleaning -----------------------------------------------------------

#El set de datos de effect traits viene con columnas que no son necesarias,
#el genero y especie estas estan separadas y los nombres de las columnas 
#tiene un nombre inadecuado

deff_clean <- deff %>% 
  rename(familia=Familia,genero=Género,especie=Especie,af=AF.mm2.,afe=AFE.mm2mg.1.,cfms=CFMS.Mgg.1.,
         dm=DM.gcm3.1.,n=N.mgg.1.,p=P.mgg.1.) %>% 
  select(-rasgo.de.) %>% 
  unite(especie,genero,especie,sep="_")

head(deff_clean)
