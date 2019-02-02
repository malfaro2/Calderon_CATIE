rm(list = ls())
dir()
dir("data/raw/effect_traits")

#comentario prueba

# Cargar paquetes ---------------------------------------------------------
library(tidyverse)


# Cargar data -------------------------------------------------------------
deff<- read.csv("data/raw/effect_traits/data_effect_traits.csv")
head(deff)

# Data cleaning -----------------------------------------------------------

# Effect traits -----------------------------------------------------------
#El set de datos de effect traits viene con columnas que no son necesarias,
#el genero y especie estas estan separadas y los nombres de las columnas 
#tiene un nombre inadecuado

deff_clean <- deff %>% 
  rename(familia=Familia,especie=Especie,af=AF.mm2.,
         afe=AFE.mm2mg.1.,cfms=CFMS.Mgg.1.,dm=DM.gcm3.1.,n=N.mgg.1.,
         p=P.mgg.1.) %>% 
  select(-rasgo.de.) %>% 
  unite(especie,genero,especie,sep="_")

length(deff_clean$especie)
head(deff_clean)
dim(deff_clean)

#Eliminacion de especies de palmas y del rasgo de area foliar del data set
#Esto debido a que los analisis se deben hacer tambien sin estas especies
#además el area foliar se elimina debido a su dificultad de interpretacion

deff_clean_sin_palmas <- deff_clean %>%
   filter(familia  != "ARECACEAE") %>% 
   select(-af)

length(deff_clean_sin_palmas$especie)
dim(deff_clean_sin_palmas)
head(deff_clean_sin_palmas)

# Response traits ---------------------------------------------------------
#El set de datos tiene columnas que no son necesarias, los nombres de las 
#columnas son inadecuadas y el genero y especie estan en columnas separadas
dir("data/raw/response_traits")

dresp <- read.csv("data/raw/response_traits/data_response_traits.csv")
head(dresp)

dresp_clean<-dresp %>% 
  rename(estrato=Estrato,diseminacion=Diseminacion,rep.veg=Rep.Vegetativa,
         sist.sexual=Sist.Sexual,polinizacion=Polinizacion,
         tasacrecimiento=Tasacrecimiento) %>% 
  select(-autoridad) %>% 
  unite(especie,genero,especie,sep = "_")

# Save clean data ---------------------------------------------------------

write.csv(deff_clean,"data/clean/deff_clean.csv")
write.csv(dresp_clean,"data/clean/dresp_clean.csv")
write.csv(deff_clean_sin_palmas,"data/clean/deff_clean_sin_palmas.csv")














