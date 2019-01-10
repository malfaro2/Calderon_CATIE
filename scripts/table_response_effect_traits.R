rm(list = ls())
source("scripts/data_joining_response_effect_traits.R")


# Cargar paquetes ---------------------------------------------------------
library(knitr)
library(kableExtra)


#-----------------------------------------------------------------------
#Este script es para la creacion de las tablas de especies que tienen 
#rasgos de efecto y respuesta completos y especies a las que le falta esta 
#información 


# eliminar data sets no usados --------------------------------------------
rm(data_full,deff_clean,dresp_clean,especies_completas2,especies_faltantes2)



# Tablas ------------------------------------------------------------------
#https://haozhu233.github.io/kableExtra/awesome_table_in_html.html

#Tabla con especies sin rasgos de respuesta
#Eliminar columna 
especies_faltantes <- especies_faltantes %>% select(-coespec) 
  kable(especies_faltantes)  %>% 
  kable_styling(bootstrap_options = "striped", full_width = F) %>% 
  save_kable(file = "table_sp_sin_response_traits.html", self_contained = T)
  
  
#Tabla con especies sin rasgos de respuesta
#Eliminar columna 
especies_completas <- especies_completas %>% select(-coespec) 
kable(especies_completas)  %>% 
  kable_styling(bootstrap_options = "striped", full_width = F) %>% 
  save_kable(file = "table_sp_con_response-effect_traits.html", self_contained = T)
  