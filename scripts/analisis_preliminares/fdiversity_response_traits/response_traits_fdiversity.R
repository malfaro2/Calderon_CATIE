rm(list = ls())

# Objetivo ----------------------------------------------------------------
#Calcular los indices de diversidad funcional FDis, FDiv, FEve, con los 
#datos de rasgos de respuesta para todas las 127 parcelas del norte de 
#Costa Rica mediante el paquete FD. 

#Para el calculo de los indices se siguio el libro Functional and 
#Phylogenetic Ecology in R. pp80

#Paquetes
library(FD)
library(tidyverse)



# Cargar datos ------------------------------------------------------------
#Response traits

dresp <- read.csv("data/raw/response_traits/data_respose_traits_final.csv", 
                  header = T,row.names = 1)

dresp <- dresp %>% 
  select(-c(familia,especie))

str(dresp)
head(dresp)

#Data abundancia
dabund_relativa <- read.csv("data/clean/dabund_relativa.csv", 
                            row.names = 1, header = T)

# Cleaning data sets ------------------------------------------------------
#Las especies en abundancias y rasgos tienen que tener el mismo orden para
# que la funcion dbFD corra

#Ver que las especies sean las mismas en ambos data sets
n1 <- data.frame(as.factor(colnames(dabund_relativa)))
colnames(n1) <- "especie" 
class(n1)

n2 <- data.frame(as.factor(dresp$coespec))
colnames(n2) <- "especie" 
class(n2)

#Eliminar especies
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
#View(dresp$coespec)

# Indices de diversidad ---------------------------------------------------

#Calcular indices de diversidad

gowdist <- gowdis(dresp)

indices <- dbFD(dresp[colnames(dabund_rela_clean),],
                dabund_rela_clean, w.abun = T,stand.x = F,corr="cailliez")







