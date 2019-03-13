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

head(dresp)
dresp <- dresp %>% 
  select(-c(familia,especie))

str(dresp)
levels(dresp$fijacion_nitrogeno)
levels(dresp$dispersion)
levels(dresp$sist_sexual)
levels(dresp$polinizacion)

head(dresp)
dim(dresp)

# Data abundancia ---------------------------------------------------------
dabund_relativa <- read.csv("data/clean/dabund_relativa.csv", 
                            row.names = 1, header = T)
dim(dabund_relativa)
# Data area basal ---------------------------------------------------------

darea_basal <- read.csv("data/clean/dareabasal_sp.csv", 
                            row.names = 1, header = T)
head(darea_basal)
str(darea_basal)
dim(darea_basal)


# Cleaning data sets ------------------------------------------------------
#Las especies en abundancias y rasgos tienen que tener el mismo orden para
# que la funcion dbFD corra

#Ver que las especies sean las mismas en ambos data sets
n1 <- data.frame(as.factor(colnames(darea_basal)))
colnames(n1) <- "especie" 

n2 <- data.frame(row.names (dresp))
colnames(n2) <- "especie" 

#View(n1)
#View(n2)
anti_join(n1,n2, by="especie")

#Eliminar especies 260-3

darea_basal <- darea_basal  %>% 
  select(-c(MAYTGU,QUETOC,RUPTCA))


#Ordenar los nombres 
#target <- colnames(dabund_rela_clean) 
#dresp <- dresp[match(target, row.names(dresp)),]

#View(dresp$coespec)

# Indices de diversidad ---------------------------------------------------

#Calcular indices de diversidad


indices_abundrela <- dbFD(dresp[colnames(dabund_relativa),],
                dabund_relativa, w.abun = T,stand.x = F,corr="cailliez")

indices_areabasal <- dbFD(dresp[colnames(darea_basal),],
                          darea_basal,stand.x = F,corr="cailliez")

# Extraer indices ---------------------------------------------------------

#Ponderados por abunancia relativa
fdis_abundrela <- data.frame(indices_abundrela$FDis)
feve_abundrela <- data.frame(indices_abundrela$FEve)
fdiv_abundrela <- data.frame(indices_abundrela$FDiv)

fdiver_resptrait_abunrela <- cbind(fdis_abundrela,feve_abundrela,fdiv_abundrela)
colnames(fdiver_resptrait_abunrela) <- c("fdis_abundrela","feve_abundrela","fdiv_abundrela")
fdiver_resptrait_abunrela

write.csv(fdiver_resptrait_abunrela,"data/resultados_csv/data_fdiver_resptrait_abunrela.csv")

#Ponderados por area basal
fdis_areabasal <- data.frame(indices_areabasal$FDis)
feve_areabasal <- data.frame(indices_areabasal$FEve)
fdiv_areabasal <- data.frame(indices_areabasal$FDiv)

fdiver_resptrait_areabas <- cbind(fdis_areabasal,feve_areabasal,fdiv_areabasal)
colnames(fdiver_resptrait_areabas) <- c("fdis_areabasal","feve_areabasal","fdiv_areabasal")
fdiver_resptrait_areabas

write.csv(fdiver_resptrait_areabas,"data/resultados_csv/data_fdiver_resptrait_areabas.csv")
