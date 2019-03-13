rm(list = ls())

# Objetivo ----------------------------------------------------------------
#Calcular los indices de diversidad funcional FDis, FDiv, FEve, con los 
#datos de rasgos de respuesta para todas las 127 parcelas del norte de 
#Costa Rica mediante el paquete FD. Excluyendo las palmas 

#Para el calculo de los indices se siguio el libro Functional and 
#Phylogenetic Ecology in R. pp80

#Paquetes
library(FD)
library(tidyverse)

# Cargar datos ------------------------------------------------------------
#Response traits

dresp_sp <- read.csv("data/clean/dresp_clean_sinpalmas.csv", 
                  header = T, row.names = 1)

head(dresp_sp)
#Se elimina columnas inncesarias y las palmas

dresp_sp <- dresp_sp %>% 
 select(-c(familia,especie))  
#  filter(!X %in%  c("EUTEPR","IRIADE","SOCREX","WELFRE") ) %>% 
#  tibble::column_to_rownames(var= "X")

dim(dresp_sp)

str(dresp_sp)
head(dresp_sp)

#Data abundancia
dabund_relativa_sp <- read.csv("data/clean/dabund_relativa_sinpalmas.csv", 
                            row.names = 1, header = T)
dim(dabund_relativa_sp)

#Data area basal
darea_basal_sp <- read.csv("data/clean/dareabasal_sp_sinpalmas.csv", 
                               row.names = 1, header = T)

dim(darea_basal_sp)

darea_basal_sp <- darea_basal_sp  %>% 
  select(-c(MAYTGU,QUETOC,RUPTCA))
dim(darea_basal_sp)


# Cleaning data sets ------------------------------------------------------
#Las especies en abundancias y rasgos tienen que tener el mismo orden para
# que la funcion dbFD corra


#Ordenar los nombres 
#target <- colnames(dabund_rela_clean_sp) 
#dresp_sp <- dresp_sp[match(target, row.names(dresp_sp)),]
#View(dresp$coespec)

# Indices de diversidad ---------------------------------------------------

#Calcular indices de diversidad
dim(dabund_relativa_sp)
dim(dresp_sp)

indices_abundrela_sp <- dbFD(dresp_sp[colnames(dabund_relativa_sp),],
                   dabund_relativa_sp, w.abun = T,stand.x = F,corr="cailliez")

indices_areabasal_sp <- dbFD(dresp_sp[colnames(dabund_relativa_sp),],
                             darea_basal_sp,stand.x = F,corr="cailliez")


# Extraer indices ---------------------------------------------------------

#Ponderados por abunancia relativa
fdis_abundrela_sp <- data.frame(indices_abundrela_sp$FDis)
feve_abundrela_sp <- data.frame(indices_abundrela_sp$FEve)
fdiv_abundrela_sp <- data.frame(indices_abundrela_sp$FDiv)

fdiver_resptrait_abunrela_sp <- cbind(fdis_abundrela_sp,feve_abundrela_sp,fdiv_abundrela_sp)
colnames(fdiver_resptrait_abunrela_sp) <- c("fdis_abundrela_sp","feve_abundrela_sp","fdiv_abundrela_sp")
fdiver_resptrait_abunrela_sp

write.csv(fdiver_resptrait_abunrela_sp,"data/resultados_csv/data_fdiver_resptrait_abunrela_sp.csv")

#Ponderados por area basal
fdis_areabasal_sp <- data.frame(indices_areabasal_sp$FDis)
feve_areabasal_sp <- data.frame(indices_areabasal_sp$FEve)
fdiv_areabasal_sp <- data.frame(indices_areabasal_sp$FDiv)

fdiver_resptrait_areabas_sp <- cbind(fdis_areabasal_sp,feve_areabasal_sp,fdiv_areabasal_sp)
colnames(fdiver_resptrait_areabas_sp) <- c("fdis_areabasal_sp","feve_areabasal_sp","fdiv_areabasal_sp")
fdiver_resptrait_areabas_sp

write.csv(fdiver_resptrait_areabas_sp,"data/resultados_csv/data_fdiver_resptrait_areabas_sp.csv")
