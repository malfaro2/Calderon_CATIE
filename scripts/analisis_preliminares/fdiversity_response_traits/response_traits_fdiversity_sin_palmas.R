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

dresp <- read.csv("data/raw/response_traits/data_respose_traits_final.csv", 
                  header = T)

head(dresp)
#Se elimina columnas inncesarias y las palmas

dresp_sp <- dresp %>% 
  select(-c(familia,especie)) %>% 
  filter(!X %in%  c("EUTEPR","IRIADE","SOCREX","WELFRE") ) %>% 
  tibble::column_to_rownames(var= "X")

head(dresp_sp)
dim(dresp)
dim(dresp_sp)

str(dresp)
head(dresp)

#Data abundancia
dabund_relativa_sp <- read.csv("data/clean/dabund_relativa_sinpalmas.csv", 
                            row.names = 1, header = T)
dim(dabund_relativa_sp)
# Cleaning data sets ------------------------------------------------------
#Las especies en abundancias y rasgos tienen que tener el mismo orden para
# que la funcion dbFD corra

#Ver que las especies sean las mismas en ambos data sets
n1 <- data.frame(as.factor(colnames(dabund_relativa_sp)))
colnames(n1) <- "especie" 
class(n1)

n2 <- data.frame(row.names (dresp_sp))
colnames(n2) <- "especie" 
class(n2)

anti_join(n1,n2, by="especie")

#Eliminar especies
dabund_rela_clean_sp <- dabund_relativa_sp %>% 
  select(-c(ABARAD ,APEIME ,BALIEL ,BROSGU ,BROSLA,
            CASEAR ,CECRIN ,CECROB ,COJOCO ,COUMMA,
            CYNORE ,DIALGU ,ENTESC ,GRIACA ,GUETSP,
            INGAAC ,INGAAE ,INGAAL ,INGACH ,INGAJI,
            INGALE ,INGAMO ,INGAPE ,INGASE ,LICNAF,
            LICNKA ,LICNSA ,LICNSP ,MICRME ,OCHRPY,
            PACHAQ ,PODOGU ,POURBI ,POURMI ,POUTBE,
            POUTCU ,POUTDU ,PSYCPA ,TAPIGU ,TOVOWE))

#Ordenar los nombres 
target <- colnames(dabund_rela_clean_sp) 
dresp_sp <- dresp_sp[match(target, row.names(dresp_sp)),]
#View(dresp$coespec)

# Indices de diversidad ---------------------------------------------------

#Calcular indices de diversidad
dim(dabund_rela_clean_sp)
dim(dresp_sp)

indices_sp <- dbFD(dresp_sp[colnames(dabund_rela_clean_sp),],
                dabund_rela_clean_sp, w.abun = T,stand.x = F,corr="cailliez")




