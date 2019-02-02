rm(list = ls())

# -----------------------------------------------------------------------
#El fin de este script es ver cuales especies tienen rasgos efecto y rasgos
#respuesta y a cuales especies le faltan rasgos respuesta
source("scripts/data_cleaning.R")

#Eliminar data sets que no se van a usar 
rm(deff,dresp)


# Full join ---------------------------------------------------------------
#Se va a hacer un full join para ver cuales especies tienen effect traits
#y response traits, además de ver cuales especies les faltan response traits
#en total debe de ser 257 especies

data_full<-left_join(deff_clean,dresp_clean,key=especie) 

#Ordenar por nombre de la especie 
data_full<- arrange(data_full,  especie)

#Especies que no tienen response traits
especies_faltantes<-data_full[!complete.cases(data_full),]
length(especies_faltantes$especie)

#Ordenar por nombre de la especie 
especies_faltantes<-arrange(especies_faltantes,especie)

#Especies que tienen response traits
especies_completas<-data_full[complete.cases(data_full),]
length(especies_completas$especie)

#Ordenar por nombre de la especie 
especies_completas<-arrange(especies_completas, especie)


# Comprobacion ------------------------------------------------------------

# anti join ---------------------------------------------------------------
#drops all observations in x that have a match in y. R4DS pp190
# Estas especies no tienen response traits
especies_faltantes2 <- anti_join(deff_clean,dresp_clean,key=especie)
length(especies_faltantes2$especie)


# semi join ---------------------------------------------------------------
#keeps all observations in x that have a match in y.
#Estas especies tienen response and effect traits
especies_completas2 <-  semi_join(deff_clean,dresp_clean,key=especie)
length(especies_completas2$especie)







