rm(list = ls())

# Objetivo ----------------------------------------------------------------
#Calcular los indices fdis,feve y fric para las 127 parcelas siguiendo el 
#capitulo 4 del libro Functional and Phylogenetic Ecology in R


library(tidyverse)
library(fBasics)
library(vegan)


# data de abundancia por parcela -----------------------------------
#Las especies van en las columnas y los nombres de las filas es la
#parcela

dabund<-read.csv("data/clean/data_abund_plot.csv",header=T,row.names = 1)
dim(dabund)


# data effect traits ------------------------------------------------------
#Las especies tienen que venir como row names

deff<-read.csv("data/clean/deff_clean.csv",header=T,row.names = 3)


# Cleaning data -----------------------------------------------------------
#Los datos tienen una columna de familia y especie. Es mejor eliminarlas
#para que ambos data sets sean iguales

deff_clean<-deff %>% 
  select(-c(familia,especie))

dim(deff_clean)


# Prueba ------------------------------------------------------------------
#El numero de columnas en dabund tienen que ser igual al numero de filas en
#deff_clean, sin embargo en este caso hay 3 especies de mas en dabund


names_abund<-as.data.frame(colnames(dabund))
colnames(names_abund)<-"especie"
names_effe<-as.data.frame(rownames(deff_clean))
colnames(names_effe)<-"especie"



#Anti join para saber cuales son las especies diferentes
anti_join(names_abund,names_effe, by="especie")




# Analisis ----------------------------------------------------------------

#Abundancia y abundancia relativas de las especies en las 127 parcelas

#Especies con abundanca mayor a 0 en la primera parcela
(spp1<-names(dabund[1,dabund[1,]>0]))

(spp12<-names(dabund[12,dabund[12,]>0]))

(spp3<-names(dabund[3,dabund[3,]>0]))

#Rasgos funcionales de las especies presentes en la parcela 12
deff_clean[spp12,]

dabund_relativa<-decostand(dabund,method = "total",MARGIN = 1)

#Abundancia relativa de la parcela 2
dabund_relativa[12,dabund_relativa[12,]>0]

rowSums(dabund_relativa[12,dabund_relativa[12,]>0])










