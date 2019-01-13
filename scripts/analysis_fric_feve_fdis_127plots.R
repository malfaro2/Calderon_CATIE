rm(list = ls())

# Objetivo ----------------------------------------------------------------
#Calcula los indices fdis,feve y fric para las 127 parcelas siguiendo el 
#capitulo 4 del libro Functional and Phylogenetic Ecology in R


#Paquetes
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


#Especies con abundanca mayor a 0 en la primera parcela
spp1<-names(dabund[1,dabund[1,]>0])
spp

spp2<-names(dabund[1,dabund[2,]>0])
spp2

#Seria interesante hacer un loop que me de la composicion de cada parcela
# el problema es que cada parcela tienen un numero diferente de espieces



















