rm(list = ls())

# Objetivo ----------------------------------------------------------------
#Calcular CWM para los rasgos funcionales de las 127 parcelas siguiendo el 
#capitulo 4 del libro Functional and Phylogenetic Ecology in R y el tuorial
# Intro to functional programing

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
dim(dabund)
dim(deff_clean)
anti_join(names_abund,names_effe, by="especie")

#Eliminar las espcecies que no tienen rasgos funcionales, para que ambos
#data sets tengan las mismas especies
dabund_clean <- dabund %>% 
  select(-c(MAYTGU,QUETOC,RUPTCA))

#Revisar que los datasets tengan las mismas especies
dim(deff_clean)
dim(dabund_clean)

#Anti_joint
names_abund_clean<-as.data.frame(colnames(dabund_clean))
colnames(names_abund_clean)<-"especie"
anti_join(names_abund_clean,names_effe,by="especie")  
#No hay matches

#Covertir cantidad de individuos por especie a abundancia relativa
dabund_clean
dabund_relativa<-decostand(dabund_clean,method = "total",MARGIN = 1)
dabund_relativa

#Remover datos que no se utilizan
rm(dabund, deff,names_abund_clean,names_abund,names_effe)
















