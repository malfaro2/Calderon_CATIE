rm(list = ls())

#Fitting species abundance models with maximum likelihood
#Quick reference for sads package

library(tidyverse)
library(vegan)
library(sads)


#Cargar data
data_abund <- read.csv("data/clean/data_abund_plot.csv",row.names = 1)
length(row.names(data_abund))

#Obtener cantidad total de individuos por especie
data_rankabund <- apply(data_abund, 2, sum)
data_rankabund

#Ranks
rankabund<-rad(data_rankabund)
rankabund<-data.frame(rankabund)

#Curva
ggplot(rankabund,aes(x=rank,y=abund)) + geom_point()+theme_bw()

#Especies con menos de 10 individuos
esp10<- (rankabund[rankabund$abund<=10,])
(length(row.names(esp10))/260)*100

#ESpecies con mas de 10 individuos pero menos de 61
esp_11_61 <- rankabund[rankabund$abund>10 & rankabund$abund<=61,]
(length(row.names(esp_11_61))/260)*100

#Especies tienen mayor a 61
(5/260)*100

#Total
24.23077+73.84615+1.923077