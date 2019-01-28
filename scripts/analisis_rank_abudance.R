rm(list = ls())

#Fitting species abundance models with maximum likelihood
#Quick reference for sads package

library(tidyverse)
library(vegan)
library(sads)
library(knitr)
library(kableExtra)
library(magrittr)

#Cargar data
data_abund <- read.csv("data/clean/data_abund_plot.csv",row.names = 1)
length(row.names(data_abund))

#Obtener cantidad total de individuos por especie
data_rankabund <- apply(data_abund, 2, sum)


#Ranks abundnacia
rankabund <- rad(data_rankabund)
rankabund <- data.frame(rankabund)
sum(rankabund$abund)


#Ranks abundancia relativa
rankabund$relativa <- rankabund$abund/4808
sum(rankabund$relativa)



kable(rankabund, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "rankabund_realtiva.html")

#Curva abundancia
ggplot(rankabund,aes(x=rank,y=abund)) + geom_point()+theme_bw()+
  ylab("Abundancia")+
  annotate("text", x = 200, y = 800, label = "108 singletons and doubletons")+
  annotate("text", x = 200, y = 770, label = "192 especies con 10 o menos individuos(73.8%)")      +
  annotate("text", x = 200, y = 740, label = "63 especies con mas de 11 pero menos de 61(24.23%)")+
  annotate("text", x = 200, y = 710, label = "5 especies con mas de 61 individuos (1.92%)")


#Cantidad de singletons and doubletons
count(rankabund[rankabund$abund<= 2,])


#Curva abundancia relativa
ggplot(rankabund,aes(x=rank,y=relativa)) + geom_point()+theme_bw()+
  ylab("Abundancia relativa")+
  annotate("text", x = 200, y = .13, label = "Abundancia relativa,especies con 10 o menos individuos(12%)")      +
  annotate("text", x = 200, y = .12, label = "Abundancia relativa,especies con mas de 11 pero menos de 61(34%)")+
  annotate("text", x = 200, y = .11, label = "Abundancia relativa,especies con mas de 61 individuos (58%)")


#Especies con menos de 10 individuos
esp10<- (rankabund[rankabund$abund<=10,])
esp10

#Abundancia relativa de especies con 10 o menos
sum(esp10$relativa)

#Porcentaje,  especies con 10 o menos
(length(row.names(esp10))/260)*100


#Porcentaje, Especies con mas de 10 individuos pero menos de 61
esp_11_61 <- rankabund[rankabund$abund>10 & rankabund$abund<=61,]
(length(row.names(esp_11_61))/260)*100

#Abundancia relativa de las especies con 11 a 61
sum(esp_11_61$relativa)

#Especies que tienen mayor a 61
esp_mayor_61 <- rankabund[rankabund$abund > 61,]

#Abundancia relativa
sum(esp_mayor_61$relativa)

(5/260)*100

#Total
24.23077+73.84615+1.923077


#Total2
0.5740433 + 0.3036606 +  0.1222962
