rm(list = ls())

# Variation Partitioning - incorporating spatial processes ----------------

# Objetivo ----------------------------------------------------------------
# Se realizará el procedimiento de partition variation para cwm 
#2 categorias de data ambiental, buscando el poder explicativo de cada una
#como variables predictoras de cambios en la comunidad.

#Este ejemplo fue tomado de www.hiercourse.com/docs/Rnotes_multivariate.pdf

# Paquetes ----------------------------------------------------------------
library(vegan)
library(tidyverse)

# Cargar datos ------------------------------------------------------------

# Data cwm  ---------------------------------------------------------------

data_species<- read.csv("data/clean/data_abund_plot.csv",header = T,row.names = 1)
head(data_species)
dim(data_species)
str(data_species)

#Extraer data coordenadas y quitarlas del data set data_cwm
data_coor_parcelas <- read.csv("data/resultados_csv/data_cwm_coord.csv")
data_coor_parcelas <- data_coor_parcelas %>% 
  select(longitude,latitude)



# Data Coordenadas de las parcelas ----------------------------------------
head(data_coor_parcelas)
dim(data_coor_parcelas)
plot(data_coor_parcelas$longitude, data_coor_parcelas$latitude)

# Data Variables ambientales ----------------------------------------------
data_environmet <- read.csv("data/raw/data_enviroment_worldclim.csv")
head(data_environmet)
str(data_environmet)

a <- c(data_environmet$ELEV,data_environmet$PREC,data_environmet$PRECDRIEST,
       data_environmet$PRECCV,data_environmet$TEMP,data_environmet$TEMPMIN)
data_environmet$TEMPSD <- as.numeric(data_environmet$TEMPSD)
data_environmet$ <- as.numeric(data_environmet$)


#Eliminar columnas innecesarias
data_environmet <- data_environmet %>% 
  select(-c(1:4))
head(data_environmet)

dim(data_environmet)


# Calcular PCNMs a partir de una matriz de distancia euclidea -------------

#Principal Coordinates of Neighbour Matrices (PCNMs) generate a dataframe 
#containing variables that represent different spatial scales

#This represent the spatial distributions of samples
#las coordenadas geográficas (descompuestas en un análisis previo de PCNM 
#con 21 descriptores espaciales - autovectores del PCNM - ).


parcelas_pcnm <- pcnm(dist(data_coor_parcelas))


# Seleccionar pcnm significativos  --------------------------------------

# Model con all predictors af

especies_pcnm <- rda(data_species ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors af

especies0_pcnm <- rda(data_species ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para af
step_pcnm_especies <- ordistep(especies0_pcnm, scope=formula(especies_pcnm))

#Ver pcnm significativos
step_pcnm_especies$anova 

# create pcnm table with only significant axes
especies_pcnm_sub <- scores(especies_pcnm,
                          choices=c(1,2,3,4,5,6,7,9,17,36,25))




# Modelos  ----------------------------------------------------------------


# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela
#     (SAND, LIMO, CLAY, pH, acidicty, )

# 2) Caracteristicas fisicas de la parcela  quimicas de la parcela
#    (Ca, Mg, K, organic matter)

# 3)   Topografia 
#    (elevacion, pendiente )

# 3) Clima: Precipitacion temperatura

# 4) space ('parcelas_pcnm')



# Modelo af ---------------------------------------------------------------

cwm_var <- varpart(data_species,
                   ~ SAND + LIMO + CLAY + pH  , 
                   ~ Ca + Mg + K + P  ,
                  
                   ~ PRECDRIEST + PRECCV + TEMP + TEMPMIN + TEMPSD,
                   especies_pcnm_sub,data=data_environmet)
