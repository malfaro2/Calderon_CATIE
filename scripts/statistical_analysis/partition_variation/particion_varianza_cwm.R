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

cwm_af_pcnm <- rda(data_species ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors af

cwm_af0_pcnm <- rda(data_cwm$cwm_af ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para af
step_pcnm_af <- ordistep(cwm_af0_pcnm, scope=formula(cwm_af_pcnm))

#Ver pcnm significativos
step_pcnm_af$anova 

# create pcnm table with only significant axes
cwm_af_pcnm_sub <- scores(cwm_af_pcnm,
                        choices=c(1,2,3,4,7,9))


# Seleccionar pcnm significativos afe -------------------------------------

# Model con all predictors afe

cwm_afe_pcnm <- rda(data_cwm$cwm_afe ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors [rasgo]
cwm_afe0_pcnm <- rda(data_cwm$cwm_afe ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para [rasgo]
step_pcnm_afe <- ordistep(cwm_afe0_pcnm, scope=formula(cwm_afe_pcnm))

#Ver pcnm significativos
step_pcnm_afe$anova 

# create pcnm table with only significant axes
cwm_afe_pcnm_sub <- scores(cwm_afe_pcnm,
                               choices=c(1,2,3,4,8,23,28,37,43))


# Seleccionar pcnm significativos cfms ------------------------------------

# Model con all predictors CFMS

cwm_cfms_pcnm <- rda(data_cwm$cwm_cfms ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors cmfs
cwm_cfms0_pcnm <- rda(data_cwm$cwm_cfms ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para cfms
step_pcnm_cfms <- ordistep(cwm_cfms0_pcnm, scope=formula(cwm_cfms_pcnm))

#Ver pcnm significativos
step_pcnm_cfms$anova 

# create pcnm table with only significant axes
cwm_cfms_pcnm_sub <- scores(cwm_cfms_pcnm,
                               choices=c(1,4,5,8,11,24,27,37))

# Seleccionar pcnm significativos dm --------------------------------------

# Model con all predictors dm

cwm_dm_pcnm <- rda(data_cwm$cwm_dm ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors [rasgo]
cwm_dm0_pcnm <- rda(data_cwm$cwm_dm ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para [rasgo]
step_pcnm_dm <- ordistep(cwm_dm0_pcnm, scope=formula(cwm_dm_pcnm))

#Ver pcnm significativos
step_pcnm_dm$anova 

# create pcnm table with only significant axes
cwm_dm_pcnm_sub <- scores(cwm_dm_pcnm,
                               choices=c(1,2,4,5,7,10,24))


# Seleccionar pcnm significativos Nitrogeno -------------------------------

# Model con all predictors [rasgo]

cwm_n_pcnm <- rda(data_cwm$cwm_n ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors [rasgo]
cwm_n0_pcnm <- rda(data_cwm$cwm_n ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para [rasgo]
step_pcnm_n <- ordistep(cwm_n0_pcnm, scope=formula(cwm_n_pcnm))

#Ver pcnm significativos
step_pcnm_n$anova 

# create pcnm table with only significant axes
cwm_n_pcnm_sub <- scores(cwm_n_pcnm,
                               choices=c(1,3,9,16,17,36))

# Seleccionar pcnm significativos Fosforo ---------------------------------

# Model con all predictors fosforo

cwm_p_pcnm <- rda(data_cwm$cwm_p ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors fosforo
cwm_p0_pcnm <- rda(data_cwm$cwm_p ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para fosforo
step_pcnm_p <- ordistep(cwm_p0_pcnm, scope=formula(cwm_p_pcnm))

#Ver pcnm significativos
step_pcnm_p$anova 

# create pcnm table with only significant axes
cwm_p_pcnm_sub <- scores(cwm_p_pcnm,
                               choices=c(1:5,7,17,24))



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

cwm_var <- varpart(as.matrix(data_cwm$af),
                    ~ SAND + LIMO + CLAY + pH + ACIDITY , 
                    ~ Ca + Mg + K + P + OrganicMatter ,
                    ~ ELEV + SLOPE_PER ,
                    ~ PRECDRIEST + PRECCV + TEMP + TEMPMIN + TEMPSD,
                      endo.pcnm.sub,data=data_environmet)


