rm(list = ls())

# Variation Partitioning - incorporating spatial processes ----------------

# Objetivo ----------------------------------------------------------------
# Se realizará el procedimiento de partition variation para cwm 
#2 categorias de data ambiental, buscando el poder explicativo de cada una
#como variables predictoras de cambios en la comunidad.

#Este ejemplo fue tomado de www.hiercourse.com/docs/Rnotes_multivariate.pdf

# Paquetes ----------------------------------------------------------------
library(vegan)
library(adespatial)

# Cargar datos ------------------------------------------------------------

# Data cwm  ---------------------------------------------------------------

data_redundancy <- read.csv("scripts/statistical_analysis/partition_variation/data/data_redundancy.csv")

# Data Variables ambientales ----------------------------------------------
data_environmet_topo <- read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_topo.csv")
data_environmet_parcelas <- read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_quimico.csv")
data_environmet_clima <- read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_clima.csv")

# Data Coordenadas de las parcelas ----------------------------------------
#Extraer data coordenadas y quitarlas del data set data_cwm
#Y transformarlas a log 10 usar CRTM

data_coor_parcelas <- read.csv("scripts/statistical_analysis/partition_variation/data/data_coor_parcelas.csv")


# Redundancy --------------------------------------------------------------
# Calcular PCNMs a partir de una matriz de distancia euclidea -------------

#Principal Coordinates of Neighbour Matrices (PCNMs) generate a dataframe 
#containing variables that represent different spatial scales

#This represent the spatial distributions of samples
#las coordenadas geográficas (descompuestas en un análisis previo de PCNM 
#con 21 descriptores espaciales - autovectores del PCNM - ).


parcelas_pcnm <- pcnm(dist(data_coor_parcelas))


# Seleccionar pcnms significativos  --------------------------------------

# Model con all predictors af

redundancy_pcnm <- rda(data_redundancy$redundancy ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors af

redundancy0_pcnm <- rda(data_redundancy$redundancy ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para af
step_pcnm_redundnacy <- ordistep(redundancy0_pcnm, scope=formula(redundancy_pcnm))

#Ver pcnm significativos
step_pcnm_redundnacy$anova 

# create pcnm table with only significant axes
n_redundancy<-paste('PCNM', c(1,8,7,6,5,40,4,15,35,16,10,24,54,13), sep='')
n_redundancy
redundancy_pcnm_sub <- parcelas_pcnm$vectors[,n_redundancy]


# Forward selection de variables ambientales ------------------------------

#Variables caracteristicas suelo las variables que suman 100 pueden generar
#error

forward.sel(data_redundancy$redundancy, data_environmet_parcelas,
            alpha = 0.01)

forward.sel(data_redundancy$redundancy, data_environmet_clima,
            alpha = 0.01)

forward.sel(data_redundancy$redundancy, data_environmet_topo,
            alpha = 0.01)


# Modelo redundancy ---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela
#     ( CLAY )

# 2) Clima: PRECDRIEST TEMP

#3) Topo:ELEV

# 4) space ('parcelas_pcnm')


redundancy_var <- varpart(data_redundancy,
                      data_environmet_parcelas[,c("CLAY")],
                      data_environmet_topo[,c("ELEV")],
                      data_environmet_clima[,c("PRECDRIEST","TEMP")],
                      redundancy_pcnm_sub)

plot(redundancy_var)
redundancy_var


# RAO --------------------------------------------------------------------

# Seleccionar pcnms significativos  --------------------------------------

# Model con all predictors af

rao_pcnm <- rda(data_redundancy$Q ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors af

rao0_pcnm <- rda(data_redundancy$Q ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para af
step_pcnm_rao <- ordistep(rao0_pcnm, scope=formula(rao_pcnm))

#Ver pcnm significativos
step_pcnm_rao$anova 

# create pcnm table with only significant axes
n_rao<-paste('PCNM', c(1,8,4,35,15,10,5,6,40,7,13,17,16), sep='')
n_rao
rao_pcnm_sub <- parcelas_pcnm$vectors[,n_rao]


# Forward selection de variables ambientales ------------------------------

#Variables caracteristicas suelo las variables que suman 100 pueden generar
#error

forward.sel(data_redundancy$Q, data_environmet_parcelas,
            alpha = 0.01)

forward.sel(data_redundancy$Q, data_environmet_clima,
            alpha = 0.01)

forward.sel(data_redundancy$Q, data_environmet_topo,
            alpha = 0.01)

# Modelo rao---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela
#     ( CLAY Mg Ca )

# 2) Clima: PRECDRIEST TEMP

#3) Topo:ELEV

# 4) space ('parcelas_pcnm')


rao_var <- varpart(data_redundancy,
                          data_environmet_parcelas[,c("CLAY","Mg","Ca")],
                          data_environmet_topo[,c("ELEV")],
                          data_environmet_clima[,c("PRECDRIEST","TEMP")],
                          rao_pcnm_sub)

plot(rao_var)
rao_var


# Uniqueness --------------------------------------------------------------
# Seleccionar pcnms significativos  --------------------------------------

# Model con all predictors af

uni_pcnm <- rda(data_redundancy$U ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors af

uni0_pcnm <- rda(data_redundancy$U ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para af
step_pcnm_uni <- ordistep(uni0_pcnm, scope=formula(uni_pcnm))

#Ver pcnm significativos
step_pcnm_uni$anova 

# create pcnm table with only significant axes
n_uni<-paste('PCNM', c(1,8,7,5,6,15,40,4,16,35,10,24,13), sep='')
n_uni
uni_pcnm_sub <- parcelas_pcnm$vectors[,n_rao]


# Forward selection de variables ambientales ------------------------------

#Variables caracteristicas suelo las variables que suman 100 pueden generar
#error

forward.sel(data_redundancy$U, data_environmet_parcelas,
            alpha = 0.01)

forward.sel(data_redundancy$U, data_environmet_clima,
            alpha = 0.01)

forward.sel(data_redundancy$U, data_environmet_topo,
            alpha = 0.01)

# Modelo uni---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela
#     ( CLAY Mg Ca )

# 2) Clima: PRECDRIEST TEMP

#3) Topo:ELEV

# 4) space ('parcelas_pcnm')


uni_var <- varpart(data_redundancy,
                   data_environmet_parcelas[,c("CLAY")],
                   data_environmet_topo[,c("ELEV")],
                   data_environmet_clima[,c("PRECDRIEST","TEMP")],
                   rao_pcnm_sub)
redundancy_var
plot(redundancy_var)

uni_var
plot(uni_var)

rao_var
plot(rao_var)


