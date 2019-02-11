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

data_cwm <- read.csv("scripts/statistical_analysis/partition_variation/data/data_cwm.csv")

# Data Variables ambientales ----------------------------------------------
data_environmet_fisicas <- read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_suelos.csv")
data_environmet_topo <- read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_topo.csv")
data_environmet_quimicas <- read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_quimico.csv")
data_environmet_clima <- read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_clima.csv")

# Data Coordenadas de las parcelas ----------------------------------------
#Extraer data coordenadas y quitarlas del data set data_cwm
#Y transformarlas a log 10 usar CRTM

data_coor_parcelas <- read.csv("scripts/statistical_analysis/partition_variation/data/data_coor_parcelas.csv")


# Calcular PCNMs a partir de una matriz de distancia euclidea -------------

#Principal Coordinates of Neighbour Matrices (PCNMs) generate a dataframe 
#containing variables that represent different spatial scales

#This represent the spatial distributions of samples
#las coordenadas geográficas (descompuestas en un análisis previo de PCNM 
#con 21 descriptores espaciales - autovectores del PCNM - ).


parcelas_pcnm <- pcnm(dist(data_coor_parcelas))


# Seleccionar pcnms significativos  --------------------------------------

# Model con all predictors af

cwm_af_pcnm <- rda(data_cwm$cwm_af ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors af

cwm_af0_pcnm <- rda(data_cwm$cwm_af ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para af
step_pcnm_af <- ordistep(cwm_af0_pcnm, scope=formula(cwm_af_pcnm))

#Ver pcnm significativos
step_pcnm_af$anova 

# create pcnm table with only significant axes
n_af<-paste('PCNM', c(1,4,8,15,10,3,13,40,5,42,35,6,24,7), sep='')
n_af
cwm_af_pcnm_sub <- parcelas_pcnm$vectors[,n_af]


# Forward selection de variables ambientales ------------------------------

#Variables caracteristicas suelo las variables que suman 100 pueden generar
#error

forward.sel(data_cwm$cwm_af, data_environmet_fisicas,
              alpha = 0.01)

forward.sel(data_cwm$cwm_af, data_environmet_clima,
            alpha = 0.01)
            
forward.sel(data_cwm$cwm_af, data_environmet_quimicas,
            alpha = 0.01)
            
forward.sel(data_cwm$cwm_af, data_environmet_topo,
            alpha = 0.01)
            


# Modelo af ---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela
#     ( CLAY )

# 2) Caracteristicas quimicas de la parcela
#    (organic matter)

# 3) Clima: TEMP, PRECDRIEST

# 4) space ('parcelas_pcnm')


cwm_var_af <- varpart(data_cwm$cwm_afe,
                   data_environmet_fisicas[,c("CLAY")],
                   data_environmet_quimicas[,c("OrganicMatter")],
                   data_environmet_clima[,c("TEMP","PRECDRIEST")],
                   cwm_af_pcnm_sub)

plot(cwm_var_af)



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
n<-paste('PCNM', c(), sep='')
cwm_afe_pcnm_sub <- parcelas_pcnm$vectors[,n]


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

cwm_var <- varpart(data_cwm$cwm_afe,
                    data_environmet[,c("TEMP","Mg")], 
                                        parcelas_pcnm$vectors[,n])

varpart
cwm_var
