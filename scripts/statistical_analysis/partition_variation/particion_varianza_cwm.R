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

data_cwm <- 
read.csv("scripts/statistical_analysis/partition_variation/data/data_cwm.csv")

#eliminar af
data_cwm <- data_cwm[,-1]


# Scale variables ---------------------------------------------------------
data_cwm <- scale(data_cwm)


# Data Variables ambientales ----------------------------------------------
data_environmet_topo <- 
read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_topo.csv")


data_environmet_parcelas <- 
read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_quimico.csv")

data_environmet_clima <- 
read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_clima.csv")

# Data log coordenadas de las parcelas  -----------------------------------

data_coor_parcelas <- 
read.csv("scripts/statistical_analysis/partition_variation/data/data_coor_parcelas.csv")


# Calcular PCNMs a partir de una matriz de distancia euclidea -------------
#Principal Coordinates of Neighbour Matrices (PCNMs) generate a dataframe 
#containing variables that represent different spatial scales

#This represent the spatial distributions of samples
#las coordenadas geográficas (descompuestas en un análisis previo de PCNM 
#con 21 descriptores espaciales - autovectores del PCNM - ).

parcelas_pcnm <- pcnm(dist(data_coor_parcelas))

# Varpart CWM -------------------------------------------------------------
# Seleccionar pcnms significativos  ---------------------------------------
# Model con all predictors para CWM

cwm_pcnm <- rda(data_cwm ~ ., 
                       data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors para redundancy

cwm0_pcnm <- rda(data_cwm ~ 1, 
                        data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para redundancy
cwm_step_pcnm <- ordistep(cwm0_pcnm, 
                                 scope=formula(cwm_pcnm))

#Ver pcnm significativos
cwm_step_pcnm$anova 

# create pcnm table with only significant axes y se quita el 1
n_cwm<-paste('PCNM', c(4,2,6,16,5,9), sep='')
n_cwm
cwm_pcnm_sub <- parcelas_pcnm$vectors[,n_cwm]


# Forward selection de variables ambientales redundancy -------------------

forward.sel(data_cwm, data_environmet_parcelas,
            alpha = 0.01)

forward.sel(data_cwm, data_environmet_clima,
            alpha = 0.01)

forward.sel(data_cwm, data_environmet_topo,
            alpha = 0.01)

# Modelo CWM --------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: CLAY 

# 2) Clima: PRECDRIEST, TEMPMIN

#3) Topo:ELEV

# 4) space (cwm_pcnm_sub )

cwm_var <- varpart(data_cwm,
                          data_environmet_parcelas[,c("CLAY")],
                          data_environmet_clima[,c("PRECDRIEST","TEMPMIN")],
                          data_environmet_topo[,c("ELEV")],
                          cwm_pcnm_sub)
cwm_var

# Probando la significancia  ----------------------------------------------

# significance of partition X1: CLAY
anova(rda(data_cwm ~ data_environmet_parcelas$CLAY +
            
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMPMIN) +
            Condition(data_environmet_topo$ELEV) + 
            Condition( cwm_pcnm_sub)))


# significance of partition X2: PRECDRIEST, TEMP
anova(rda(data_cwm ~ data_environmet_clima$PRECDRIEST+
            data_environmet_clima$TEMPMIN+
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_topo$ELEV) + 
            Condition(cwm_pcnm_sub)))

# significance of partition X3: ELEV
anova(rda(data_cwm ~ data_environmet_topo$ELEV +
            
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(cwm_pcnm_sub)))

# significance of partition X4: space ('parcelas_pcnm')
anova(rda(data_cwm ~  cwm_pcnm_sub +
            
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_topo$ELEV)))



rm(cwm0_pcnm,cwm_pcnm,cwm_pcnm_sub,cwm_var,
   cwm_step_pcnm, n_cwm)

