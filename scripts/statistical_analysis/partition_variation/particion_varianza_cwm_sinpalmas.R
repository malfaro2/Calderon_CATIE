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

data_cwm_sp <- 
  read.csv("data/resultados_csv/data_cwm_sinpalmas_coord.csv")

#eliminar columnas
data_cwm_sp <- data_cwm_sp[,-c(1:4)]

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

cwm_pcnm_sp <- rda(data_cwm_sp ~ ., 
                data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors para redundancy

cwm0_pcnm_sp <- rda(data_cwm_sp ~ 1, 
                 data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para redundancy
cwm_step_pcnm_sp <- ordistep(cwm0_pcnm_sp, 
                          scope=formula(cwm_pcnm_sp))

#Ver pcnm significativos
cwm_step_pcnm_sp$anova 

# create pcnm table with only significant axes y se quita el 1
n_cwm_sp<-paste('PCNM', c(7,10,8,4,5), sep='')
n_cwm_sp
cwm_pcnm_sub_sp <- parcelas_pcnm$vectors[,n_cwm_sp]


# Forward selection de variables ambientales redundancy -------------------

forward.sel(data_cwm_sp, data_environmet_parcelas,
            alpha = 0.01)

forward.sel(data_cwm_sp, data_environmet_clima,
            alpha = 0.01)

forward.sel(data_cwm_sp, data_environmet_topo,
            alpha = 0.01)


# Modelo CWM sin palmas ---------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: CLAY 

# 2) Clima: TEMP

#3) Topo:ELEV

# 4) space (cwm_pcnm_sub_sp )

cwm_var_sp <- varpart(data_cwm_sp,
                   data_environmet_parcelas[,c("CLAY")],
                   data_environmet_clima[,c("TEMP")],
                   data_environmet_topo[,c("ELEV")],
                   cwm_pcnm_sub_sp)
cwm_var_sp

# Probando la significancia  ----------------------------------------------

# significance of partition X1: CLAY
anova(rda(data_cwm_sp ~ data_environmet_parcelas$CLAY +
            
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_topo$ELEV) + 
            Condition(cwm_pcnm_sub_sp)))


# significance of partition X2: TEMP
anova(rda(data_cwm_sp ~ data_environmet_clima$TEMP +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_topo$ELEV) + 
            Condition(cwm_pcnm_sub_sp)))

# significance of partition X3: ELEV
anova(rda(data_cwm_sp ~ data_environmet_topo$ELEV +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(cwm_pcnm_sub_sp)))

# significance of partition X4: space (cwm_pcnm_sub_sp)
anova(rda(data_cwm_sp ~  cwm_pcnm_sub_sp +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_topo$ELEV)))



rm(cwm0_pcnm_sp,cwm_pcnm_sp,cwm_pcnm_sub_sp,cwm_var_sp,
   cwm_step_pcnm_sp, n_cwm_sp)
