rm(list = ls())
set.seed(123)

# Variation Partitioning - incorporating spatial processes ----------------

# Objetivo ----------------------------------------------------------------
# Se realizará el procedimiento de partition variation para los indices de 
#redundancia, uniqueness y Rao con los datos sin palmas.

#Este ejemplo fue tomado de www.hiercourse.com/docs/Rnotes_multivariate.pdf

# Paquetes ----------------------------------------------------------------
library(vegan)
library(adespatial)

# Cargar datos ------------------------------------------------------------

# Data redundancy-Rao-Uniqueness ------------------------------------------

data_redundancy_sinpalmas <- 
  read.csv("data/resultados_csv/data_redundancy_resptraits_sinpalmas.csv")

#Eliminar columnas
data_redundancy_sinpalmas <- data_redundancy_sinpalmas[,-c(1)]
head(data_redundancy_sinpalmas)


# Data Variables ambientales ----------------------------------------------
data_environmet_topo <- 
  read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_topo.csv")

data_environmet_parcelas <- 
  read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_quimico.csv")

data_environmet_clima <- 
  read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_clima.csv")


# Data log coordenadas de las parcelas  -----------------------------------

data_coor_parcelas <- 
  read.csv("scripts/statistical_analysis/partition_variation/data/data_coor_parcelas_log.csv")

# Scale variables ---------------------------------------------------------
data_redundancy_sinpalmas <- data.frame(scale(data_redundancy_sinpalmas))
data_environmet_topo <- data.frame(scale(data_environmet_topo))
data_environmet_parcelas <- data.frame(scale(data_environmet_parcelas))
data_environmet_clima <- data.frame(scale(data_environmet_clima))

# Calcular PCNMs a partir de una matriz de distancia euclidea -------------
#Principal Coordinates of Neighbour Matrices (PCNMs) generate a dataframe 
#containing variables that represent different spatial scales

#This represent the spatial distributions of samples
#las coordenadas geográficas (descompuestas en un análisis previo de PCNM 
#con 21 descriptores espaciales - autovectores del PCNM - ).

parcelas_pcnm <- pcnm(dist(data_coor_parcelas))

#Sacar pcnm 1
parcelas_pcnm$vectors <- parcelas_pcnm$vectors[,-(1)]

# Varpart Redundancia funcional sin palmas --------------------------------
# Seleccionar pcnms significativos  ---------------------------------------
# Model con all predictors para redundancy

redundancy_pcnm_sp <- rda(data_redundancy_sinpalmas$redundancy ~ ., 
                       data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors para redundancy

redundancy0_pcnm_sp <- rda(data_redundancy_sinpalmas$redundancy ~ 1, 
                        data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para redundancy
step_pcnm_redundnacy_sp <- ordistep(redundancy0_pcnm_sp, 
                                 scope=formula(redundancy_pcnm_sp),
                                 permutations = how(nperm = 2000))

#Ver pcnm significativos
step_pcnm_redundnacy_sp$anova 

# create pcnm table with only significant axes y se quita el 1
n_redundancy_sp<-paste('PCNM', c(7,3,29,48,8,43,34,15,46), sep='')
n_redundancy_sp
redundancy_pcnm_sub_sp <- parcelas_pcnm$vectors[,n_redundancy_sp]


# Forward selection de variables ambientales redundancy -------------------

forward.sel(data_redundancy_sinpalmas$redundancy, data_environmet_parcelas,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy_sinpalmas$redundancy, data_environmet_clima,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy_sinpalmas$redundancy, data_environmet_topo,
            alpha = 0.01,nperm = 2000)
# Data para qeco ----------------------------------------------------------

data_espacio_qeco <- redundancy_pcnm_sub_sp

TEMPSD <- data_environmet_clima[,c("TEMPSD")]
redundancy_sp <- data_redundancy_sinpalmas$redundancy

data_redundancy_sp_qeco <- cbind(data_espacio_qeco,
                              TEMPSD,
                              redundancy_sp)

#View(data_redundancy_sp_qeco)

write.csv(data_redundancy_sp_qeco,"data/resultados_csv/varpart/response_traits/data_redundancy_sp_qeco.csv")


# Modelo redundancy ---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: Ninguna 

# 2) Clima: PRECCV

#3) Topo:Ninguna

# 4) space (redundancy_pcnm_sub_sp)

redundancy_sinpalmas_var <- varpart(data_redundancy_sinpalmas$redundancy,
                          
                          data_environmet_clima[,c("PRECCV")],
                          
                          redundancy_pcnm_sub_sp)
redundancy_sinpalmas_var

# Probando la significancia  ----------------------------------------------

# significance of partition X1: PRECCV
anova(rda(data_redundancy_sinpalmas$redundancy ~data_environmet_clima$PRECCV +
            Condition(redundancy_pcnm_sub_sp)))


# significance of partition X2: Espacial
anova(rda(data_redundancy_sinpalmas$redundancy ~ redundancy_pcnm_sub_sp +
            Condition(data_environmet_clima$PRECCV)))


rm(redundancy0_pcnm_sp,redundancy_pcnm_sp,redundancy_pcnm_sub_sp,
   redundancy_sinpalmas_var,step_pcnm_redundnacy_sp, n_redundancy_sp)


# Varpart Rao sin palmas --------------------------------------------------
# Seleccionar pcnms significativos  --------------------------------------

# Model con all predictors RAO

rao_pcnm_sp <- rda(data_redundancy_sinpalmas$Q ~ ., 
                   data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors RAO

rao0_pcnm_sp <- rda(data_redundancy_sinpalmas$Q ~ 1, 
                    data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para RAO
step_pcnm_rao_sp <- ordistep(rao0_pcnm_sp, scope=formula(rao_pcnm_sp),
                             permutations = how(nperm = 2000))

#Ver pcnm significativos
step_pcnm_rao_sp$anova 

# create pcnm table with only significant axes sin el 1
n_rao_sp<-paste('PCNM', c(29,5,13,38,32), sep='')
n_rao_sp
rao_pcnm_sub_sp <- parcelas_pcnm$vectors[,n_rao_sp]


# Forward selection de variables ambientales ------------------------------

forward.sel(data_redundancy_sinpalmas$Q, data_environmet_parcelas,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy_sinpalmas$Q, data_environmet_clima,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy_sinpalmas$Q, data_environmet_topo,
            alpha = 0.01,nperm = 2000)

# Data para qeco ----------------------------------------------------------

data_espacio_qeco <- rao_pcnm_sub_sp
rao_sp <- data_redundancy_sinpalmas$Q

data_rao_sp_qeco <- cbind(data_espacio_qeco,
                       rao_sp)
#View(data_rao_sp_qeco)

write.csv(data_rao_sp_qeco,"data/resultados_csv/varpart/response_traits/data_rao_sp_qeco.csv")


# Modelo rao---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: SAND

# 2) Clima: TEMPMIN PREC

# 3) Topo:ELEV

# 4) space (rao_pcnm_sub_sp)

rao_var_sp <- varpart(data_redundancy_sinpalmas$Q,
                   data_environmet_parcelas[,c("SAND")],
                   data_environmet_clima[,c("TEMPSD")],
                   data_environmet_topo[,c("ELEV")],
                   rao_pcnm_sub_sp)

rao_var_sp

# Probando la significancia  ----------------------------------------------

# significance of partition X1: SAND
anova(rda(data_redundancy_sinpalmas$Q ~ data_environmet_parcelas$SAND +

            Condition(data_environmet_clima$TEMPSD) +
            Condition(data_environmet_topo$ELEV) + 
            Condition(rao_pcnm_sub_sp)))


# significance of partition X2: TEMPSD
anova(rda(data_redundancy_sinpalmas$Q ~   data_environmet_clima$TEMPSD +
            
            Condition(data_environmet_parcelas$SAND) + 
            Condition(data_environmet_topo$ELEV) + 
            Condition(rao_pcnm_sub_sp)))

# significance of partition X3: ELEV
anova(rda(data_redundancy_sinpalmas$Q ~   data_environmet_topo$ELEV +
            
            Condition(data_environmet_parcelas$SAND) +
            Condition(data_environmet_clima$TEMPSD) +
            Condition(rao_pcnm_sub_sp)))

# significance of partition X4: space ('parcelas_pcnm')
anova(rda(data_redundancy_sinpalmas$Q ~ rao_pcnm_sub_sp   +
            
            Condition(data_environmet_parcelas$SAND) +
            Condition(data_environmet_clima$TEMPSD) +
            Condition(data_environmet_topo$ELEV)))


rm(rao_pcnm_sp,rao0_pcnm_sp,rao_var_sp,rao_pcnm_sub_sp,step_pcnm_rao_sp, 
   n_rao_sp)


# Varpart Uniqueness SIN PALMAS -------------------------------------------
# Seleccionar pcnms significativos  --------------------------------------

# Model con all predictors uniqueness

uni_pcnm_sp <- rda(data_redundancy_sinpalmas$U ~ ., 
                   data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors uniqueness

uni0_pcnm_sp <- rda(data_redundancy_sinpalmas$U ~ 1, 
                    data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para uniqueness
step_pcnm_uni_sp <- ordistep(uni0_pcnm_sp, scope=formula(uni_pcnm_sp),
                             permutations = how(nperm = 2000))

#Ver pcnm significativos
step_pcnm_uni_sp$anova 

# create pcnm table with only significant axes
n_uni_sp<-paste('PCNM', c(7,29,3,48,8,43,34,15), sep='')
n_uni_sp
uni_pcnm_sub_sp <- parcelas_pcnm$vectors[,n_uni_sp]


# Forward selection de variables ambientales ------------------------------

forward.sel(data_redundancy_sinpalmas$U, data_environmet_parcelas,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy_sinpalmas$U, data_environmet_clima,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy_sinpalmas$U, data_environmet_topo,
            alpha = 0.01,nperm = 2000)

# Data para qeco ----------------------------------------------------------

data_espacio_qeco <- uni_pcnm_sub_sp 
TEMPSD <- data_environmet_clima[,c("TEMPSD")]
uniqueness_sp <- data_redundancy_sinpalmas$U

data_uniq_sp_qeco <- cbind(data_espacio_qeco,
                        TEMPSD,
                        uniqueness_sp)


write.csv(data_uniq_sp_qeco,"data/resultados_csv/varpart/response_traits/data_uniq_sp_qeco.csv")
# Modelo Uniqueness -------------------------------------------------------

# do predictor matrices explain community composition, and how much?
# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: Ninguna

# 2) Clima: PRECCV

# 3) Topo:Ninguna

# 4) space (uni_pcnm_sub_sp)

uni_var_sp <- varpart(data_redundancy_sinpalmas$U,
                   
                   data_environmet_clima[,c("PRECCV")],
                   uni_pcnm_sub_sp)
uni_var_sp


# Probando la significancia  ----------------------------------------------

# significance of partition X1: PRECCV
anova(rda(data_redundancy_sinpalmas$U ~ data_environmet_clima$PRECCV +
            
            Condition(uni_pcnm_sub_sp)))


# significance of partition X2: PRECDRIEST, TEMP
anova(rda(data_redundancy_sinpalmas$U ~ uni_pcnm_sub_sp +
            
            Condition(data_environmet_clima$PRECCV)))



rm(uni_pcnm_sp,uni0_pcnm_sp,uni_var_sp,uni_pcnm_sub_sp,step_pcnm_uni_sp, 
   n_uni_sp)
