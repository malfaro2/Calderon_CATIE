rm(list = ls())
set.seed(123)

# Variation Partitioning - incorporating spatial processes ----------------

# Objetivo ----------------------------------------------------------------
# Se realizará el procedimiento de partition variation para los indices de 
#redundancia, uniqueness y Rao 

#Este ejemplo fue tomado de www.hiercourse.com/docs/Rnotes_multivariate.pdf

# Paquetes ----------------------------------------------------------------
library(vegan)
library(adespatial)

# Cargar datos ------------------------------------------------------------

# Data redundancy-Rao-Uniqueness ------------------------------------------

data_redundancy <- 
read.csv("data/resultados_csv/data_redundancy_resptraits.csv")

#Cleanig data redundancy
head(data_redundancy)
data_redundancy <- data_redundancy[,-c(1)]

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
data_redundancy <- data.frame(scale(data_redundancy))
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

# Varpart Redundancia funcional -------------------------------------------
# Seleccionar pcnms significativos  ---------------------------------------
# Model con all predictors para redundancy

redundancy_pcnm <- rda(data_redundancy$redundancy ~ ., 
                       data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors para redundancy

redundancy0_pcnm <- rda(data_redundancy$redundancy ~ 1, 
                        data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para redundancy
step_pcnm_redundnacy <- ordistep(redundancy0_pcnm, 
                                 scope=formula(redundancy_pcnm),
                                 permutations = how(nperm = 2000))

#Ver pcnm significativos
step_pcnm_redundnacy$anova 

# create pcnm table with only significant axes 
n_redundancy<-paste('PCNM', c(7,3,8,15,10,4,48,34,), sep='')
n_redundancy
redundancy_pcnm_sub <- parcelas_pcnm$vectors[,n_redundancy]


# Forward selection de variables ambientales redundancy -------------------

forward.sel(data_redundancy$redundancy, data_environmet_parcelas,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy$redundancy, data_environmet_clima,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy$redundancy, data_environmet_topo,
            alpha = 0.01,nperm = 2000)

# Data para qeco ----------------------------------------------------------

data_espacio_qeco <- redundancy_pcnm_sub 
TEMPSD <- data_environmet_clima[,c("TEMPSD")]
redundancy <- data_redundancy$redundancy

data_redundancy_qeco <- cbind(data_espacio_qeco,
                        TEMPSD,
                        redundancy)

#View(data_redundancy_qeco)

write.csv(data_redundancy_qeco,"data/resultados_csv/varpart/response_traits/data_redundancy_qeco.csv")

# Modelo redundancy ---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: CLAY 

# 2) Clima: PRECDRIEST, TEMP

#3) Topo:ELEV

# 4) space ('parcelas_pcnm')

redundancy_var <- varpart(data_redundancy$redundancy,
                      data_environmet_parcelas[,c("CLAY")],
                      data_environmet_clima[,c("PRECDRIEST","TEMP")],
                      data_environmet_topo[,c("ELEV")],
                      redundancy_pcnm_sub)
redundancy_var

# Probando la significancia  ----------------------------------------------

# significance of partition X1: CLAY
anova(rda(data_redundancy$redundancy ~ data_environmet_parcelas$CLAY +
             
             Condition(data_environmet_clima$PRECDRIEST) + 
             Condition(data_environmet_clima$TEMP) +
             Condition(data_environmet_topo$ELEV) + 
             Condition(redundancy_pcnm_sub)))
         
        
# significance of partition X2: PRECDRIEST, TEMP
anova(rda(data_redundancy$redundancy ~ data_environmet_clima$PRECDRIEST+
            data_environmet_clima$TEMP+
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_topo$ELEV) + 
            Condition(redundancy_pcnm_sub)))

# significance of partition X3: ELEV
anova(rda(data_redundancy$redundancy ~ data_environmet_topo$ELEV +
            
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(redundancy_pcnm_sub)))

# significance of partition X4: space ('parcelas_pcnm')
anova(rda(data_redundancy$redundancy ~  redundancy_pcnm_sub +
            
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_topo$ELEV)))



rm(redundancy0_pcnm,redundancy_pcnm,redundancy_pcnm_sub,redundancy_var,
   step_pcnm_redundnacy, n_redundancy)


# Varpart Rao -------------------------------------------------------------
# Seleccionar pcnms significativos  --------------------------------------

# Model con all predictors RAO

rao_pcnm <- rda(data_redundancy$Q ~ ., 
                data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors RAO

rao0_pcnm <- rda(data_redundancy$Q ~ 1, 
                 data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para RAO
step_pcnm_rao <- ordistep(rao0_pcnm, scope=formula(rao_pcnm),
                          permutations = how(nperm = 2000))

#Ver pcnm significativos
step_pcnm_rao$anova 

# create pcnm table with only significant axes sin el 1
n_rao<-paste('PCNM', c(7,48,29,3,15,34), sep='')
n_rao
rao_pcnm_sub <- parcelas_pcnm$vectors[,n_rao]


# Forward selection de variables ambientales ------------------------------

forward.sel(data_redundancy$Q, data_environmet_parcelas,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy$Q, data_environmet_clima,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy$Q, data_environmet_topo,
            alpha = 0.01,nperm = 2000)
# Data para qeco ----------------------------------------------------------

pH <- data_environmet_parcelas[,c("pH")]
PRECDRIEST_PREC <- data_environmet_clima[,c("PREC","PRECDRIEST")]
rao <- data_redundancy$Q
data_espacio_qeco <- rao_pcnm_sub

data_rao_qeco <- cbind(data_espacio_qeco,
                       pH,
                       PRECDRIEST_PREC,
                       rao)
#View(data_rao_qeco)

write.csv(data_rao_qeco,"data/resultados_csv/varpart/response_traits/data_rao_qeco.csv")

# Modelo rao---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: CLAY Mg Ca

# 2) Clima: TEMP PRECDRIEST 

# 3) Topo:ELEV

# 4) space ('parcelas_pcnm')

rao_var <- varpart(data_redundancy$Q,
                          data_environmet_parcelas[,c("CLAY","Mg","Ca")],
                          data_environmet_clima[,c("PRECDRIEST","TEMP")],
                          data_environmet_topo[,c("ELEV")],
                          rao_pcnm_sub)

rao_var

# Probando la significancia  ----------------------------------------------

# significance of partition X1: CLAY Mg
anova(rda(data_redundancy$Q ~ data_environmet_parcelas$CLAY +
            data_environmet_parcelas$Mg +
            
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_topo$ELEV) + 
            Condition(rao_pcnm_sub)))


# significance of partition X2: PRECDRIEST, TEMP
anova(rda(data_redundancy$Q ~   data_environmet_clima$PRECDRIEST +
             data_environmet_clima$TEMP +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$Mg) +
            Condition(data_environmet_topo$ELEV) + 
            Condition(rao_pcnm_sub)))

# significance of partition X3: ELEV
anova(rda(data_redundancy$Q ~   data_environmet_topo$ELEV +
             
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$Mg) +
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(rao_pcnm_sub)))

# significance of partition X4: space ('parcelas_pcnm')
anova(rda(data_redundancy$Q ~  rao_pcnm_sub  +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$Mg) +
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_topo$ELEV)))


rm(rao_pcnm,rao0_pcnm,rao_var,rao_pcnm_sub,step_pcnm_rao, 
   n_rao)

# Varpart Uniqueness ------------------------------------------------------
# Seleccionar pcnms significativos  --------------------------------------

# Model con all predictors uniqueness

uni_pcnm <- rda(data_redundancy$U ~ ., 
                data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors uniqueness

uni0_pcnm <- rda(data_redundancy$U ~ 1, 
                 data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para uniqueness
step_pcnm_uni <- ordistep(uni0_pcnm, scope=formula(uni_pcnm),
                          permutations = how(nperm = 2000))

#Ver pcnm significativos
 step_pcnm_uni$anova 

# create pcnm table with only significant axes
n_uni<-paste('PCNM', c(7,3,8,15,48,10,4,34,5), sep='')
n_uni
uni_pcnm_sub <- parcelas_pcnm$vectors[,n_uni]

# Forward selection de variables ambientales ------------------------------

forward.sel(data_redundancy$U, data_environmet_parcelas,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy$U, data_environmet_clima,
            alpha = 0.01,nperm = 2000)

forward.sel(data_redundancy$U, data_environmet_topo,
            alpha = 0.01,nperm = 2000)

# Data para qeco ----------------------------------------------------------

data_espacio_qeco <- uni_pcnm_sub 
TEMPSD <- data_environmet_clima[,c("TEMPSD")]
uniqueness <- data_redundancy$U

data_uniq_qeco <- cbind(data_espacio_qeco,
                       TEMPSD,
                       uniqueness)

#View(data_uniq_qeco)

write.csv(data_uniq_qeco,"data/resultados_csv/varpart/response_traits/data_uniq_qeco.csv")


# Modelo Uniqueness -------------------------------------------------------

# do predictor matrices explain community composition, and how much?
# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: Clay 

# 2) Clima: PRECDRIEST TEMP

# 3) Topo:ELEV

# 4) space ('parcelas_pcnm')

uni_var <- varpart(data_redundancy$U,
                   data_environmet_parcelas[,c("CLAY")],
                   data_environmet_clima[,c("PRECDRIEST","TEMP")],
                   data_environmet_topo[,c("ELEV")],
                   uni_pcnm_sub)


uni_var


# Probando la significancia  ----------------------------------------------

# significance of partition X1: CLAY 
anova(rda(data_redundancy$U ~ data_environmet_parcelas$CLAY +
            
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_topo$ELEV) + 
            Condition(uni_pcnm_sub)))


# significance of partition X2: PRECDRIEST, TEMP
anova(rda(data_redundancy$U ~ data_environmet_clima$PRECDRIEST +
            data_environmet_clima$TEMP +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_topo$ELEV) + 
            Condition(uni_pcnm_sub)))

# significance of partition X3: ELEV
anova(rda(data_redundancy$U ~ data_environmet_topo$ELEV +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(uni_pcnm_sub)))

# significance of partition X4: space ('parcelas_pcnm')
anova(rda(data_redundancy$U ~ uni_pcnm_sub +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_topo$ELEV)))


rm(uni_pcnm,uni0_pcnm,uni_var,uni_pcnm_sub,step_pcnm_uni, 
   n_uni)

