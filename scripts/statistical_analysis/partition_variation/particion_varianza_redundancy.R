rm(list = ls())

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

data_redundancy <- read.csv("scripts/statistical_analysis/partition_variation/data/data_redundancy.csv")

# Data Variables ambientales ----------------------------------------------
data_environmet_topo <- read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_topo.csv")
data_environmet_parcelas <- read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_quimico.csv")
data_environmet_clima <- read.csv("scripts/statistical_analysis/partition_variation/data/data_carac_clima.csv")


# Data log coordenadas de las parcelas  -----------------------------------

data_coor_parcelas <- read.csv("scripts/statistical_analysis/partition_variation/data/data_coor_parcelas.csv")


# Calcular PCNMs a partir de una matriz de distancia euclidea -------------
#Principal Coordinates of Neighbour Matrices (PCNMs) generate a dataframe 
#containing variables that represent different spatial scales

#This represent the spatial distributions of samples
#las coordenadas geográficas (descompuestas en un análisis previo de PCNM 
#con 21 descriptores espaciales - autovectores del PCNM - ).

parcelas_pcnm <- pcnm(dist(data_coor_parcelas))


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
                                 scope=formula(redundancy_pcnm))

#Ver pcnm significativos
step_pcnm_redundnacy$anova 

# create pcnm table with only significant axes y se quita el 1
n_redundancy<-paste('PCNM', c(8,7,6,5,40,4,15,35,16,10,24,54,13), sep='')
n_redundancy
redundancy_pcnm_sub <- parcelas_pcnm$vectors[,n_redundancy]


# Forward selection de variables ambientales redundancy -------------------

forward.sel(data_redundancy$redundancy, data_environmet_parcelas,
            alpha = 0.01)

forward.sel(data_redundancy$redundancy, data_environmet_clima,
            alpha = 0.01)

forward.sel(data_redundancy$redundancy, data_environmet_topo,
            alpha = 0.01)


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

rao_pcnm <- rda(data_redundancy$Q ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors RAO

rao0_pcnm <- rda(data_redundancy$Q ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para RAO
step_pcnm_rao <- ordistep(rao0_pcnm, scope=formula(rao_pcnm))

#Ver pcnm significativos
step_pcnm_rao$anova 

# create pcnm table with only significant axes sin el 1
n_rao<-paste('PCNM', c(8,4,35,15,10,5,6,40,7,13,16), sep='')
n_rao
rao_pcnm_sub <- parcelas_pcnm$vectors[,n_rao]


# Forward selection de variables ambientales ------------------------------

forward.sel(data_redundancy$Q, data_environmet_parcelas,
            alpha = 0.01)

forward.sel(data_redundancy$Q, data_environmet_clima,
            alpha = 0.01)

forward.sel(data_redundancy$Q, data_environmet_topo,
            alpha = 0.01)

# Modelo rao---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: CLAY Mg

# 2) Clima: PRECDRIEST TEMP

# 3) Topo:ELEV

# 4) space ('parcelas_pcnm')

rao_var <- varpart(data_redundancy$Q,
                          data_environmet_parcelas[,c("CLAY","Mg")],
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

uni_pcnm <- rda(data_redundancy$U ~ ., data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors uniqueness

uni0_pcnm <- rda(data_redundancy$U ~ 1, data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para uniqueness
step_pcnm_uni <- ordistep(uni0_pcnm, scope=formula(uni_pcnm))

#Ver pcnm significativos
step_pcnm_uni$anova 

# create pcnm table with only significant axes
n_uni<-paste('PCNM', c(8,7,5,6,15,40,4,16,35,10,24,13), sep='')
n_uni
uni_pcnm_sub <- parcelas_pcnm$vectors[,n_uni]


# Forward selection de variables ambientales ------------------------------

forward.sel(data_redundancy$U, data_environmet_parcelas,
            alpha = 0.01)

forward.sel(data_redundancy$U, data_environmet_clima,
            alpha = 0.01)

forward.sel(data_redundancy$U, data_environmet_topo,
            alpha = 0.01)


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

# significance of partition X1: CLAY P
anova(rda(data_redundancy$U ~ data_environmet_parcelas$CLAY +
            data_environmet_parcelas$P+
            
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_topo$ELEV) + 
            Condition(uni_pcnm_sub)))


# significance of partition X2: PRECDRIEST, TEMP
anova(rda(data_redundancy$U ~ data_environmet_clima$PRECDRIEST +
            data_environmet_clima$TEMP +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$P) +
            Condition(data_environmet_topo$ELEV) + 
            Condition(uni_pcnm_sub)))

# significance of partition X3: ELEV
anova(rda(data_redundancy$U ~ data_environmet_topo$ELEV +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$P) +
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(uni_pcnm_sub)))

# significance of partition X4: space ('parcelas_pcnm')
anova(rda(data_redundancy$U ~ uni_pcnm_sub +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$P) +
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_topo$ELEV)))


rm(uni_pcnm,uni0_pcnm,uni_var,uni_pcnm_sub,step_pcnm_uni, 
   n_uni)

