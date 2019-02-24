rm(list = ls())
set.seed(123)

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
read.csv("data/resultados_csv/data_cwm_coord.csv")

#eliminar af
data_cwm <- data_cwm[,-c(1:5)]

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
data_cwm <- data.frame(scale(data_cwm))
data_environmet_topo <- data.frame(scale(data_environmet_topo))
data_environmet_parcelas <- data.frame(scale(data_environmet_parcelas))
data_environmet_clima <- data.frame(scale(data_environmet_clima))
data_coor_parcelas <- data.frame(scale(data_coor_parcelas))


# Calcular PCNMs a partir de una matriz de distancia euclidea -------------
#Principal Coordinates of Neighbour Matrices (PCNMs) generate a dataframe 
#containing variables that represent different spatial scales

#This represent the spatial distributions of samples
#las coordenadas geográficas (descompuestas en un análisis previo de PCNM 
#con 21 descriptores espaciales - autovectores del PCNM - ).

parcelas_pcnm <- pcnm(dist(data_coor_parcelas))

#Sacar pcnm 1
parcelas_pcnm$vectors <- parcelas_pcnm$vectors[,-(1)]

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
                                 scope=formula(cwm_pcnm),
                          permutations = how(nperm = 2000))

#Ver pcnm significativos
cwm_step_pcnm$anova 

# create pcnm table with only significant axes 
n_cwm<-paste('PCNM', c(7,4,5,21,3,18,8), sep='')
n_cwm
cwm_pcnm_sub <- parcelas_pcnm$vectors[,n_cwm]


# Forward selection de variables ambientales redundancy -------------------

forward.sel(data_cwm, data_environmet_parcelas,
            alpha = 0.01,nperm = 2000)

forward.sel(data_cwm, data_environmet_clima,
            alpha = 0.01,nperm = 2000)

forward.sel(data_cwm, data_environmet_topo,
            alpha = 0.01,nperm = 2000)


# Data para qeco ----------------------------------------------------------

data_espacio_qeco <- cwm_pcnm_sub 
data_clima_qeco <- data_environmet_clima[,c("TEMP","PREC","PRECCV", "PRECDRIEST")]
data_parcelas_qeco <- data_environmet_parcelas[,c("CLAY","OrganicMatter")]
ELEV <- data_environmet_topo[,c("ELEV")]
data_cwm_qeco <- data_cwm

data_cwm_qeco <- cbind(data_espacio_qeco,
                       data_clima_qeco, 
                       data_parcelas_qeco,
                       ELEV,
                       data_cwm_qeco)

View(data_cwm_qeco)

write.csv(data_cwm_qeco,"data/resultados_csv/varpart/data_cwm_qeco.csv")

# Modelo CWM --------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: CLAY OrganicMatter

# 2) Clima:  TEMP,PREC,PRECCV, PRECDRIEST

#3) Topo:ELEV

# 4) space (cwm_pcnm_sub )

cwm_var <- varpart(data_cwm,
                    data_environmet_parcelas[,c("CLAY","OrganicMatter")],
                    data_environmet_clima[,c("TEMP","PREC","PRECCV","PRECDRIEST")],
                    data_environmet_topo[,c("ELEV")],
                    cwm_pcnm_sub)


# Probando la significancia  ----------------------------------------------

#Fractions
# significance of partition X1: CLAY OrganicMatter
(x1 <- anova(rda(data_cwm ~ data_environmet_parcelas$CLAY + 
                      data_environmet_parcelas$OrganicMatter +
                      
                      (data_environmet_clima$TEMP) +
                      (data_environmet_clima$PREC) +
                      (data_environmet_clima$PRECCV)+  
                      (data_environmet_clima$PRECDRIEST) + 
                      (data_environmet_topo$ELEV) + 
                      ( cwm_pcnm_sub))))

# significance of partition X2:  TEMP,PREC,PRECCV, PRECDRIEST
(x2 <- anova(rda(data_cwm ~ data_environmet_clima$TEMP +
                      data_environmet_clima$PREC +
                      data_environmet_clima$PRECCV +
                      data_environmet_clima$PRECDRIEST+
                      
                      (data_environmet_parcelas$CLAY) + 
                      (data_environmet_parcelas$OrganicMatter)+
                      (data_environmet_topo$ELEV) + 
                      (cwm_pcnm_sub))))

# significance of partition X3: ELEV
(x3 <- anova(rda(data_cwm ~ data_environmet_topo$ELEV +
                      
                      (data_environmet_clima$TEMP) +
                      (data_environmet_clima$PREC) +
                      (data_environmet_clima$PRECCV)+  
                      (data_environmet_clima$PRECDRIEST) +
                      (data_environmet_parcelas$CLAY) + 
                      (data_environmet_parcelas$OrganicMatter)+
                      (cwm_pcnm_sub))))

# significance of partition X4: space (cwm_pcnm_sub)
(x4 <- anova(rda(data_cwm ~ cwm_pcnm_sub +
                      
                      (data_environmet_clima$TEMP) +
                      (data_environmet_clima$PREC) +
                      (data_environmet_clima$PRECCV)+  
                      (data_environmet_clima$PRECDRIEST) +
                      (data_environmet_parcelas$CLAY) + 
                      (data_environmet_parcelas$OrganicMatter)+
                      (data_environmet_topo$ELEV))))


#Individual fractions 
# significance of partition X1: CLAY OrganicMatter
(x1ind <- anova(rda(data_cwm ~ data_environmet_parcelas$CLAY + 
                     data_environmet_parcelas$OrganicMatter +
            
            Condition(data_environmet_clima$TEMP) +
            Condition(data_environmet_clima$PREC) +
            Condition(data_environmet_clima$PRECCV)+  
            Condition(data_environmet_clima$PRECDRIEST) + 
            Condition(data_environmet_topo$ELEV) + 
            Condition( cwm_pcnm_sub))))

# significance of partition X2:  TEMP,PREC,PRECCV, PRECDRIEST
(x2ind <- anova(rda(data_cwm ~ data_environmet_clima$TEMP +
            data_environmet_clima$PREC +
            data_environmet_clima$PRECCV +
            data_environmet_clima$PRECDRIEST+
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$OrganicMatter)+
            Condition(data_environmet_topo$ELEV) + 
            Condition(cwm_pcnm_sub))))

# significance of partition X3: ELEV
(x3ind <- anova(rda(data_cwm ~ data_environmet_topo$ELEV +
                     
                     Condition(data_environmet_clima$TEMP) +
                     Condition(data_environmet_clima$PREC) +
                     Condition(data_environmet_clima$PRECCV)+  
                     Condition(data_environmet_clima$PRECDRIEST) +
                     Condition(data_environmet_parcelas$CLAY) + 
                     Condition(data_environmet_parcelas$OrganicMatter)+
                     Condition(cwm_pcnm_sub))))

# significance of partition X4: space (cwm_pcnm_sub)
(x4ind <- anova(rda(data_cwm ~ cwm_pcnm_sub +
                     
                     Condition(data_environmet_clima$TEMP) +
                     Condition(data_environmet_clima$PREC) +
                     Condition(data_environmet_clima$PRECCV)+  
                     Condition(data_environmet_clima$PRECDRIEST) +
                     Condition(data_environmet_parcelas$CLAY) + 
                     Condition(data_environmet_parcelas$OrganicMatter)+
                     Condition(data_environmet_topo$ELEV))))


# Resultados --------------------------------------------------------------
(frac <- cwm_var$part$fract[1:4,])

#Pvalues fractions
(pvalues_frac <- rbind(x1[1,c(3,4)],x2[1,c(3,4)],x3[1,c(3,4)],x4[1,c(3,4)]))
(row.names(pvalues_frac) <- c("X1","X2","X3","X4"))

#Pvalues individuals fractions
pvalues_indfrac <- rbind(x1ind[1,c(3,4)],x2ind[1,c(3,4)],
                         x3ind[1,c(3,4)],x4ind[1,c(3,4)])

row.names(pvalues_indfrac) <- c("indX1","indX2","indX3","indX4")
(indfrac <- cwm_var$part$indfract[1:4,])
cbind(indfrac, pvalues_indfrac)


