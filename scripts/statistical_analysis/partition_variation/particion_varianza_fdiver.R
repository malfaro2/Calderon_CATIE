rm(list = ls())

# Variation Partitioning - incorporating spatial processes ----------------

# Objetivo ----------------------------------------------------------------
# Se realizará el procedimiento de partition variation para los indices de 
#fdis feve fdiv

#Este ejemplo fue tomado de www.hiercourse.com/docs/Rnotes_multivariate.pdf

# Paquetes ----------------------------------------------------------------
library(vegan)
library(adespatial)

# Cargar datos ------------------------------------------------------------


# Data Fiv-Feve-fdis ------------------------------------------------------

data_fdiver <- 
read.csv("data/resultados_csv/data_fdiversity_coord.csv")

#Eliminar columnas
data_fdiver <- data_fdiver[,-c(1:4)]

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


# Varpart Fdiv ------------------------------------------------------------
# Seleccionar pcnms significativos  ---------------------------------------
# Model con all predictors para fdiv

fdiv_pcnm <- rda(data_fdiver$fdiv ~ ., 
                       data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors para fdiv

fdiv0_pcnm <- rda(data_fdiver$fdiv ~ 1, 
                        data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para fdiv
fdiv_step_pcnm <- ordistep(fdiv0_pcnm, 
                                 scope=formula(fdiv_pcnm))

#Ver pcnm significativos
fdiv_step_pcnm$anova 

# create pcnm table with only significant axes y se quita el 1
n_fdiv <- paste('PCNM', c(10,8,3,15,13,4,35,9,36), sep='')
n_fdiv
fdiv_pcnm_sub <- parcelas_pcnm$vectors[,n_fdiv]


# Forward selection de variables ambientales redundancy -------------------

forward.sel(data_fdiver$fdiv, data_environmet_parcelas,
            alpha = 0.01)

forward.sel(data_fdiver$fdiv, data_environmet_clima,
            alpha = 0.01)

forward.sel(data_fdiver$fdiv, data_environmet_topo,
            alpha = 0.01)


# Modelo fdiv ---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: Mg 

# 2) Clima: TEMPSD , TEMPMIN

#3) Topo: Ninguna

# 4) space (fdiv_pcnm_sub)

fdiv_var <- varpart(data_fdiver$fdiv,
                          data_environmet_parcelas[,c("Mg")],
                          data_environmet_clima[,c("TEMPSD","TEMPMIN")],
                          fdiv_pcnm_sub)
fdiv_var

# Probando la significancia  ----------------------------------------------

# significance of partition X1: Mg
anova(rda(data_fdiver$fdiv ~ data_environmet_parcelas$Mg +
            
            Condition(data_environmet_clima$TEMPSD) + 
            Condition(data_environmet_clima$TEMPMIN) + 
            Condition(fdiv_pcnm_sub)))


# significance of partition X2: TEMPSD , TEMPMIN
anova(rda(data_fdiver$fdiv ~  + data_environmet_clima$TEMPSD+
            data_environmet_clima$TEMPMIN +
            
            Condition(data_environmet_parcelas$Mg) + 
            Condition(fdiv_pcnm_sub)))


# significance of partition X3: space (fdiv_pcnm_sub)
anova(rda(data_fdiver$fdiv ~ fdiv_pcnm_sub  +
            
            Condition(data_environmet_parcelas$Mg)+
            Condition(data_environmet_clima$TEMPSD) + 
            Condition(data_environmet_clima$TEMPMIN))) 


rm(fdiv0_pcnm,fdiv_pcnm,fdiv_pcnm_sub,fdiv_var,
   fdiv_step_pcnm, n_fdiv)


# Varpart Feve ------------------------------------------------------------
# Seleccionar pcnms significativos  ---------------------------------------
# Model con all predictors para feve

feve_pcnm <- rda(data_fdiver$feve ~ ., 
                 data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors para feve

feve0_pcnm <- rda(data_fdiver$feve ~ 1, 
                  data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para feve
feve_step_pcnm <- ordistep(feve0_pcnm, 
                           scope=formula(feve_pcnm))

#Ver pcnm significativos
feve_step_pcnm$anova 

# create pcnm table with only significant axes y se quita el 1
n_feve <- paste('PCNM', c(10,5,7,15), sep='')
n_feve
feve_pcnm_sub <- parcelas_pcnm$vectors[,n_feve]


# Forward selection de variables ambientales feve -------------------

forward.sel(data_fdiver$feve, data_environmet_parcelas,
            alpha = 0.01)

forward.sel(data_fdiver$feve, data_environmet_clima,
            alpha = 0.01)

forward.sel(data_fdiver$feve, data_environmet_topo,
            alpha = 0.01)


# Modelo feve ---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: Ca CLAY

# 2) Clima: TEMPSD , PRECCV

#3) Topo: ELEV

# 4) space (feve_pcnm_sub)

feve_var <- varpart(data_fdiver$feve,
                    data_environmet_parcelas[,c("Ca","CLAY")],
                    data_environmet_clima[,c("TEMPSD","PRECCV")],
                    data_environmet_topo[,"ELEV"],
                    feve_pcnm_sub)
feve_var

# Probando la significancia  ----------------------------------------------

# significance of partition X1: Ca CLAY
anova(rda(data_fdiver$feve ~ data_environmet_parcelas$Ca +
            data_environmet_parcelas$CLAY +
          
            Condition(data_environmet_clima$TEMPSD) + 
            Condition(data_environmet_clima$PRECCV) + 
            Condition(data_environmet_topo$ELEV) +
            Condition(feve_pcnm_sub)))


# significance of partition X2: TEMPSD , PRECCV 
anova(rda(data_fdiver$feve ~ data_environmet_clima$PRECCV  +
            data_environmet_clima$TEMPSD  +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$Ca) + 
            Condition(data_environmet_topo$ELEV) +
            Condition(feve_pcnm_sub)))

# significance of partition X3: ELEV
anova(rda(data_fdiver$feve ~ data_environmet_topo$ELEV +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$Ca) + 
            Condition(data_environmet_clima$TEMPSD)+
            Condition(data_environmet_clima$PRECCV) +
            Condition(feve_pcnm_sub)))

# significance of partition X4: space (feve_pcnm_sub)
anova(rda(data_fdiver$feve ~ feve_pcnm_sub +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$Ca) + 
            Condition(data_environmet_clima$TEMPSD)+
            Condition(data_environmet_clima$PRECCV) +
            Condition(data_environmet_topo$ELEV)))

rm(feve0_pcnm,feve_pcnm,feve_pcnm_sub,feve_var,
   feve_step_pcnm, n_feve)



# Varpart fdis ------------------------------------------------------------
# Seleccionar pcnms significativos  ---------------------------------------
# Model con all predictors para fdis

fdis_pcnm <- rda(data_fdiver$fdis ~ ., 
                 data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors para fdis

fdis0_pcnm <- rda(data_fdiver$fdis ~ 1, 
                  data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para fdis
fdis_step_pcnm <- ordistep(fdis0_pcnm, 
                           scope=formula(fdis_pcnm))

#Ver pcnm significativos
fdis_step_pcnm$anova 

# create pcnm table with only significant axes y se quita el 1
n_fdis <- paste('PCNM', c(8,10,35,9,6,15,3,13,7,4,17,5,24,40,54), sep='')
n_fdis
fdis_pcnm_sub <- parcelas_pcnm$vectors[,n_fdis]


# Forward selection de variables ambientales fdis -------------------

forward.sel(data_fdiver$fdis, data_environmet_parcelas,
            alpha = 0.01)

forward.sel(data_fdiver$fdis, data_environmet_clima,
            alpha = 0.01)

forward.sel(data_fdiver$fdis, data_environmet_topo,
            alpha = 0.01)


# Modelo fdis ---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: Mg CLAY

# 2) Clima: TEMP

#3) Topo: ELEV

# 4) space (fdis_pcnm_sub)

fdis_var <- varpart(data_fdiver$fdis,
                    data_environmet_parcelas[,c("Mg","CLAY")],
                    data_environmet_clima[,c("TEMP")],
                    data_environmet_topo[,"ELEV"],
                    fdis_pcnm_sub)
fdis_var

# Probando la significancia  ----------------------------------------------

# significance of partition X1: Mg CLAY
anova(rda(data_fdiver$fdis ~ data_environmet_parcelas$Mg +
            data_environmet_parcelas$CLAY +
            
            Condition(data_environmet_clima$TEMP) + 
            Condition(data_environmet_topo$ELEV) +
            Condition(fdis_pcnm_sub)))


# significance of partition X2: TEMP
anova(rda(data_fdiver$fdis ~ data_environmet_clima$TEMP  +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$Mg) + 
            Condition(data_environmet_topo$ELEV) +
            Condition(fdis_pcnm_sub)))

# significance of partition X3: ELEV
anova(rda(data_fdiver$fdis ~ data_environmet_topo$ELEV +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$Mg) + 
            Condition(data_environmet_clima$TEMP)+
            Condition(fdis_pcnm_sub)))

# significance of partition X4: space (fdis_pcnm_sub)
anova(rda(data_fdiver$fdis ~ fdis_pcnm_sub +
            
            Condition(data_environmet_parcelas$CLAY) + 
            Condition(data_environmet_parcelas$Mg) + 
            Condition(data_environmet_clima$TEMP)+
            Condition(data_environmet_topo$ELEV)))

rm(fdis0_pcnm,fdis_pcnm,fdis_pcnm_sub,fdis_var,
   fdis_step_pcnm, n_fdis)







