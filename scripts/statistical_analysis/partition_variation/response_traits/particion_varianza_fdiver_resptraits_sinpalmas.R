rm(list = ls())
set.seed(123)

# Variation Partitioning - incorporating spatial processes ----------------

# Objetivo ----------------------------------------------------------------
# Se realizará el procedimiento de partition variation para los indices de 
#fdis feve fdiv sin palmas

#Este ejemplo fue tomado de www.hiercourse.com/docs/Rnotes_multivariate.pdf

# Paquetes ----------------------------------------------------------------
library(vegan)
library(adespatial)

# Cargar datos ------------------------------------------------------------

# Data Fiv-Feve-fdis ------------------------------------------------------

data_fdiver_sp <- 
  read.csv("data/resultados_csv/data_fdiversity_resptraits_sinpalmas.csv")
head(data_fdiver_sp)

#Eliminar columnas
data_fdiver_sp <- data_fdiver_sp[,-c(1)]
head(data_fdiver_sp)


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
data_fdiver_sp <- data.frame(scale(data_fdiver_sp))
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

# Varpart Fdiv sin palmas -------------------------------------------------
# Seleccionar pcnms significativos  ---------------------------------------
# Model con all predictors para fdiv

fdiv_pcnm_sp <- rda(data_fdiver_sp$fdiv ~ ., 
                 data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors para fdiv

fdiv0_pcnm_sp <- rda(data_fdiver_sp$fdiv ~ 1, 
                  data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para fdiv
fdiv_step_pcnm_sp <- ordistep(fdiv0_pcnm_sp, 
                           scope=formula(fdiv_pcnm_sp),
                           permutations = how(nperm = 2000))

#Ver pcnm significativos
fdiv_step_pcnm_sp$anova 

# create pcnm table with only significant axes y se quita el 1
n_fdiv_sp <- paste('PCNM', c(15,29,5,19,54,4,25), sep='')
n_fdiv_sp
fdiv_pcnm_sub_sp <- parcelas_pcnm$vectors[,n_fdiv_sp]


# Forward selection de variables ambientales redundancy -------------------

forward.sel(data_fdiver_sp$fdiv, data_environmet_parcelas,
            alpha = 0.01,nperm = 2000)

forward.sel(data_fdiver_sp$fdiv, data_environmet_clima,
            alpha = 0.01,nperm = 2000)

forward.sel(data_fdiver_sp$fdiv, data_environmet_topo,
            alpha = 0.01,nperm = 2000)
# Data para qeco ----------------------------------------------------------

data_espacio_qeco <- fdiv_pcnm_sub_sp
SAND <- data_environmet_parcelas[,c("SAND")]
TEMPMIN <- data_environmet_clima[,c("TEMPMIN")]
elevation <- data_environmet_topo[,"ELEV"]
fdiv_sp <- data_fdiver_sp$fdiv

data_fdiv_sp_qeco <- cbind(data_espacio_qeco,
                        SAND, 
                        TEMPMIN,
                        elevation,
                        fdiv_sp)


write.csv(data_fdiv_sp_qeco,"data/resultados_csv/varpart/response_traits/data_fdiv_sp_qeco.csv")

# Modelo fdiv ---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: Mg 

# 2) Clima: TEMPSD

#3) Topo: Ninguna

# 4) space (fdiv_pcnm_sub_sp)

fdiv_var_sp <- varpart(data_fdiver_sp$fdiv,
                    data_environmet_parcelas[,c("Mg")],
                    data_environmet_clima[,c("TEMPSD")],
                    fdiv_pcnm_sub_sp)
fdiv_var_sp

# Probando la significancia  ----------------------------------------------

# significance of partition X1: Mg
anova(rda(data_fdiver_sp$fdiv ~ data_environmet_parcelas$Mg +
            
            Condition(data_environmet_clima$TEMPSD) + 
            Condition(fdiv_pcnm_sub_sp)))


# significance of partition X2: TEMPSD 
anova(rda(data_fdiver_sp$fdiv ~ data_environmet_clima$TEMPSD +
            
            Condition(data_environmet_parcelas$Mg) + 
            Condition(fdiv_pcnm_sub_sp)))


# significance of partition X3: space (fdiv_pcnm_sub)
anova(rda(data_fdiver_sp$fdiv ~fdiv_pcnm_sub_sp +
            
            Condition(data_environmet_parcelas$Mg) + 
            Condition(data_environmet_clima$TEMPSD)))


rm(fdiv0_pcnm_sp,fdiv_pcnm_sp,fdiv_pcnm_sub_sp,fdiv_var_sp,
   fdiv_step_pcnm_sp, n_fdiv_sp)


# Varpart Feve sin palmas -------------------------------------------------
# Seleccionar pcnms significativos  ---------------------------------------
# Model con all predictors para feve

feve_pcnm_sp <- rda(data_fdiver_sp$feve ~ ., 
                 data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors para feve

feve0_pcnm_sp <- rda(data_fdiver_sp$feve ~ 1, 
                  data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para feve
feve_step_pcnm_sp <- ordistep(feve0_pcnm_sp, 
                           scope=formula(feve_pcnm_sp),
                           permutations = how(nperm = 2000))

#Ver pcnm significativos
feve_step_pcnm_sp$anova 

# create pcnm table with only significant axes y se quita el 1
n_feve_sp <- paste('PCNM', c(47,9,6,24,25), sep='')
n_feve_sp
feve_pcnm_sub_sp <- parcelas_pcnm$vectors[,n_feve_sp]


# Forward selection de variables ambientales feve -------------------

forward.sel(data_fdiver_sp$feve, data_environmet_parcelas,
            alpha = 0.01,nperm = 2000)

forward.sel(data_fdiver_sp$feve, data_environmet_clima,
            alpha = 0.01,nperm = 2000)

forward.sel(data_fdiver_sp$feve, data_environmet_topo,
            alpha = 0.01,nperm = 2000)

# Data para qeco ----------------------------------------------------------

data_espacio_qeco <- feve_pcnm_sub_sp 
P <- data_environmet_parcelas[,c("P")]
feve_sp <- data_fdiver_sp$feve

data_feve_sp_qeco <- cbind(data_espacio_qeco,
                        P,
                        feve_sp)

write.csv(data_feve_sp_qeco,"data/resultados_csv/varpart/response_traits/data_feve_sp_qeco.csv")


# Modelo feve ---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: SAND

# 2) Clima: TEMPSD , PRECCV

#3) Topo: ELEV

# 4) space (feve_pcnm_sub_sp)

feve_var_sp <- varpart(data_fdiver_sp$feve,
                    data_environmet_parcelas[,c("SAND")],
                    data_environmet_clima[,c("TEMPSD","PRECCV")],
                    data_environmet_topo[,"ELEV"],
                    feve_pcnm_sub_sp)
feve_var_sp

# Probando la significancia  ----------------------------------------------

# significance of partition X1: SAND
anova(rda(data_fdiver_sp$feve ~ data_environmet_parcelas$SAND +
            
            Condition(data_environmet_clima$TEMPSD) + 
            Condition(data_environmet_clima$PRECCV) + 
            Condition(data_environmet_topo$ELEV) +
            Condition(feve_pcnm_sub_sp)))


# significance of partition X2: TEMPSD , PRECCV 
anova(rda(data_fdiver_sp$feve ~ data_environmet_clima$PRECCV  +
            data_environmet_clima$TEMPSD  +
            
            Condition(data_environmet_parcelas$SAND) + 
            Condition(data_environmet_topo$ELEV) +
            Condition(feve_pcnm_sub_sp)))

# significance of partition X3: ELEV
anova(rda(data_fdiver_sp$feve ~ data_environmet_topo$ELEV +
            
            Condition(data_environmet_parcelas$SAND) +
            Condition(data_environmet_clima$TEMPSD) + 
            Condition(data_environmet_clima$PRECCV) + 
            Condition(feve_pcnm_sub_sp)))

# significance of partition X4: space (feve_pcnm_sub)
anova(rda(data_fdiver_sp$feve ~ feve_pcnm_sub_sp +
            
            Condition(data_environmet_parcelas$SAND) +
            Condition(data_environmet_clima$TEMPSD) + 
            Condition(data_environmet_clima$PRECCV) + 
            Condition(data_environmet_topo$ELEV)))


rm(feve0_pcnm_sp,feve_pcnm_sp,feve_pcnm_sub_sp,feve_var_sp,
   feve_step_pcnm_sp, n_feve_sp)


# Varpart fdis sin palmas -------------------------------------------------
# Seleccionar pcnms significativos  ---------------------------------------
# Model con all predictors para fdis

fdis_pcnm_sp <- rda(data_fdiver_sp$fdis ~ ., 
                 data=as.data.frame(scores(parcelas_pcnm)))

# Model con no predictors para fdis

fdis0_pcnm_sp <- rda(data_fdiver_sp$fdis ~ 1, 
                  data=as.data.frame(scores(parcelas_pcnm)))

#Seleccionar variables significativas para fdis
fdis_step_pcnm_sp <- ordistep(fdis0_pcnm_sp, 
                           scope=formula(fdis_pcnm_sp),
                           permutations = how(nperm = 2000))

#Ver pcnm significativos
fdis_step_pcnm_sp$anova 

# create pcnm table with only significant axes y se quita el 1
n_fdis_sp <- paste('PCNM', c(10,38), sep='')
n_fdis_sp
fdis_pcnm_sub_sp <- parcelas_pcnm$vectors[,n_fdis_sp]

# Forward selection de variables ambientales fdis -------------------

forward.sel(data_fdiver_sp$fdis, data_environmet_parcelas,
            alpha = 0.01,nperm = 2000)

forward.sel(data_fdiver_sp$fdis, data_environmet_clima,
            alpha = 0.01,nperm = 2000)

forward.sel(data_fdiver_sp$fdis, data_environmet_topo,
            alpha = 0.01,nperm = 2000)
# Data para qeco ----------------------------------------------------------

data_espacio_qeco <- fdis_pcnm_sub_sp 
fdis_sp <- data_fdiver_sp$fdis

data_fdis_sp_qeco <- cbind(data_espacio_qeco,
                        fdis_sp)

#View(data_fdis_sp_qeco)

write.csv(data_fdis_sp_qeco,"data/resultados_csv/varpart/response_traits/data_fdis_sp_qeco.csv")

# Modelo fdis ---------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) Caracteristicas fisicas de la parcela: SAND

# 2) Clima: TEMPSD

#3) Topo: ELEV

# 4) space (fdis_pcnm_sub_sp)

fdis_var_sp <- varpart(data_fdiver_sp$fdis,
                    data_environmet_parcelas[,c("SAND")],
                    data_environmet_clima[,c("TEMPSD")],
                    data_environmet_topo[,"ELEV"],
                    fdis_pcnm_sub_sp)
fdis_var_sp

# Probando la significancia  ----------------------------------------------

# significance of partition X1: SAND
anova(rda(data_fdiver_sp$fdis ~ data_environmet_parcelas$SAND +
            
            Condition(data_environmet_clima$TEMPSD) + 
            Condition(data_environmet_topo$ELEV) +
            Condition(fdis_pcnm_sub_sp)))


# significance of partition X2: TEMPSD
anova(rda(data_fdiver_sp$fdis ~ data_environmet_clima$TEMPSD  +
            
            Condition(data_environmet_parcelas$SAND) + 
            Condition(data_environmet_topo$ELEV) +
            Condition(fdis_pcnm_sub_sp)))

# significance of partition X3: ELEV
anova(rda(data_fdiver_sp$fdis ~ data_environmet_topo$ELEV +
            
            Condition(data_environmet_parcelas$SAND) + 
            Condition(data_environmet_clima$TEMPSD) +
            Condition(fdis_pcnm_sub_sp)))

# significance of partition X4: space (fdis_pcnm_sub)
anova(rda(data_fdiver_sp$fdis ~ fdis_pcnm_sub_sp +
            
            Condition(data_environmet_parcelas$SAND) + 
            Condition(data_environmet_clima$TEMPSD) +
            Condition(data_environmet_topo$ELEV)))

rm(fdis0_pcnm_sp,fdis_pcnm_sp,fdis_pcnm_sub_sp,fdis_var_sp,
   fdis_step_pcnm_sp, n_fdis_sp)























