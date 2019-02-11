rm(list = ls())

# Variation Partitioning - incorporating spatial processes ----------------

# Objetivo ----------------------------------------------------------------
#En este ejemplo, se realizara el procedimiento de partition variation para
#2 categorias de data ambiental, buscando el poder explicativo de cada una
#como variables predictoras de cambios en la comunidad.

#Este ejemplo fue tomado de www.hiercourse.com/docs/Rnotes_multivariate.pdf


# Paquetes ----------------------------------------------------------------
library(vegan)



# Cargar datos ------------------------------------------------------------

# Data especies ----------------------------------------------------------


data(mite)
dim(mite)
head(mite)


# Data Variables ambientales ----------------------------------------------
data(mite.env)
dim(mite.env)
head(mite.env)



# Data Coordenadas de las parcelas ----------------------------------------
data(mite.xy)
dim(mite.xy)
head(mite.xy)
plot(mite.xy)



# Calcular PCNMs a partir de una matriz de distancia euclidea -------------


#Principal Coordinates of Neighbour Matrices (PCNMs) generate a dataframe 
#containing variables that represent different spatial scales

#This represent the spatial distributions of samples
#las coordenadas geográficas (descompuestas en un análisis previo de PCNM 
#con 21 descriptores espaciales - autovectores del PCNM - ).


mite.pcnm <- pcnm(dist(mite.xy))


# Seleccionar pcnm significativos -----------------------------------------

# Model con all predictors
miteall.pcnm <- rda(mite ~ ., data=as.data.frame(scores(mite.pcnm)))

# Model con no predictors
mite0.pcnm <- rda(mite ~ 1, data=as.data.frame(scores(mite.pcnm)))

#Seleccionar variables significativas

step.pcnm <- ordistep(mite0.pcnm, scope=formula(miteall.pcnm))

#Ver pcnm significativos
step.pcnm$anova 

# create pcnm table with only significant axes
endo.pcnm.sub <- scores(mite.pcnm,
                        choices=c(2,3, 6, 10,12,27,39))


# Model -------------------------------------------------------------------

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) substrate ('Substrate', 'SubsDens', and 'WatrCont')

# 2) landscape, i.e., shrub density adn microtopography ('Shrub' and 'Topo')

# 3) space ('mite.pcnm')


mite.var <- varpart(mite,
                    ~ Substrate + SubsDens + WatrCont,
                    ~ Shrub + Topo,
                      mite.pcnm, 
                    data=mite.env)



# Ejemplo de interpretacion -----------------------------------------------


#The individual fraction associated with ’landscape’ is missing because 
#this number is negative. These numbers represent R2 values after adjusting 
#for the number of explanatory variables in each partition (’adjusted R2’) 
#and will be negative when the raw R2 is very small 
#plot(mite.var, bg=1:3, Xnames=c('substrate', 'landscape', 'space'), id.size=0.75)


