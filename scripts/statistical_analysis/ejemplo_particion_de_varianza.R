rm(list = ls())


# Objetivo ----------------------------------------------------------------
#En este ejemplo, se realizara el procedimiento de partition variation para
#2 categorias de data ambiental, buscando el poder explicativo de cada una
#como variables predictoras de cambios en la comunidad.

#Este ejemplo fue tomado de www.hiercourse.com/docs/Rnotes_multivariate.pdf


# Paquetes ----------------------------------------------------------------
library(vegan)


# Data --------------------------------------------------------------------
data(varechem)
data(varespec)



# Variation partitioning sin componente espacial --------------------------
#Separar varechem
#1- Soil chemistry
#2- Soil exposure (Baresoil y Humdepth)

#standardize: scale x to zero mean and unit variance (default MARGIN = 2)
varechem.scld <- decostand(varechem, method='standardize')

vare.var <- varpart(varespec,
                    ~N + Al + Fe + Mn,
                    ~Baresoil + Humdepth,
                    data=varechem.scld, scale=T)

vare.var

plot(vare.var, bg=1:3, Xnames=c('chemistry', 'exposure'), id.size=0.75)


# test significance of variation in partition 'X1' (chemistry)
anova(rda(varespec ~ N + Al + Fe + Mn, data=varechem.scld, scale=T))

# test significance of variation in partition 'X2' (exposure)
anova(rda(varespec ~ Baresoil + Humdepth, data=varechem.scld, scale=T))

# test significance of variation in both partitions
anova(rda(varespec ~ N + Al + Fe + Mn + Baresoil + Humdepth,
          data=varechem.scld, scale=T))

# test significance of variation in partition 'X1' (chemistry) after
# accounting for variation in 'X2'
anova(rda(varespec ~ N + Al + Fe + Mn
          + Condition(Baresoil + Humdepth), data=varechem.scld, scale=T))

# test significance of variation in partition 'X2' (exposure) after
# accounting for variation in 'X1'
anova(rda(varespec ~ Baresoil + Humdepth
          + Condition(N + Al + Fe + Mn), data=varechem.scld, scale=T))


# Variation Partitioning - incorporating spatial processes ----------------

#Data especies
data(mite)
dim(mite)
head(mite)

#Variables ambientales
data(mite.env)
dim(mite.env)
head(mite.env)


#Coordenadas de las parcelas
data(mite.xy)
dim(mite.xy)
head(mite.xy)
plot(mite.xy)

#Calcular PCNMs a partir de una matriz de distancia euclidea 

#Principal Coordinates of Neighbour Matrices (PCNMs) generate a dataframe 
#containing variables that represent different spatial scales

#This represent the spatial distributions of samples
mite.pcnm <- as.data.frame(scores(pcnm(dist(mite.xy))))
dim(mite.pcnm)

# Ordistep ----------------------------------------------------------------

vare.rda <- rda(mite ~ ., data=mite.pcnm)
# set up the null case with no predictors (be sure to include the
# 'data' argument, even though no predictors)
vare.pca <- rda(mite ~ 1, data=mite.pcnm)

step.env <- ordistep(vare.pca, scope=formula(vare.rda))

step.env
anova(step.env)
step.env$anova


# set up a multipanel graphics window
par(mfrow=c(2, 3))
# set colour palette with ten levels along a gradient from red to blue
# from Chapter 4
blueredfun <- colorRampPalette(c("blue","red"))
palette(blueredfun(10))

# for each of the first six PCNM axes, use colour to represent loadings
plot(mite.xy, pch=16, col=cut(mite.pcnm[[1]], breaks=10), cex.lab=1.5, cex.axis=1.3, cex=2)
plot(mite.xy, pch=16, col=cut(mite.pcnm[[2]], breaks=10), cex.lab=1.5, cex.axis=1.3, cex=2)
plot(mite.xy, pch=16, col=cut(mite.pcnm[[3]], breaks=10), cex.lab=1.5, cex.axis=1.3, cex=2)
plot(mite.xy, pch=16, col=cut(mite.pcnm[[4]], breaks=10), cex.lab=1.5, cex.axis=1.3, cex=2)
plot(mite.xy, pch=16, col=cut(mite.pcnm[[5]], breaks=10), cex.lab=1.5, cex.axis=1.3, cex=2)
plot(mite.xy, pch=16, col=cut(mite.pcnm[[43]], breaks=10), cex.lab=1.5, cex.axis=1.3, cex=2)

# do predictor matrices explain community composition, and how much?

# partition variation among three predictor tables:

# 1) substrate ('Substrate', 'SubsDens', and 'WatrCont')

# 2) landscape, i.e., shrub density adn microtopography ('Shrub' and 'Topo')

# 3) space ('mite.pcnm')


mite.var <- varpart(mite,
                    ~ Substrate + SubsDens + WatrCont,
                    ~ Shrub + Topo,
                    mite.pcnm, data=mite.env)


#   -----------------------------------------------------------------------

#The individual fraction associated with ’landscape’ is missing because 
#this number is negative. These numbers represent R2 values after adjusting 
#for the number of explanatory variables in each partition (’adjusted R2’) 
#and will be negative when the raw R2 is very small 
#plot(mite.var, bg=1:3, Xnames=c('substrate', 'landscape', 'space'), id.size=0.75)


