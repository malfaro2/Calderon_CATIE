rm(list = ls())

# Cargar paquetes ---------------------------------------------------------

library(tidyverse)
library(corrplot)

# Objetivo ----------------------------------------------------------------
#Mostrar el grado de correlacion de todas las variables de interes con los
#datos sin palmas

#El codigo para realizar este grafico fue tomado de
#http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

# Load data ---------------------------------------------------------------
data_cwm_sp <- read.csv("data/resultados_csv/data_cwm_sinpalmas_coord.csv",
                     header = T)

data_redundancy_sp <- read.csv("data/resultados_csv/data_redundancy_sinpalmas.csv",
                            header = T)

data_fdiver_sp <- read.csv("data/resultados_csv/data_fdiversity_sinpalmas_coord.csv",
                        header = T)

data_env <- read.csv("data/raw/data_enviroment_worldclim.csv",header=T)

#Change names
data_env <- data_env %>% 
  rename(plot = PLOT,forest_type = FOREST_TYPE ) %>% 
  select(-c(CRTM_90_X,CRTM_90_Y))

# Juntar data sets en unos solo -------------------------------------------
data_full_sp <- left_join(data_cwm_sp,data_redundancy_sp,data_fdiver_sp   ,
                       by = c("plot","forest_type", "longitude","latitude"))

data_full_sp <- left_join(data_full_sp, data_env,by=c("plot","forest_type")) %>% 
  select(-c(plot,forest_type))

# Plot --------------------------------------------------------------------

#Matriz de correlacion
M <- cor(data_full_sp)

#Funcion
# matrix of the p-value of the correlation
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#Matriz de p-values
p.mat <- cor.mtest(data_full_sp)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         # Add coefficient of correlation
         addCoef.col = "black", 
         #Text label color and rotation
         tl.col="black", tl.srt=90, 
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
