
# Objetivo ----------------------------------------------------------------
#Crear una funcion que calcule CWM
#La funcion fue tomada del libro Functional and Phylogenetic Ecology in R
#pp64


# Argumentos --------------------------------------------------------------
#x= abundancia relativa de las parcelas 
#traits= nombre de el data set que contienen los rasgos funcionales 


# Function ----------------------------------------------------------------
cwm.func <- function(x){
  weighted.mean(traits[names(x[x>0]),1],x[x>0])
}

#apply
apply(x,MARGIN = ,cwm.func)

