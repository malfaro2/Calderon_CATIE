rm(list = ls())

# Objetivo ----------------------------------------------------------------
#Calcular CWM para los rasgos funcionales de las 127 parcelas siguiendo el 
#capitulo 4 del libro Functional and Phylogenetic Ecology in R y el tuorial
# Intro to functional programing



# Analisis ----------------------------------------------------------------


# Rasgos funcioanles  -----------------------------------------------------

# CWM del las 127 parcelas weight por abund relativa ----------------------
# Funcion CWM -------------------------------------------------------------

#af
cwm.func1<- function(x){
  weighted.mean(deff_clean[names(x[x>0]),1],x[x>0],na.rm = T)
}

#afe
cwm.func2<- function(x){
  weighted.mean(deff_clean[names(x[x>0]),2],x[x>0],na.rm = T)
}

#cfms
cwm.func3<- function(x){
  weighted.mean(deff_clean[names(x[x>0]),3],x[x>0],na.rm = T)
}


apply(dabund_relativa, 1, cwm.func) 
as.data.frame(apply(dabund, 1, cwm.func)) 


#Se elige la CWM del trait 2, parcela 1
weighted.mean(deff_clean[colnames(dabund_relativa),127],dabund_relativa[1,])#wRONG
weighted.mean(deff_clean[colnames(dabund_relativa),1],dabund_relativa[127,])













