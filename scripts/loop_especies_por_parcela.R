# Loop --------------------------------------------------------------------

#Este loop funciona para conocer la composion de especies en datos con las
#parcelas en las filas y las especies en las columnas

# Derivacion del loop -----------------------------------------------------
#Todos los argumentos hacen lo mismo excepto el ultimo
#names(my.sample[1,my.sample[1,]>0])
#names(my.sample[1,my.sample[1,]>0])
#names(lista$com1[,lista$com1>0])
#names(lista[[1]][,lista$com1>0])
#names(lista[[1]][,lista[[1]]>0])
#row.names(lista[[1]][,lista[[1]]>0])

# loop especies por parcela -----------------------------------------------

# 1-Separar el ser de datos en una lista por parcela
list<-split("data", row.names("data"))

# 2-Crear una lista para almacenar la composicion de espeices de cada 
# parcela
sp_comp<-list()

#Loop 
for (i in seq_along(names(list))) {
  
  #especies de cada parcela con una abundancia mayor a 0 
  comp<-names(list[[i]][,list[[i]]>0])
  
  #Nombres de cada parcela
  plot<- row.names(list[[i]][,list[[i]]>0])
  
  #datos con parcela y composicion de especies 
  data<-data.frame(comp,plot)
  
  #Resultado
  sp_comp[[i]] <- data
  print(sp_comp)
}




