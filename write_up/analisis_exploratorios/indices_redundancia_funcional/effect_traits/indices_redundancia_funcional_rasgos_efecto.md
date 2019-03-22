---
title: "Indices diversidad funcional rasgos efecto"
author: "Erick Calderón-Morales"
date: "March 19, 2019"
output:   
 html_document:
   code_folding: hide
   fig_height: 8
   fig_width: 10
   keep_md: yes
   toc: yes
   toc_depth: 2
   toc_float: yes
---




```
## [1] 127 257
```

```
## [1] 127 257
```

```
## [1] 127   4
```

```
## [1] 257   5
```


#Calculo de los indices de redundancia funcional 


```r
#Ordenar los nombres 
#target <- colnames(dabund_relativa) 
#deff <- deff[match(target, row.names(dresp)),]
```

__Indices redundancia funcional ponderados por la abundancia relativa(w.abund=T) de las especies presentes (total de individuos presentes 4801). Los rasgos estan estandarizados con media 0 y misma varianza.__

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Rasgos de efecto</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Rasgo </th>
   <th style="text-align:left;"> Significado </th>
   <th style="text-align:left;"> Unidades_de_medida </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> AFE </td>
   <td style="text-align:left;"> Area foliar especifica </td>
   <td style="text-align:left;"> mm2/Mg </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CFMS </td>
   <td style="text-align:left;"> Contenido foliar de materia seca </td>
   <td style="text-align:left;"> Mg/g </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DM </td>
   <td style="text-align:left;"> Densidad de madera </td>
   <td style="text-align:left;"> g/cm3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> N </td>
   <td style="text-align:left;"> Concentracion foliar de nitrogeno </td>
   <td style="text-align:left;"> Mg/g </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P </td>
   <td style="text-align:left;"> Concentracion foliar de fosforo </td>
   <td style="text-align:left;"> Mg/g </td>
  </tr>
</tbody>
</table>

#Objetivo 
El objetivo de este script es calcular el uniqueness y redundancy para los 
datos de response traits de las 127 parcelas

LOS RASGOS UTILIZADOS PARA EL ANALISIS ES AFE,DM,CFMS,N,P


Argumentos de la funcion 

comm: A matrix or a data frame of N plots × S species containing the 
abundance or incidence (0/1) of all species in the in plots. 
Columns are species and plots are rows

dis: An object of class 'dist' containing the functional distances 
among species

tol: A tolerance threshold (a value less than tol is considered as null)

abundance: A logical. If TRUE abundance data are used when available; 
if FALSE incidence (0/1) data are used.




```r
# comm --------------------------------------------------------------------
#Data parcelas por especies
#En las Columnas deben ir las especies y las parcelas en las filas 

comm<- dabund_relativa

# dis ---------------------------------------------------------------------
#Data rasgos: se deben convertir objeto dis
#dis: An object of class 'dist' containing the functional distances among 
#species

#Transformar traits a distancias
dist <- vegdist(deff_redun, method = "euclidean") 

#Transformar distancias a un rango de entre 0 y 1
dist_rescaled <- dist / max(dist)

# Funcion -----------------------------------------------------------------
unique <- uniqueness(comm, dist_rescaled, abundance=TRUE)
```




```r
#Obtener indices Redundancy, Uniqueness, Rao
indices_redun <-   data.frame(unique$red) %>% 
  rownames_to_column("plot") %>% 
  mutate(redundancy= 1-U)
  
#Guardar  archivos .csv
write.csv(indices_redun, "C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_indices_eff_redundancy.csv")
```


##Medidas de resumen para Redundancy, Uniqueness, Rao

###Medidas de resumen por indice

```r
indices_redun %>% gather("D", "Q", "U","redundancy" , key = "rasgo", value = "valor_del_rasgo") %>% 
  group_by(rasgo) %>% 
  summarize(mean=mean(valor_del_rasgo),sd=sd(valor_del_rasgo),max=max(valor_del_rasgo),min=min(valor_del_rasgo)) %>%
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),full_width = F)
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> rasgo </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> min </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.8360976 </td>
   <td style="text-align:right;"> 0.0838429 </td>
   <td style="text-align:right;"> 0.9464923 </td>
   <td style="text-align:right;"> 0.4791667 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q </td>
   <td style="text-align:right;"> 0.1162531 </td>
   <td style="text-align:right;"> 0.0301485 </td>
   <td style="text-align:right;"> 0.2077294 </td>
   <td style="text-align:right;"> 0.0538777 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> redundancy </td>
   <td style="text-align:right;"> 0.8613343 </td>
   <td style="text-align:right;"> 0.0316136 </td>
   <td style="text-align:right;"> 0.9240270 </td>
   <td style="text-align:right;"> 0.7710738 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 0.1386657 </td>
   <td style="text-align:right;"> 0.0316136 </td>
   <td style="text-align:right;"> 0.2289262 </td>
   <td style="text-align:right;"> 0.0759730 </td>
  </tr>
</tbody>
</table>



###Medidas de resumen para cada indice por tipo de bosque

```r
dfull_redundancy <- left_join(indices_redun,dparcelas, by="plot") 
```

```
## Warning: Column `plot` joining character vector and factor, coercing into
## character vector
```

```r
dfull_redundancy %>% gather("D", "Q", "U","redundancy" , key = "rasgo", value = "valor_del_rasgo") %>% 
  group_by(forest_type,rasgo) %>% 
  summarize(mean=mean(valor_del_rasgo),sd=sd(valor_del_rasgo),max=max(valor_del_rasgo),min=min(valor_del_rasgo)) %>%
  arrange(rasgo) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),full_width = F)
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> forest_type </th>
   <th style="text-align:left;"> rasgo </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> min </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.8880520 </td>
   <td style="text-align:right;"> 0.0596885 </td>
   <td style="text-align:right;"> 0.9464923 </td>
   <td style="text-align:right;"> 0.6758495 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.8036586 </td>
   <td style="text-align:right;"> 0.0911169 </td>
   <td style="text-align:right;"> 0.9243918 </td>
   <td style="text-align:right;"> 0.4791667 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.8555055 </td>
   <td style="text-align:right;"> 0.0484759 </td>
   <td style="text-align:right;"> 0.9393579 </td>
   <td style="text-align:right;"> 0.7312501 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> Q </td>
   <td style="text-align:right;"> 0.1474302 </td>
   <td style="text-align:right;"> 0.0274382 </td>
   <td style="text-align:right;"> 0.2077294 </td>
   <td style="text-align:right;"> 0.0815767 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> Q </td>
   <td style="text-align:right;"> 0.1006995 </td>
   <td style="text-align:right;"> 0.0217392 </td>
   <td style="text-align:right;"> 0.1597311 </td>
   <td style="text-align:right;"> 0.0538777 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> Q </td>
   <td style="text-align:right;"> 0.1188600 </td>
   <td style="text-align:right;"> 0.0225287 </td>
   <td style="text-align:right;"> 0.1645926 </td>
   <td style="text-align:right;"> 0.0694097 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> redundancy </td>
   <td style="text-align:right;"> 0.8335888 </td>
   <td style="text-align:right;"> 0.0314064 </td>
   <td style="text-align:right;"> 0.9077905 </td>
   <td style="text-align:right;"> 0.7710738 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> redundancy </td>
   <td style="text-align:right;"> 0.8743733 </td>
   <td style="text-align:right;"> 0.0250860 </td>
   <td style="text-align:right;"> 0.9240270 </td>
   <td style="text-align:right;"> 0.7910974 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> redundancy </td>
   <td style="text-align:right;"> 0.8608688 </td>
   <td style="text-align:right;"> 0.0264175 </td>
   <td style="text-align:right;"> 0.9110830 </td>
   <td style="text-align:right;"> 0.8068816 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 0.1664112 </td>
   <td style="text-align:right;"> 0.0314064 </td>
   <td style="text-align:right;"> 0.2289262 </td>
   <td style="text-align:right;"> 0.0922095 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 0.1256267 </td>
   <td style="text-align:right;"> 0.0250860 </td>
   <td style="text-align:right;"> 0.2089026 </td>
   <td style="text-align:right;"> 0.0759730 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 0.1391312 </td>
   <td style="text-align:right;"> 0.0264175 </td>
   <td style="text-align:right;"> 0.1931184 </td>
   <td style="text-align:right;"> 0.0889170 </td>
  </tr>
</tbody>
</table>


###Mapa de Costa Rica 


```r
world <- getMap(resolution = "low")

clipper_costarica <- as(extent(-86, -82.5, 8, 11.2), "SpatialPolygons")
proj4string(clipper_costarica) <- CRS(proj4string(world))
costarica_clip <- raster::intersect(world, clipper_costarica)
costarica_clip<- fortify(costarica_clip)

ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull_redundancy,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=forest_type))+
    
    #geom_point(dfull, mapping=aes(x=longitude,y=latitude),
    #           colour=dfull$feve, alpha=0.5)+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    #scale_color_gradient(low="yellow", high="red")+
   
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))
```

![](indices_redundancia_funcional_rasgos_efecto_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



```r
world <- getMap(resolution = "low")

clipper_costarica <- as(extent(-84.5, -83.75, 10, 10.80), "SpatialPolygons")
proj4string(clipper_costarica) <- CRS(proj4string(world))
costarica_clip <- raster::intersect(world, clipper_costarica)
costarica_clip<- fortify(costarica_clip)

Q <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull_redundancy,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull_redundancy$Q,shape=forest_type))+

    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "Rao Q")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))


U <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull_redundancy,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull_redundancy$U,shape=forest_type))+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "Uniqueness")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))

redundancy <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull_redundancy,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull_redundancy$redundancy,shape=forest_type))+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "redundancy")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))

#https://cran.r-project.org/web/packages/cowplot/vignettes/plot_grid.html
theme_set(theme_cowplot(font_size=30)) 
plot_grid(Q, U,redundancy,hjust = 2, vjust = 3)
```

![](indices_redundancia_funcional_rasgos_efecto_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



