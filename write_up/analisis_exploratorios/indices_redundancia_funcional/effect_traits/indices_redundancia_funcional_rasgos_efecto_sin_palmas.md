---
title: "Indices diversidad funcional rasgos efecto sin palmas "
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
## [1] 127 253
```

```
## [1] 127   4
```

```
## [1] 253   5
```


#Calculo de los indices de redundancia funcional sin palmas


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

comm_sp <- dabund_relativa_sp

# dis ---------------------------------------------------------------------
#Data rasgos: se deben convertir objeto dis
#dis: An object of class 'dist' containing the functional distances among 
#species

#Transformar traits a distancias
dist_sp <- vegdist(deff_redun_sp, method = "euclidean") 

#Transformar distancias a un rango de entre 0 y 1
dist_rescaled_sp <- dist_sp / max(dist_sp)

# Funcion -----------------------------------------------------------------
unique_sp <- uniqueness(comm_sp, dist_rescaled_sp, abundance=TRUE)
```




```r
#Obtener indices Redundancy, Uniqueness, Rao
indices_redun_sp <-   data.frame(unique_sp$red) %>% 
  rownames_to_column("plot") %>% 
  mutate(redundancy_sp = 1-U)
  
#Guardar  archivos .csv
#write.csv(indices_redun_sp, "C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_indices_eff_redundancy_sp.csv")
```


##Medidas de resumen para Redundancy, Uniqueness, Rao

###Medidas de resumen por indice

```r
indices_redun_sp %>% gather("D", "Q", "U","redundancy_sp" , key = "rasgo", value = "valor_del_rasgo") %>% 
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
   <td style="text-align:right;"> 0.8057410 </td>
   <td style="text-align:right;"> 0.1258156 </td>
   <td style="text-align:right;"> 0.9492455 </td>
   <td style="text-align:right;"> 0.4260205 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q </td>
   <td style="text-align:right;"> 0.1291388 </td>
   <td style="text-align:right;"> 0.0402151 </td>
   <td style="text-align:right;"> 0.2365941 </td>
   <td style="text-align:right;"> 0.0439941 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> redundancy_sp </td>
   <td style="text-align:right;"> 0.8411476 </td>
   <td style="text-align:right;"> 0.0390354 </td>
   <td style="text-align:right;"> 0.9249970 </td>
   <td style="text-align:right;"> 0.7410012 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 0.1588524 </td>
   <td style="text-align:right;"> 0.0390354 </td>
   <td style="text-align:right;"> 0.2589988 </td>
   <td style="text-align:right;"> 0.0750030 </td>
  </tr>
</tbody>
</table>



###Medidas de resumen para cada indice por tipo de bosque

```r
dfull_redundancy_sp <- left_join(indices_redun_sp,dparcelas, by="plot") 
```

```
## Warning: Column `plot` joining character vector and factor, coercing into
## character vector
```

```r
dfull_redundancy_sp %>% gather("D", "Q", "U","redundancy_sp" , key = "rasgo", value = "valor_del_rasgo") %>% 
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
   <td style="text-align:right;"> 0.9057761 </td>
   <td style="text-align:right;"> 0.0297733 </td>
   <td style="text-align:right;"> 0.9467456 </td>
   <td style="text-align:right;"> 0.8208617 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.7243998 </td>
   <td style="text-align:right;"> 0.1206447 </td>
   <td style="text-align:right;"> 0.9050000 </td>
   <td style="text-align:right;"> 0.4260205 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 0.8867330 </td>
   <td style="text-align:right;"> 0.0476392 </td>
   <td style="text-align:right;"> 0.9492455 </td>
   <td style="text-align:right;"> 0.7265625 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> Q </td>
   <td style="text-align:right;"> 0.1560554 </td>
   <td style="text-align:right;"> 0.0280781 </td>
   <td style="text-align:right;"> 0.2077294 </td>
   <td style="text-align:right;"> 0.0781407 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> Q </td>
   <td style="text-align:right;"> 0.1085830 </td>
   <td style="text-align:right;"> 0.0340801 </td>
   <td style="text-align:right;"> 0.1771712 </td>
   <td style="text-align:right;"> 0.0439941 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> Q </td>
   <td style="text-align:right;"> 0.1478567 </td>
   <td style="text-align:right;"> 0.0389764 </td>
   <td style="text-align:right;"> 0.2365941 </td>
   <td style="text-align:right;"> 0.0681855 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> redundancy_sp </td>
   <td style="text-align:right;"> 0.8277047 </td>
   <td style="text-align:right;"> 0.0310748 </td>
   <td style="text-align:right;"> 0.9106964 </td>
   <td style="text-align:right;"> 0.7710738 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> redundancy_sp </td>
   <td style="text-align:right;"> 0.8505148 </td>
   <td style="text-align:right;"> 0.0395992 </td>
   <td style="text-align:right;"> 0.9249970 </td>
   <td style="text-align:right;"> 0.7669946 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> redundancy_sp </td>
   <td style="text-align:right;"> 0.8338758 </td>
   <td style="text-align:right;"> 0.0407591 </td>
   <td style="text-align:right;"> 0.9096823 </td>
   <td style="text-align:right;"> 0.7410012 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 0.1722953 </td>
   <td style="text-align:right;"> 0.0310748 </td>
   <td style="text-align:right;"> 0.2289262 </td>
   <td style="text-align:right;"> 0.0893036 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 0.1494852 </td>
   <td style="text-align:right;"> 0.0395992 </td>
   <td style="text-align:right;"> 0.2330054 </td>
   <td style="text-align:right;"> 0.0750030 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:right;"> 0.1661242 </td>
   <td style="text-align:right;"> 0.0407591 </td>
   <td style="text-align:right;"> 0.2589988 </td>
   <td style="text-align:right;"> 0.0903177 </td>
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
    geom_point(data=dfull_redundancy_sp,
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

![](indices_redundancia_funcional_rasgos_efecto_sin_palmas_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



```r
world <- getMap(resolution = "low")

clipper_costarica <- as(extent(-84.5, -83.75, 10, 10.80), "SpatialPolygons")
proj4string(clipper_costarica) <- CRS(proj4string(world))
costarica_clip <- raster::intersect(world, clipper_costarica)
costarica_clip<- fortify(costarica_clip)

Q_sp <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull_redundancy_sp,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull_redundancy_sp$Q,shape=forest_type))+

    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "Rao Q sin palmas")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))


U_sp <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull_redundancy_sp,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull_redundancy_sp$U,shape=forest_type))+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "Uniqueness sin palmas")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))

redundancy_sp <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull_redundancy_sp,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull_redundancy_sp$redundancy_sp,shape=forest_type))+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "Redundancy sin palmas")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))

#https://cran.r-project.org/web/packages/cowplot/vignettes/plot_grid.html
theme_set(theme_cowplot(font_size=30)) 
plot_grid(Q_sp, U_sp ,redundancy_sp)
```

![](indices_redundancia_funcional_rasgos_efecto_sin_palmas_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



