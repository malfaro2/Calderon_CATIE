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


#Calculo de los indices de diversidad funcional 


```r
#Ordenar los nombres 
#target <- colnames(dabund_relativa) 
#deff <- deff[match(target, row.names(dresp)),]
```

__Indices diversidad funcional ponderados por la abundancia relativa(w.abund=T) de las especies presentes (total de individuos presentes 4801) y los rasgos estan estandarizados con media 0 y misma varianza.__


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



```r
indices_abunrelativa <- dbFD(deff[colnames(dabund_relativa),],
     dabund_relativa, w.abun = T,stand.x = T)
```

```
## Warning in is.euclid(x.dist): Zero distance(s)

## Warning in is.euclid(x.dist): Zero distance(s)
```

```
## FRic: No dimensionality reduction was required. All 5 PCoA axes were kept as 'traits'.
```




```r
#Obtener indices Feve, Fdis, Fdiv
indices <-   data.frame(indices_abunrelativa$FEve) %>% 
  data.frame(indices_abunrelativa$FDiv) %>% 
  data.frame(indices_abunrelativa$FDis) %>% 
  rownames_to_column("plot") %>% 
  cbind() %>% 
  rename(feve= indices_abunrelativa.FEve   ,fdiv = indices_abunrelativa.FDiv, fdis= indices_abunrelativa.FDis )

#Obtener CWMs
cwm <- data.frame(indices_abunrelativa$CWM) 
colnames(cwm) <-  paste("cwm", colnames(cwm), sep = "_") 
cwm <- cwm %>% rownames_to_column("plot")

#Guardar  archivos .csv
#write.csv(indices, "C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_indices_eff_fdiver.csv")
#write.csv(cwm, "C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_cwm_eff.csv")
```



##Medidas de resumen para FDis, FEve, FDiv

###Medidas de resumen por indice

```r
indices %>% gather("feve", "fdiv", "fdis" , key = "rasgo", value = "valor_del_rasgo") %>% 
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
   <td style="text-align:left;"> fdis </td>
   <td style="text-align:right;"> 1.8092915 </td>
   <td style="text-align:right;"> 0.3095519 </td>
   <td style="text-align:right;"> 2.5931772 </td>
   <td style="text-align:right;"> 0.9092283 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fdiv </td>
   <td style="text-align:right;"> 0.7150348 </td>
   <td style="text-align:right;"> 0.0839323 </td>
   <td style="text-align:right;"> 0.9404433 </td>
   <td style="text-align:right;"> 0.4425307 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> feve </td>
   <td style="text-align:right;"> 0.7129368 </td>
   <td style="text-align:right;"> 0.0816633 </td>
   <td style="text-align:right;"> 0.8699363 </td>
   <td style="text-align:right;"> 0.4902044 </td>
  </tr>
</tbody>
</table>

###Medidas de resumen para cada indice por tipo de bosque

```r
dfull <- left_join(indices,dparcelas, by="plot") 
```

```
## Warning: Column `plot` joining character vector and factor, coercing into
## character vector
```

```r
dfull %>% gather("feve", "fdiv", "fdis" , key = "rasgo", value = "valor_del_rasgo") %>% 
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
   <td style="text-align:left;"> fdis </td>
   <td style="text-align:right;"> 2.1274187 </td>
   <td style="text-align:right;"> 0.2625008 </td>
   <td style="text-align:right;"> 2.5931772 </td>
   <td style="text-align:right;"> 1.5163399 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> fdis </td>
   <td style="text-align:right;"> 1.6702509 </td>
   <td style="text-align:right;"> 0.2400224 </td>
   <td style="text-align:right;"> 2.2306126 </td>
   <td style="text-align:right;"> 0.9092283 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> fdis </td>
   <td style="text-align:right;"> 1.7904563 </td>
   <td style="text-align:right;"> 0.2507995 </td>
   <td style="text-align:right;"> 2.2372050 </td>
   <td style="text-align:right;"> 1.2492650 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> fdiv </td>
   <td style="text-align:right;"> 0.7714724 </td>
   <td style="text-align:right;"> 0.0584756 </td>
   <td style="text-align:right;"> 0.9404433 </td>
   <td style="text-align:right;"> 0.6426793 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> fdiv </td>
   <td style="text-align:right;"> 0.6807111 </td>
   <td style="text-align:right;"> 0.0803319 </td>
   <td style="text-align:right;"> 0.8277621 </td>
   <td style="text-align:right;"> 0.4425307 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> fdiv </td>
   <td style="text-align:right;"> 0.7340045 </td>
   <td style="text-align:right;"> 0.0781528 </td>
   <td style="text-align:right;"> 0.8868150 </td>
   <td style="text-align:right;"> 0.5907243 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> feve </td>
   <td style="text-align:right;"> 0.7828046 </td>
   <td style="text-align:right;"> 0.0539625 </td>
   <td style="text-align:right;"> 0.8699363 </td>
   <td style="text-align:right;"> 0.6190301 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> feve </td>
   <td style="text-align:right;"> 0.6789605 </td>
   <td style="text-align:right;"> 0.0769472 </td>
   <td style="text-align:right;"> 0.8229988 </td>
   <td style="text-align:right;"> 0.4902044 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> feve </td>
   <td style="text-align:right;"> 0.7167477 </td>
   <td style="text-align:right;"> 0.0689337 </td>
   <td style="text-align:right;"> 0.8258209 </td>
   <td style="text-align:right;"> 0.5645684 </td>
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
    geom_point(data=dfull,
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

![](indices_diversidad_funcional_rasgos_efecto_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


```r
world <- getMap(resolution = "low")

clipper_costarica <- as(extent(-84.5, -83.75, 10, 10.80), "SpatialPolygons")
proj4string(clipper_costarica) <- CRS(proj4string(world))
costarica_clip <- raster::intersect(world, clipper_costarica)
costarica_clip<- fortify(costarica_clip)

feve <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull$feve,shape=forest_type))+
    
    #geom_point(dfull, mapping=aes(x=longitude,y=latitude),
    #           colour=dfull$feve, alpha=0.5)+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "FEve")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))


fdis <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull$fdis,shape=forest_type))+
    
    #geom_point(dfull, mapping=aes(x=longitude,y=latitude),
    #           colour=dfull$feve, alpha=0.5)+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "FDis")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))

fdiv <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull$fdiv,shape=forest_type))+
    
    #geom_point(dfull, mapping=aes(x=longitude,y=latitude),
    #           colour=dfull$feve, alpha=0.5)+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "FDiv")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))

#https://cran.r-project.org/web/packages/cowplot/vignettes/plot_grid.html
theme_set(theme_cowplot(font_size=30)) 
plot_grid(feve, fdiv, fdis,hjust = 2, vjust = 3)
```

![](indices_diversidad_funcional_rasgos_efecto_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


##Medidas de resumen para CWM


###Medidas de resumen por CWM

```r
cwm %>% gather("cwm_afe","cwm_cfms","cwm_dm","cwm_n","cwm_p" , key = "rasgo", value = "valor_del_rasgo") %>% 
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
   <td style="text-align:left;"> cwm_afe </td>
   <td style="text-align:right;"> 12.5306124 </td>
   <td style="text-align:right;"> 1.7259972 </td>
   <td style="text-align:right;"> 17.1441298 </td>
   <td style="text-align:right;"> 9.8697501 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cwm_cfms </td>
   <td style="text-align:right;"> 412.1749678 </td>
   <td style="text-align:right;"> 20.4872753 </td>
   <td style="text-align:right;"> 446.7624960 </td>
   <td style="text-align:right;"> 346.8028571 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cwm_dm </td>
   <td style="text-align:right;"> 0.4561811 </td>
   <td style="text-align:right;"> 0.0697643 </td>
   <td style="text-align:right;"> 0.6246429 </td>
   <td style="text-align:right;"> 0.2673913 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cwm_n </td>
   <td style="text-align:right;"> 23.2168212 </td>
   <td style="text-align:right;"> 2.2554404 </td>
   <td style="text-align:right;"> 28.6220832 </td>
   <td style="text-align:right;"> 19.1760002 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cwm_p </td>
   <td style="text-align:right;"> 1.0487746 </td>
   <td style="text-align:right;"> 0.1065416 </td>
   <td style="text-align:right;"> 1.3155000 </td>
   <td style="text-align:right;"> 0.8082143 </td>
  </tr>
</tbody>
</table>

###Medidas de resumen para cada cwm por tipo de bosque

```r
dfull_cwm <- left_join(cwm,dparcelas, by="plot") 
```

```
## Warning: Column `plot` joining character vector and factor, coercing into
## character vector
```

```r
dfull_cwm %>% gather("cwm_afe","cwm_cfms","cwm_dm","cwm_n","cwm_p" , key = "rasgo", value = "valor_del_rasgo") %>% 
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
   <td style="text-align:left;"> cwm_afe </td>
   <td style="text-align:right;"> 14.2043477 </td>
   <td style="text-align:right;"> 1.6970918 </td>
   <td style="text-align:right;"> 17.1168966 </td>
   <td style="text-align:right;"> 11.1040001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> cwm_afe </td>
   <td style="text-align:right;"> 12.4534590 </td>
   <td style="text-align:right;"> 1.2614046 </td>
   <td style="text-align:right;"> 17.1441298 </td>
   <td style="text-align:right;"> 10.5252173 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> cwm_afe </td>
   <td style="text-align:right;"> 10.9196981 </td>
   <td style="text-align:right;"> 0.8751565 </td>
   <td style="text-align:right;"> 13.5610345 </td>
   <td style="text-align:right;"> 9.8697501 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> cwm_cfms </td>
   <td style="text-align:right;"> 390.4213962 </td>
   <td style="text-align:right;"> 22.6219317 </td>
   <td style="text-align:right;"> 432.8341176 </td>
   <td style="text-align:right;"> 346.8028571 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> cwm_cfms </td>
   <td style="text-align:right;"> 415.7839807 </td>
   <td style="text-align:right;"> 14.3229885 </td>
   <td style="text-align:right;"> 436.6533307 </td>
   <td style="text-align:right;"> 377.8478046 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> cwm_cfms </td>
   <td style="text-align:right;"> 427.0907215 </td>
   <td style="text-align:right;"> 8.4496912 </td>
   <td style="text-align:right;"> 446.7624960 </td>
   <td style="text-align:right;"> 408.2800000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> cwm_dm </td>
   <td style="text-align:right;"> 0.4741000 </td>
   <td style="text-align:right;"> 0.0621753 </td>
   <td style="text-align:right;"> 0.6083333 </td>
   <td style="text-align:right;"> 0.3471739 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> cwm_dm </td>
   <td style="text-align:right;"> 0.4353927 </td>
   <td style="text-align:right;"> 0.0639451 </td>
   <td style="text-align:right;"> 0.5535897 </td>
   <td style="text-align:right;"> 0.2673913 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> cwm_dm </td>
   <td style="text-align:right;"> 0.4850548 </td>
   <td style="text-align:right;"> 0.0766065 </td>
   <td style="text-align:right;"> 0.6246429 </td>
   <td style="text-align:right;"> 0.3552500 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> cwm_n </td>
   <td style="text-align:right;"> 23.5799207 </td>
   <td style="text-align:right;"> 2.3962414 </td>
   <td style="text-align:right;"> 27.4758824 </td>
   <td style="text-align:right;"> 19.2346341 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> cwm_n </td>
   <td style="text-align:right;"> 24.0902822 </td>
   <td style="text-align:right;"> 1.8436242 </td>
   <td style="text-align:right;"> 28.6220832 </td>
   <td style="text-align:right;"> 20.7181663 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> cwm_n </td>
   <td style="text-align:right;"> 20.8106840 </td>
   <td style="text-align:right;"> 0.8716662 </td>
   <td style="text-align:right;"> 22.3600002 </td>
   <td style="text-align:right;"> 19.1760002 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:left;"> cwm_p </td>
   <td style="text-align:right;"> 1.1444019 </td>
   <td style="text-align:right;"> 0.0997933 </td>
   <td style="text-align:right;"> 1.3155000 </td>
   <td style="text-align:right;"> 0.9236585 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:left;"> cwm_p </td>
   <td style="text-align:right;"> 1.0534656 </td>
   <td style="text-align:right;"> 0.0759407 </td>
   <td style="text-align:right;"> 1.2233333 </td>
   <td style="text-align:right;"> 0.9090698 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:left;"> cwm_p </td>
   <td style="text-align:right;"> 0.9357143 </td>
   <td style="text-align:right;"> 0.0583108 </td>
   <td style="text-align:right;"> 1.0682692 </td>
   <td style="text-align:right;"> 0.8082143 </td>
  </tr>
</tbody>
</table>

```r
world <- getMap(resolution = "low")

clipper_costarica <- as(extent(-84.5, -83.75, 10, 10.80), "SpatialPolygons")
proj4string(clipper_costarica) <- CRS(proj4string(world))
costarica_clip <- raster::intersect(world, clipper_costarica)
costarica_clip<- fortify(costarica_clip)

cwm_afe <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull_cwm$cwm_afe,shape=forest_type))+
    
    #geom_point(dfull, mapping=aes(x=longitude,y=latitude),
    #           colour=dfull$feve, alpha=0.5)+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "CWM AFE")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))

cwm_cfms <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull_cwm$cwm_cfms,shape=forest_type))+
    
    #geom_point(dfull, mapping=aes(x=longitude,y=latitude),
    #           colour=dfull$feve, alpha=0.5)+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "CWM CFMS")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))

cwm_dm <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull_cwm$cwm_dm,shape=forest_type))+
    
    #geom_point(dfull, mapping=aes(x=longitude,y=latitude),
    #           colour=dfull$feve, alpha=0.5)+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "CWM DM")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))

cwm_n <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull_cwm$cwm_n,shape=forest_type))+
    
    #geom_point(dfull, mapping=aes(x=longitude,y=latitude),
    #           colour=dfull$feve, alpha=0.5)+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "CWM N")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))

cwm_p <- ggplot()+
    
    #Mapa de la zona de estudio 
    geom_polygon(data = costarica_clip,
                 aes(x=long,y=lat,group=group),
                 fill="grey")+
    
    #Data de la especie
    geom_point(data=dfull,
               alpha=0.6,position = position_jitter(width=0.04, height=0.04),
               aes(x=longitude,y=latitude, 
                   colour=dfull_cwm$cwm_p,shape=forest_type))+
    
    #geom_point(dfull, mapping=aes(x=longitude,y=latitude),
    #           colour=dfull$feve, alpha=0.5)+
    
    #Le da formato de mapa
    theme_bw()+
    coord_quickmap()+
    guides(colour=guide_legend(tittle="Tipo de Bosque"))+
    #labs(colour = "FEve", shape = "Tipo de Bosque")+
    scale_color_gradient(low="yellow", high="red")+
    labs(colour = "CWM P")+
    theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"))


#https://cran.r-project.org/web/packages/cowplot/vignettes/plot_grid.html
theme_set(theme_cowplot(font_size=30)) 
plot_grid(cwm_afe,cwm_cfms,cwm_dm,cwm_n,cwm_p , nrow = 3)
```

![](indices_diversidad_funcional_rasgos_efecto_files/figure-html/unnamed-chunk-13-1.png)<!-- -->




