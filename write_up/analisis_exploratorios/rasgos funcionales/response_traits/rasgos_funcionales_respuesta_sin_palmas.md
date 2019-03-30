---
title: "Analisis exploratorio rasgos funcionales respuesta sin palmas"
author: "Erick Calderón-Morales"
date: "March 18, 2019"
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




#Objetivo

El objetivo de este Rmartkdown es mostrar las medias de resumen para lo datos de rasgos funcionales respuesta con palmas 


##Significado de las variables 

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Rasgos de respuesta</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Abreviaturas </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="5"><td colspan="2" style="border-bottom: 1px solid;"><strong>Tipo de dispersion</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Anemocoria </td>
   <td style="text-align:left;"> DW </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Hidrocoria </td>
   <td style="text-align:left;"> DH </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Zoocoria </td>
   <td style="text-align:left;"> DANI </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Dos o mas tipos de dispersion </td>
   <td style="text-align:left;"> DVAR </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Autocoria </td>
   <td style="text-align:left;"> DA </td>
  </tr>
  <tr grouplength="4"><td colspan="2" style="border-bottom: 1px solid;"><strong>Sistema sexual</strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Dioico </td>
   <td style="text-align:left;"> D </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Monoico </td>
   <td style="text-align:left;"> M </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Hermafrodita </td>
   <td style="text-align:left;"> H </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Poligama </td>
   <td style="text-align:left;"> P </td>
  </tr>
  <tr grouplength="5"><td colspan="2" style="border-bottom: 1px solid;"><strong>Tipo de polinizacion </strong></td></tr>
<tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Viento </td>
   <td style="text-align:left;"> PW </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Insectos </td>
   <td style="text-align:left;"> PI </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Aves </td>
   <td style="text-align:left;"> PAV </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Dos o mas tipos de polinizacion </td>
   <td style="text-align:left;"> PVAR </td>
  </tr>
  <tr>
   <td style="text-align:left; padding-left: 2em;" indentlevel="1"> Mamiferos </td>
   <td style="text-align:left;"> PMA </td>
  </tr>
</tbody>
</table>





```r
#Dimensiones de los datos
dim(dresp_sp)
```

```
## [1] 253   6
```

```r
str(dresp_sp)
```

```
## 'data.frame':	253 obs. of  6 variables:
##  $ coespec           : Factor w/ 253 levels "ABARAD","ABARRA",..: 221 229 6 84 85 209 239 240 251 10 ...
##  $ fijacion_nitrogeno: Factor w/ 2 levels "NO","SI": 1 1 1 1 1 1 1 1 1 1 ...
##  $ dispersion        : Factor w/ 4 levels "DA","DANI","DVAR",..: 1 3 1 1 1 2 2 2 2 4 ...
##  $ sist_sexual       : Factor w/ 4 levels "D","H","M","P": 3 1 2 2 2 2 2 2 2 2 ...
##  $ polinizacion      : Factor w/ 5 levels "PAV","PI","PMA",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ resprout_capacity : int  60 60 20 20 20 20 20 20 20 20 ...
```

```r
#Especies
length(unique(despecies_sp$coespec))
```

```
## [1] 253
```

```r
#Parcelas
length(unique(dparcelas$plot))
```

```
## [1] 127
```

#Medidas de resumen para los rasgos de respuesta 


```r
summary(dresp_sp)
```

```
##     coespec    fijacion_nitrogeno dispersion sist_sexual polinizacion
##  ABARAD :  1   NO  :208           DA  : 29   D   : 55    PAV : 12    
##  ABARRA :  1   SI  : 41           DANI:178   H   :186    PI  :215    
##  ALCOFL :  1   NA's:  4           DVAR: 18   M   : 10    PMA :  6    
##  AMPEMA :  1                      DW  : 28   P   :  1    PVAR: 17    
##  ANDIIN :  1                                 NA's:  1    PW  :  2    
##  ANNOMO :  1                                             NA's:  1    
##  (Other):247                                                         
##  resprout_capacity
##  Min.   :  0.00   
##  1st Qu.: 20.00   
##  Median : 20.00   
##  Mean   : 27.27   
##  3rd Qu.: 40.00   
##  Max.   :100.00   
## 
```


##Medidas de resumen por rasgo

```r
dresp_sp %>% gather(colnames(dresp_sp[,-1])  , key = "rasgo", value = "valor_del_rasgo") %>% 
  group_by(rasgo,valor_del_rasgo) %>% 
  summarise(n=n())  %>% 
  mutate(porcentaje= round((n/sum(n))*100, 3)) %>%
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),full_width = F)
```

```
## Warning: attributes are not identical across measure variables;
## they will be dropped
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> rasgo </th>
   <th style="text-align:left;"> valor_del_rasgo </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> porcentaje </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DA </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 11.462 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DANI </td>
   <td style="text-align:right;"> 178 </td>
   <td style="text-align:right;"> 70.356 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DVAR </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 7.115 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DW </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 11.067 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.581 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 208 </td>
   <td style="text-align:right;"> 82.213 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> SI </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 16.206 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.395 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PAV </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 4.743 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PI </td>
   <td style="text-align:right;"> 215 </td>
   <td style="text-align:right;"> 84.980 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PMA </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 2.372 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PVAR </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 6.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PW </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.791 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.395 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.581 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:right;"> 187 </td>
   <td style="text-align:right;"> 73.913 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 17.787 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 6.324 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.395 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 21.739 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> H </td>
   <td style="text-align:right;"> 186 </td>
   <td style="text-align:right;"> 73.518 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 3.953 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> P </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.395 </td>
  </tr>
</tbody>
</table>



####Note: Especies sin valor de capacidad de rebrote

A estas especies se les dio el valor del género, el valor de la familia. A 7 especies se les dió el valor de la mediana de las demas



```r
 dresp_37nas <- read.csv("C:/tesis_catie/Calderon_CATIE/data/raw/response_traits/dresp_con_37NAs.csv",header = T)
dresp_37nas %>% select(X,familia, especie,resprout_capacity) %>% 
  filter(resprout_capacity %in% NA) %>% 
  arrange(familia) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),full_width = F)
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> X </th>
   <th style="text-align:left;"> familia </th>
   <th style="text-align:left;"> especie </th>
   <th style="text-align:right;"> resprout_capacity </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> UNONPI </td>
   <td style="text-align:left;"> ANNONACEAE </td>
   <td style="text-align:left;"> Unonopsis_pittieri </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UNONSP </td>
   <td style="text-align:left;"> ANNONACEAE </td>
   <td style="text-align:left;"> Unonopsis_sp. </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> XYLOSE </td>
   <td style="text-align:left;"> ANNONACEAE </td>
   <td style="text-align:left;"> Xylopia_sericophylla </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ASPI01 </td>
   <td style="text-align:left;"> APOCYNACEAE </td>
   <td style="text-align:left;"> Aspidosperma_sp.01 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ASPISP </td>
   <td style="text-align:left;"> APOCYNACEAE </td>
   <td style="text-align:left;"> Aspidosperma_sp. </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ASPISR </td>
   <td style="text-align:left;"> APOCYNACEAE </td>
   <td style="text-align:left;"> Aspidosperma_spruceanum </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ILEXSK </td>
   <td style="text-align:left;"> AQUIFOLIACEAE </td>
   <td style="text-align:left;"> Ilex_skutchii </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TABECH </td>
   <td style="text-align:left;"> BIGNONIACEAE </td>
   <td style="text-align:left;"> Tabebuia_chrysantha </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CEIBPE </td>
   <td style="text-align:left;"> BOMBACACEAE </td>
   <td style="text-align:left;"> Ceiba_pentandra </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CECROB </td>
   <td style="text-align:left;"> CECROPIACEAE </td>
   <td style="text-align:left;"> Cecropia_obtusifolia </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> COUEPO </td>
   <td style="text-align:left;"> CHRYSOBALANACEAE </td>
   <td style="text-align:left;"> Couepia_polyandra </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HIRTSP </td>
   <td style="text-align:left;"> CHRYSOBALANACEAE </td>
   <td style="text-align:left;"> Hirtella_sp. </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LICNHY </td>
   <td style="text-align:left;"> CHRYSOBALANACEAE </td>
   <td style="text-align:left;"> Licania_hypoleuca </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LICNKA </td>
   <td style="text-align:left;"> CHRYSOBALANACEAE </td>
   <td style="text-align:left;"> Licania_kallunkiae </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LICNSA </td>
   <td style="text-align:left;"> CHRYSOBALANACEAE </td>
   <td style="text-align:left;"> Licania_sparsipilis </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LICNSP </td>
   <td style="text-align:left;"> CHRYSOBALANACEAE </td>
   <td style="text-align:left;"> Licania_sp. </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TERMBU </td>
   <td style="text-align:left;"> COMBRETACEAE </td>
   <td style="text-align:left;"> Terminalia_bucidoides </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TERMOL </td>
   <td style="text-align:left;"> COMBRETACEAE </td>
   <td style="text-align:left;"> Terminalia_oblonga </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TERMSP </td>
   <td style="text-align:left;"> COMBRETACEAE </td>
   <td style="text-align:left;"> Terminalia_sp. </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DICHMO </td>
   <td style="text-align:left;"> DICHAPETALACEAE </td>
   <td style="text-align:left;"> Dichapetalum_morenoi </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> STEPCO </td>
   <td style="text-align:left;"> DICHAPETALACEAE </td>
   <td style="text-align:left;"> Stephanopodium_costaricense </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TAPUGU </td>
   <td style="text-align:left;"> DICHAPETALACEAE </td>
   <td style="text-align:left;"> Tapura_guianensis </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PERAAR </td>
   <td style="text-align:left;"> EUPHORBIACEAE </td>
   <td style="text-align:left;"> Pera_arborea </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ABARAD </td>
   <td style="text-align:left;"> FABACEAE/MIM </td>
   <td style="text-align:left;"> Abarema_adenophora </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> COJOCO </td>
   <td style="text-align:left;"> FABACEAE/MIM </td>
   <td style="text-align:left;"> Cojoba_costaricensis </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DIPTPA </td>
   <td style="text-align:left;"> FABACEAE/PAP </td>
   <td style="text-align:left;"> Dipteryx_panamensis </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DUSS01 </td>
   <td style="text-align:left;"> FABACEAE/PAP </td>
   <td style="text-align:left;"> Dussia_sp.01 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DUSS02 </td>
   <td style="text-align:left;"> FABACEAE/PAP </td>
   <td style="text-align:left;"> Dussia_sp.02 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DUSSMA </td>
   <td style="text-align:left;"> FABACEAE/PAP </td>
   <td style="text-align:left;"> Dussia_macroprophyllata </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PTERRO </td>
   <td style="text-align:left;"> FABACEAE/PAP </td>
   <td style="text-align:left;"> Pterocarpus_rohrii </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HUMIDI </td>
   <td style="text-align:left;"> HUMIRIACEAE </td>
   <td style="text-align:left;"> Humiriastrum_diguense </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BROSPA </td>
   <td style="text-align:left;"> MORACEAE </td>
   <td style="text-align:left;"> Brosimum_panamense </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> EUGE01 </td>
   <td style="text-align:left;"> MYRTACEAE </td>
   <td style="text-align:left;"> Eugenia_sp.01 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PODOGU </td>
   <td style="text-align:left;"> PODOCARPACEAE </td>
   <td style="text-align:left;"> Podocarpus_guatemalensis </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LACUPA </td>
   <td style="text-align:left;"> QUIINACEAE </td>
   <td style="text-align:left;"> Lacunaria_panamensis </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> GUETSP </td>
   <td style="text-align:left;"> RUBIACEAE </td>
   <td style="text-align:left;"> Guettarda_sp. </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MORTAN </td>
   <td style="text-align:left;"> TILIACEAE </td>
   <td style="text-align:left;"> Mortoniodendron_anisophyllum </td>
   <td style="text-align:right;"> NA </td>
  </tr>
</tbody>
</table>

##Cantidad de especies por tipo de bosque

```r
dfull <- left_join(despecies_sp, dparcelas, by="plot" )
dfull %>%  group_by(forest_type) %>% 
 summarize(especies = n_distinct(coespec)) %>% 
  arrange(especies) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),full_width = F)
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> forest_type </th>
   <th style="text-align:right;"> especies </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 117 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 141 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 178 </td>
  </tr>
</tbody>
</table>


##Medidas de resumen para cada rasgo por tipo de bosque

```r
dfull <- left_join(dfull, dresp_sp, by="coespec" ) %>% select(-c(CRTM_90_X,CRTM_90_Y))
```

```
## Warning: Column `coespec` joining factors with different levels, coercing
## to character vector
```

```r
dfull %>% gather(colnames(dresp_sp[,-1])  , key = "rasgo", value = "valor_del_rasgo") %>% 
  group_by(rasgo,valor_del_rasgo, forest_type) %>% 
  summarise(n=n_distinct(coespec))  %>%
  
  group_by(forest_type, rasgo) %>% 
   
  mutate(porcentaje= round((n/sum(n))*100, 3)) %>%
  arrange(forest_type) %>% 
  
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),full_width = F)
```

```
## Warning: attributes are not identical across measure variables;
## they will be dropped
```

<table class="table table-striped table-hover table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> rasgo </th>
   <th style="text-align:left;"> valor_del_rasgo </th>
   <th style="text-align:left;"> forest_type </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> porcentaje </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DA </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 13.475 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DANI </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 101 </td>
   <td style="text-align:right;"> 71.631 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DVAR </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4.255 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DW </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 10.638 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.709 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 118 </td>
   <td style="text-align:right;"> 83.688 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> SI </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 15.603 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.709 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PAV </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.674 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PI </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 119 </td>
   <td style="text-align:right;"> 84.397 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PMA </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2.128 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PVAR </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 7.092 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2.128 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:right;"> 67.376 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 20.567 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 9.929 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 29.787 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> H </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 64.539 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.674 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 9.551 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DANI </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:right;"> 71.910 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DVAR </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 7.865 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DW </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 10.674 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.562 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 147 </td>
   <td style="text-align:right;"> 82.584 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> SI </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 16.854 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.562 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PAV </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.371 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PI </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 151 </td>
   <td style="text-align:right;"> 84.831 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PMA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.247 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PVAR </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 8.427 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PW </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.562 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.562 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.685 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 133 </td>
   <td style="text-align:right;"> 74.719 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 17.978 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 5.056 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.562 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 23.034 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> H </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:right;"> 71.910 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 3.933 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> P </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.562 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 9.402 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DANI </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 72.650 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DVAR </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 5.983 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DW </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 11.966 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.709 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 98 </td>
   <td style="text-align:right;"> 83.761 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> SI </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 14.530 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.855 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PAV </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2.564 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PI </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 87.179 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PMA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.709 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PVAR </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 6.838 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PW </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.855 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.855 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 86 </td>
   <td style="text-align:right;"> 73.504 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 21.368 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4.274 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.855 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 18.803 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> H </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 75.214 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 5.128 </td>
  </tr>
</tbody>
</table>
