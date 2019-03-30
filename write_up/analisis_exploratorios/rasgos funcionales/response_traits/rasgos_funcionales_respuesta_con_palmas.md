---
title: "Analisis exploratorio rasgos funcionales respuesta con palmas"
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
dim(dresp)
```

```
## [1] 257   6
```

```r
str(dresp)
```

```
## 'data.frame':	257 obs. of  6 variables:
##  $ coespec           : Factor w/ 257 levels "ABARAD","ABARRA",..: 1 37 235 236 237 97 55 225 233 69 ...
##  $ fijacion_nitrogeno: Factor w/ 2 levels "NO","SI": 2 NA 1 1 1 1 1 1 1 1 ...
##  $ dispersion        : Factor w/ 4 levels "DA","DANI","DVAR",..: 3 4 4 4 4 2 2 2 2 2 ...
##  $ sist_sexual       : Factor w/ 4 levels "D","H","M","P": 2 2 2 2 2 1 2 2 2 2 ...
##  $ polinizacion      : Factor w/ 5 levels "PAV","PI","PMA",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ resprout_capacity : int  20 20 100 100 100 20 20 20 20 20 ...
```

```r
#Especies
length(unique(despecies$coespec))
```

```
## [1] 257
```

```r
#Parcelas
length(unique(despecies$plot))
```

```
## [1] 127
```


```r
summary(dresp)
```

```
##     coespec    fijacion_nitrogeno dispersion sist_sexual polinizacion
##  ABARAD :  1   NO  :212           DA  : 29   D   : 55    PAV : 12    
##  ABARRA :  1   SI  : 41           DANI:182   H   :186    PI  :219    
##  ALCOFL :  1   NA's:  4           DVAR: 18   M   : 14    PMA :  6    
##  AMPEMA :  1                      DW  : 28   P   :  1    PVAR: 17    
##  ANDIIN :  1                                 NA's:  1    PW  :  2    
##  ANNOMO :  1                                             NA's:  1    
##  (Other):251                                                         
##  resprout_capacity
##  Min.   :  0.00   
##  1st Qu.: 20.00   
##  Median : 20.00   
##  Mean   : 26.85   
##  3rd Qu.: 40.00   
##  Max.   :100.00   
## 
```

#Medidas de resumen para los rasgos de respuesta 

##Medidas de resumen por rasgo

```r
dresp %>% gather(colnames(dresp[,-1])  , key = "rasgo", value = "valor_del_rasgo") %>% 
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
   <td style="text-align:right;"> 11.284 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DANI </td>
   <td style="text-align:right;"> 182 </td>
   <td style="text-align:right;"> 70.817 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DVAR </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 7.004 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DW </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 10.895 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.556 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:right;"> 212 </td>
   <td style="text-align:right;"> 82.490 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> SI </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 15.953 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.389 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PAV </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 4.669 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PI </td>
   <td style="text-align:right;"> 219 </td>
   <td style="text-align:right;"> 85.214 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PMA </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 2.335 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PVAR </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 6.615 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PW </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0.778 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1.946 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.556 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:right;"> 187 </td>
   <td style="text-align:right;"> 72.763 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:right;"> 45 </td>
   <td style="text-align:right;"> 17.510 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 6.226 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.389 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:right;"> 55 </td>
   <td style="text-align:right;"> 21.401 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> H </td>
   <td style="text-align:right;"> 186 </td>
   <td style="text-align:right;"> 72.374 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 5.447 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> P </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.389 </td>
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
dfull <- left_join(despecies, dparcelas, by="plot" )
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
   <td style="text-align:right;"> 121 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 145 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 182 </td>
  </tr>
</tbody>
</table>


##Medidas de resumen para cada rasgo por tipo de bosque

```r
dfull <- left_join(dfull, dresp, by="coespec" ) %>% select(-c(CRTM_90_X,CRTM_90_Y))

dfull %>% gather(colnames(dresp[,-1])  , key = "rasgo", value = "valor_del_rasgo") %>% 
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
   <td style="text-align:right;"> 13.103 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DANI </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 105 </td>
   <td style="text-align:right;"> 72.414 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DVAR </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 4.138 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DW </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 10.345 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.690 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 122 </td>
   <td style="text-align:right;"> 84.138 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> SI </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 15.172 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.690 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PAV </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.517 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PI </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 123 </td>
   <td style="text-align:right;"> 84.828 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PMA </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2.069 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PVAR </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 6.897 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.759 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2.069 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 95 </td>
   <td style="text-align:right;"> 65.517 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 29 </td>
   <td style="text-align:right;"> 20.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 9.655 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 42 </td>
   <td style="text-align:right;"> 28.966 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> H </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 91 </td>
   <td style="text-align:right;"> 62.759 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 8.276 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 9.341 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DANI </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 132 </td>
   <td style="text-align:right;"> 72.527 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DVAR </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 7.692 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DW </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 10.440 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.549 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 151 </td>
   <td style="text-align:right;"> 82.967 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> SI </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 30 </td>
   <td style="text-align:right;"> 16.484 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.549 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PAV </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> 3.297 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PI </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 155 </td>
   <td style="text-align:right;"> 85.165 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PMA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 2.198 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PVAR </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 8.242 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PW </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.549 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2.747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1.648 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 133 </td>
   <td style="text-align:right;"> 73.077 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 32 </td>
   <td style="text-align:right;"> 17.582 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 4.945 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.549 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 22.527 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> H </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:right;"> 70.330 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 6.044 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> P </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.549 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 9.091 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DANI </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 89 </td>
   <td style="text-align:right;"> 73.554 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DVAR </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 5.785 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> dispersion </td>
   <td style="text-align:left;"> DW </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 11.570 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.653 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NO </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 102 </td>
   <td style="text-align:right;"> 84.298 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> SI </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 14.050 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.826 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PAV </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2.479 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PI </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 106 </td>
   <td style="text-align:right;"> 87.603 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PMA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.653 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PVAR </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 6.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> PW </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.826 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 3.306 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 100 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.826 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 86 </td>
   <td style="text-align:right;"> 71.074 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:right;"> 20.661 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4.132 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.826 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> D </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 18.182 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> H </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 88 </td>
   <td style="text-align:right;"> 72.727 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> M </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 8.264 </td>
  </tr>
</tbody>
</table>








