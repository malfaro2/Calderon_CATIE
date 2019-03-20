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
## [1] 257   7
```

```r
str(dresp)
```

```
## 'data.frame':	257 obs. of  7 variables:
##  $ coespec           : Factor w/ 257 levels "ABARAD","ABARRA",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ fijacion_nitrogeno: Factor w/ 2 levels "NO","SI": 2 2 1 1 2 1 1 1 1 1 ...
##  $ dispersion        : Factor w/ 4 levels "DA","DANI","DVAR",..: 3 3 2 2 2 1 3 2 4 4 ...
##  $ sist_sexual       : Factor w/ 4 levels "D","H","M","P": 2 2 1 2 2 2 2 2 2 2 ...
##  $ polinizacion      : Factor w/ 5 levels "PAV","PI","PMA",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ tasacrecimiento   : int  NA NA 4 1 5 4 6 5 NA NA ...
##  $ resprout_capacity : int  NA 20 60 20 20 20 40 20 NA NA ...
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
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1.556 </td>
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
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.389 </td>
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
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.389 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:right;"> 155 </td>
   <td style="text-align:right;"> 60.311 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:right;"> 44 </td>
   <td style="text-align:right;"> 17.121 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 5.837 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 37 </td>
   <td style="text-align:right;"> 14.397 </td>
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
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.389 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 7.782 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 5.447 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 10.117 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 7.393 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 10.117 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 28 </td>
   <td style="text-align:right;"> 10.895 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 124 </td>
   <td style="text-align:right;"> 48.249 </td>
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
   arrange(forest_type) %>% 
  
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
   <td style="text-align:left;"> fijacion_nitrogeno </td>
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
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.690 </td>
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
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.690 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 80 </td>
   <td style="text-align:right;"> 55.172 </td>
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
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 11.724 </td>
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
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 7.586 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 5.517 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 13 </td>
   <td style="text-align:right;"> 8.966 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 9.655 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 12.414 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 13.103 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Foothills </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 42.759 </td>
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
   <td style="text-align:left;"> fijacion_nitrogeno </td>
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
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
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
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.549 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 110 </td>
   <td style="text-align:right;"> 60.440 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 40 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 17.033 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> 60 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 4.396 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 27 </td>
   <td style="text-align:right;"> 14.835 </td>
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
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.549 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> 8.791 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 6.044 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 12.088 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 15 </td>
   <td style="text-align:right;"> 8.242 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 12.088 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 26 </td>
   <td style="text-align:right;"> 14.286 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> P.macroloba </td>
   <td style="text-align:right;"> 70 </td>
   <td style="text-align:right;"> 38.462 </td>
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
   <td style="text-align:left;"> fijacion_nitrogeno </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1.653 </td>
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
   <td style="text-align:left;"> polinizacion </td>
   <td style="text-align:left;"> NA </td>
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
   <td style="text-align:right;"> 69 </td>
   <td style="text-align:right;"> 57.025 </td>
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
   <td style="text-align:left;"> resprout_capacity </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 14.050 </td>
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
  <tr>
   <td style="text-align:left;"> sist_sexual </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.826 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 9.917 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 4.132 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 9.917 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 8.264 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 6.612 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 17.355 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tasacrecimiento </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Q.paraensis </td>
   <td style="text-align:right;"> 53 </td>
   <td style="text-align:right;"> 43.802 </td>
  </tr>
</tbody>
</table>








