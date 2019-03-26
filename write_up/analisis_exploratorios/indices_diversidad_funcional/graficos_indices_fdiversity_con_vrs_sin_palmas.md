---
title: "Graficos: indices redundancia con vrs sin palmas effect traits"
author: "Erick Calderón-Morales"
date: "March 26, 2019"
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


```r
#Paquetes
library(tidyverse)
```

```
## -- Attaching packages -------------------------------------------------- tidyverse 1.2.1 --
```

```
## v ggplot2 3.1.0       v purrr   0.3.2  
## v tibble  2.1.1       v dplyr   0.8.0.1
## v tidyr   0.8.3       v stringr 1.4.0  
## v readr   1.3.1       v forcats 0.4.0
```

```
## -- Conflicts ----------------------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(janitor)
```


#Objetivo
Comparar que tanto varian los indices de redundancia funcional con y sin palmas



```r
#Cargar datos
dfdiver_eff <- read.csv("C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_indices_eff_fdiver.csv", header = T)
dfdiver_eff <- dfdiver_eff %>% clean_names()


dfdiver_eff_sp <- read.csv("C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_indices_eff_fdiver_sp.csv", header=T)
dfdiver_eff_sp <- dfdiver_eff_sp %>% clean_names()

dparcelas <- read.csv("C:/tesis_catie/Calderon_CATIE/data/clean/data_posicion_parcelas.csv", header=T)
```



##Indices FEve, FDiv, FDis, Rasgos effecto

```r
#Join data

 left_join(dfdiver_eff,dfdiver_eff_sp, by="plot") %>% 
  left_join(., dparcelas, by="plot") %>% 
  
  gather("fdis","feve","fdiv","fdis_sp","feve_sp","fdiv_sp",key="indices", value ="valor") %>% 
  group_by(indices, forest_type) %>% 
  summarise(mean = 
              mean(valor)) %>% 
  arrange(forest_type) %>% 
  print(n=30)
```

```
## # A tibble: 18 x 3
## # Groups:   indices [6]
##    indices forest_type  mean
##    <chr>   <fct>       <dbl>
##  1 fdis    Foothills   2.13 
##  2 fdis_sp Foothills   2.16 
##  3 fdiv    Foothills   0.771
##  4 fdiv_sp Foothills   0.755
##  5 feve    Foothills   0.783
##  6 feve_sp Foothills   0.804
##  7 fdis    P.macroloba 1.67 
##  8 fdis_sp P.macroloba 1.48 
##  9 fdiv    P.macroloba 0.681
## 10 fdiv_sp P.macroloba 0.691
## 11 feve    P.macroloba 0.679
## 12 feve_sp P.macroloba 0.721
## 13 fdis    Q.paraensis 1.79 
## 14 fdis_sp Q.paraensis 1.81 
## 15 fdiv    Q.paraensis 0.734
## 16 fdiv_sp Q.paraensis 0.782
## 17 feve    Q.paraensis 0.717
## 18 feve_sp Q.paraensis 0.780
```

```r
#ggplot(data=djoin, aes(indices, mean)) + 
# geom_bar(stat = "identity") + 
# facet_grid(.~forest_type)+
#  theme(axis.text.x = element_text(angle = 90))
```

##Indices FEve, FDiv, FDis, Rasgos respuesta


```r
#Cargar datos
dfiver_resp <- read.csv("C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_indices_resp_fdiver.csv", header = T)
dfiver_resp <- dfiver_resp %>% clean_names()


dfiver_resp_sp <- read.csv("C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_indices_resp_fdiver_sp.csv", header=T)
dfiver_resp_sp <- dfiver_resp_sp %>%clean_names()

dparcelas <- read.csv("C:/tesis_catie/Calderon_CATIE/data/clean/data_posicion_parcelas.csv", header=T)
```



```r
#Join data

 left_join(dfiver_resp, dfiver_resp_sp, by="plot") %>% 
  left_join(., dparcelas, by="plot") %>% 
  
  gather("fdis","feve","fdiv","fdis_sp","feve_sp","fdiv_sp",key="indices", value ="valor") %>% 
  group_by(indices, forest_type) %>% 
  summarise(mean = 
              mean(valor)) %>% 
  arrange(forest_type) %>% 
  print(n=30)
```

```
## # A tibble: 18 x 3
## # Groups:   indices [6]
##    indices forest_type  mean
##    <chr>   <fct>       <dbl>
##  1 fdis    Foothills   0.260
##  2 fdis_sp Foothills   0.267
##  3 fdiv    Foothills   0.578
##  4 fdiv_sp Foothills   0.665
##  5 feve    Foothills   0.651
##  6 feve_sp Foothills   0.675
##  7 fdis    P.macroloba 0.289
##  8 fdis_sp P.macroloba 0.270
##  9 fdiv    P.macroloba 0.488
## 10 fdiv_sp P.macroloba 0.585
## 11 feve    P.macroloba 0.554
## 12 feve_sp P.macroloba 0.626
## 13 fdis    Q.paraensis 0.206
## 14 fdis_sp Q.paraensis 0.220
## 15 fdiv    Q.paraensis 0.450
## 16 fdiv_sp Q.paraensis 0.661
## 17 feve    Q.paraensis 0.483
## 18 feve_sp Q.paraensis 0.575
```

