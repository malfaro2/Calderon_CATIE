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
## -- Attaching packages ------ tidyverse 1.2.1 --
```

```
## v ggplot2 3.1.0       v purrr   0.3.2  
## v tibble  2.1.1       v dplyr   0.8.0.1
## v tidyr   0.8.3       v stringr 1.4.0  
## v readr   1.3.1       v forcats 0.4.0
```

```
## -- Conflicts --------- tidyverse_conflicts() --
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
dredundacia_eff <- read.csv("C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_indices_eff_redundancy.csv", header = T)
dredundacia_eff <- dredundacia_eff %>% clean_names()


dredundanica_eff_sp <- read.csv("C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_indices_eff_redundancy_sp.csv", header=T)
dredundanica_eff_sp <- dredundanica_eff_sp %>% 
rename(n_sp=N, d_sp=D, q_sp= Q, u_sp= U)

dparcelas <- read.csv("C:/tesis_catie/Calderon_CATIE/data/clean/data_posicion_parcelas.csv", header=T)
```



##Indices Redundancia, uniqueness y Rao Rasgos effecto

```r
#Join data

 left_join(dredundacia_eff, dredundanica_eff_sp, by="plot") %>% 
  left_join(., dparcelas, by="plot") %>% 
  
  gather("n","d","q","u","redundancy","n_sp","d_sp","q_sp","u_sp","redundancy_sp",key="indices", value ="valor") %>% 
  group_by(indices, forest_type) %>% 
  summarise(mean = 
              mean(valor)) %>% 
  arrange(forest_type) %>% 
  print(n=30)
```

```
## # A tibble: 30 x 3
## # Groups:   indices [10]
##    indices       forest_type   mean
##    <chr>         <fct>        <dbl>
##  1 d             Foothills    0.888
##  2 d_sp          Foothills    0.906
##  3 n             Foothills   16.1  
##  4 n_sp          Foothills   14.6  
##  5 q             Foothills    0.147
##  6 q_sp          Foothills    0.156
##  7 redundancy    Foothills    0.834
##  8 redundancy_sp Foothills    0.828
##  9 u             Foothills    0.166
## 10 u_sp          Foothills    0.172
## 11 d             P.macroloba  0.804
## 12 d_sp          P.macroloba  0.724
## 13 n             P.macroloba 14.0  
## 14 n_sp          P.macroloba 11.2  
## 15 q             P.macroloba  0.101
## 16 q_sp          P.macroloba  0.109
## 17 redundancy    P.macroloba  0.874
## 18 redundancy_sp P.macroloba  0.851
## 19 u             P.macroloba  0.126
## 20 u_sp          P.macroloba  0.149
## 21 d             Q.paraensis  0.856
## 22 d_sp          Q.paraensis  0.887
## 23 n             Q.paraensis 17.5  
## 24 n_sp          Q.paraensis 14.0  
## 25 q             Q.paraensis  0.119
## 26 q_sp          Q.paraensis  0.148
## 27 redundancy    Q.paraensis  0.861
## 28 redundancy_sp Q.paraensis  0.834
## 29 u             Q.paraensis  0.139
## 30 u_sp          Q.paraensis  0.166
```

```r
#ggplot(data=djoin, aes(indices, mean)) + 
# geom_bar(stat = "identity") + 
# facet_grid(.~forest_type)+
#  theme(axis.text.x = element_text(angle = 90))
```

##Indices Redundancia, uniqueness y Rao Rasgos respuesta


```r
#Cargar datos
dredundacia_resp <- read.csv("C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_indices_resp_redundancy.csv", header = T)
dredundacia_resp <- dredundacia_resp %>% clean_names()


dredundanica_resp_sp <- read.csv("C:/tesis_catie/Calderon_CATIE/data/resultados_csv/data_indices_resp_redundancy_sp.csv", header=T)
dredundanica_resp_sp <- dredundanica_resp_sp %>% 
rename(n_sp=N, d_sp=D, q_sp= Q, u_sp= U)

dparcelas <- read.csv("C:/tesis_catie/Calderon_CATIE/data/clean/data_posicion_parcelas.csv", header=T)
```



```r
#Join data

 left_join(dredundacia_resp, dredundanica_resp_sp, by="plot") %>% 
  left_join(., dparcelas, by="plot") %>% 
  
  gather("n","d","q","u","redundancy","n_sp","d_sp","q_sp","u_sp","redundancy_sp",key="indices", value ="valor") %>% 
  group_by(indices, forest_type) %>% 
  summarise(mean = 
              mean(valor)) %>% 
  arrange(forest_type) %>% 
  print(n=30)
```

```
## # A tibble: 30 x 3
## # Groups:   indices [10]
##    indices       forest_type   mean
##    <chr>         <fct>        <dbl>
##  1 d             Foothills    0.888
##  2 d_sp          Foothills    0.906
##  3 n             Foothills   16.1  
##  4 n_sp          Foothills   14.6  
##  5 q             Foothills    0.338
##  6 q_sp          Foothills    0.353
##  7 redundancy    Foothills    0.620
##  8 redundancy_sp Foothills    0.610
##  9 u             Foothills    0.380
## 10 u_sp          Foothills    0.390
## 11 d             P.macroloba  0.804
## 12 d_sp          P.macroloba  0.724
## 13 n             P.macroloba 14.0  
## 14 n_sp          P.macroloba 11.2  
## 15 q             P.macroloba  0.344
## 16 q_sp          P.macroloba  0.323
## 17 redundancy    P.macroloba  0.568
## 18 redundancy_sp P.macroloba  0.550
## 19 u             P.macroloba  0.432
## 20 u_sp          P.macroloba  0.450
## 21 d             Q.paraensis  0.856
## 22 d_sp          Q.paraensis  0.887
## 23 n             Q.paraensis 17.5  
## 24 n_sp          Q.paraensis 14.0  
## 25 q             Q.paraensis  0.254
## 26 q_sp          Q.paraensis  0.287
## 27 redundancy    Q.paraensis  0.703
## 28 redundancy_sp Q.paraensis  0.675
## 29 u             Q.paraensis  0.297
## 30 u_sp          Q.paraensis  0.325
```

