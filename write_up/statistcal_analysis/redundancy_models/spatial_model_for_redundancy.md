---
title: "Spatial model for redundancy"
author: "Erick Calderón-Morales"
date: "March 28, 2019"
output: 
  html_document:
    code_folding: show
    fig_height: 8
    fig_width: 10
    keep_md: yes
    toc: yes
    toc_depth: 2
    toc_float: yes
---



#Model Formulation 
##Modelo lineal sin componente espacial 

$$redundancia \sim gamma (\mu_i, \sigma^2) $$
$$\mu_i = \frac{\alpha}{\beta},  \sigma^2 =  \frac{\alpha}{\beta^2}$$


$$ log(\mu_i) = \beta_1 + \beta_2FOREST-TYPE + \beta_3LIMO+ \beta_4CLAY + \beta_5pH + \beta_6ACIDITY + $$ 
$$\beta_7K + \beta_8P + \beta_9OrganicMatter +$$
  $$\beta_{10}PREC + \beta_{11}PRECDRIEST + $$ 
  $$\beta_{12}TEMP + \beta_{13}TEMPSD + \epsilon  $$
  

##Codigo INLA


```r
library(INLA)
#Estandarizar todas las variables 
data <- scale(data)

#Modelo 1
m1 <- inla(redundancia ~ FOREST-TYPE*LIMO*CLAY*pH*ACIDITY*
                         K*P*OrganicMatter*
                         PREC*PRECDRIEST*
                         TEMP*TEMPSD,
           family = "gamma",
           control.predictor = list(compute= TRUE),
           control.compute = list(dic = TRUE, 
                                  waic = TRUE), 
           
           #Esta parte no la entiendo
           control.family = list(
             link = "log",
             hyper = list(
               prec =list(
                 prior = "loggamma",
                 param = c(1, 0.5)
               )
             )
           ),
           data = data
)
```




#Preguntas
+ Cuando se estandarizan las variables ¿se estandariza la variable de respuesta?
+ ¿Seria bueno especificar varios modelos (uno para caracteristicas fisicas,quimicas, ect) o solo uno?
+ ¿Es bueno especificar random interceps y random slopes? o ¿solo random interceps? 
+ ¿Esta bien el moment matching?
+ PRECDRIEST tiene una alta correlacion y un alto VIF sin embargo me interesa su informacion que podria dar esta variable, ¿que se puede hacer?




