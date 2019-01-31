rm(list = ls())

#   -----------------------------------------------------------------------
#comm	A matrix or a data frame of N plots × S species containing the 
#abundance or incidence (0/1) of all species in the in plots. 
#Columns are species and plots are rows

#dis	An object of class 'dist' containing the functional distances 
#among species

#tol	A tolerance threshold (a value less than tol is considered as null)

#abundance	A logical. If TRUE abundance data are used when available; 
#if FALSE incidence (0/1) data are used.


# cargar funcion uniqueness -----------------------------------------------
source("scripts/functions/function_functional_redundancy_original.R")

# Cargar data -------------------------------------------------------------

# comm --------------------------------------------------------------------
#Data parcelas por especies
#Columns are species and plots are rows 

abundancia <- read.table("clipboard", header=T)
abundancia <-   t(abundancia)


# dis ---------------------------------------------------------------------
#Data rasgos: se deben convertir objeto dis
#dis: An object of class 'dist' containing the functional distances among 
#species
traits <- read.table("clipboard", header=T)
traits

#Transformar traits a distancias
dist <- dist(, method = "euclidean") 


# Funcion -----------------------------------------------------------------
Uni <- uniqueness(com, dis, abundance=TRUE)