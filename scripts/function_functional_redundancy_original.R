#Measuring the functional redundancy of biological communities: a quantitative 
#guide

#   -----------------------------------------------------------------------
#R function “uniqueness” for calculating the species-level coefficients   
#and Vi and the community-level coefficients N (species richness), 
#Q (quadratic diversity), D (Simpson diversity), and U (uniqueness).

# Arguments ---------------------------------------------------------------
#comm: A matrix or a data frame of N plots × S species containing the 
#abundance  or incidence (0/1) of all species in the in plots. Columns are 
#species and plots are rows 

#dis: An object of class 'dist' containing the functional distances among 
#species

#tol: A tolerance threshold (a value less than tol is considered as null)

#abundance	A logical. If TRUE abundance data are used when available; 
#if FALSE incidence (0/1) data are used.



# Output ------------------------------------------------------------------

#Output: the function uniqueness returns a list of three data frames. 

#The  first data frame named Kbar gives values for Ki’s per species (rows) 
#and community (columns). 

#The second data frame named V gives values for Vi’s per species (rows) and 
#community (columns). 

#The third data set gives values for coefficients N (species richness), 
#Q (quadratic diversity),D (Simpson diversity), and U (uniqueness) per 
#community; in this third data  frame coefficients are columns and 
#communities are rows, the coefficients  are thus calculated per community 
#only.
