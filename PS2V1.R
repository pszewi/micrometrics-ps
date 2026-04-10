# ----------------
# setup
# ----------------
# Template of R script to answer problem set
# Group number: 
# Group composition: Madelief van Weerdenburg, Nicollo Zambello, Jakub Przewoski 

### 0. Load packages ###
# install.packages("haven")        # For reading Stata/SPSS/SAS files
# install.packages("dplyr")        # For data manipulation
# install.packages("plm")          # For panel data analysis
# install.packages("fixest")       # For fixed effects estimation
# install.packages("ggplot2")      # For data visualization
# install.packages("tidyr")        # For data tidying
# install.packages("bacondecomp")  # For Bacon decomposition
# install.packages("did")          # For difference-in-differences analysis
# install.packages("modelsummary") # For regression tables
# install.packages("TwoWayFEWeights") # For TWFE weights analysis


library(haven)
library(dplyr)
library(plm)
library(fixest)
library(ggplot2)
library(tidyr)
library(bacondecomp)
# library(did)        # Uncomment if using Callaway & Sant’Anna's estimator
library(modelsummary) # For regression tables
library(TwoWayFEWeights) # If implementing TWFE weights

#excercise 1
# -------- a --------
#Given that divorce rates are
#an average computed in each state and the variable stpop provides the population in
#each of these states, which is the weight you should use when reporting the evolution
#of divorce rates or a regression of divorce rates on unilateral divorce laws to 
#match the analysis in Wolfers (2006)?

#Wolfers uses the metric of number of divorces per 1,000 people per each US state 
#for the div_rate variable. This creates a general metric with which you can properly 
#compare between states, but when one wants to report the evolution of divorce rates 
#or to run a regression of divorce rates on unilateral divorce laws, it is necessary to 
#give each state the proper weight in order to not over account for smaller states and 
#under account for larger states. (not having a bias toward smaller states).
#The weight type we use for this is Analytic Weights (aweight / cellsize) as it 
#corrects for cell means with different group sizes (stpop). Furthermore, it corrects
#for the heteroskedasticity in the standard deviations, which would naturally
#occur due to to different group sizes (here, the state populations).



