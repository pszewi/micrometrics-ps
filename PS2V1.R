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
