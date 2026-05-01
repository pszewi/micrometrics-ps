# ----------------
# setup
# ----------------
# Template of R script to answer problem set
# Group number: 
# Group composition: Madelief van Weerdenburg, Nicollo Zambello, Jakub Przewoski 

# Get the username
user <- Sys.info()["user"]
print(user)

# Define file path conditionally
if (user == "erick") {
    filepath <- "/home/erick/TEMP/"
} else if (user == "pszewi") {
    filepath <- "~/win/Documents/Studies/MSc Econ/subjects/2-semester/2-micrometrics/problem sets/PS2/"
} else if (user == "bogiano1945") {
    filepath <- "/Users/bogiano1945/Desktop"
} else if (user == "C") {
    filepath <- "/FILE/PATH/C/"
} else {    
    filepath <- ""  # Default case if user is not listed
}

setwd(filepath)
# Print the selected file path
print(paste("File path set to:", filepath))

# ----------------
# imports
# ---------------

# installing and importing libraries as needed 
cran_packages <- c("dplyr", "stargazer", "tidyr", "sandwich",
                   "lmtest", "boot", "hdm", "openxlsx", "grf",
                   "ggplot2", "modelsummary", "TwoWayFEWeights",
									 "fixest", 'bacondecomp', "haven", "plm")

for (pkg in cran_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ---------------
set.seed("12345")
# ---------------
# ---------------
# funcs
# ---------------


# ---------------
# EXE 1
# ---------------

# ---- a) -------
# ---- b) -------
# ---- c) -------
# ---- d) -------
# ---- e) -------
# ---- f) -------
# ---- g) -------
# ---- h) -------
# ---- i) -------
# ---- j) -------
# ---- k) -------

# ---------------
# EXE 2
# ---------------


# ---- a) -------
# ---- b) -------
# ---- c) -------
