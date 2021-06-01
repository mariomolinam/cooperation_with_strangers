#
# PROJECT: Cooperation with Strangers: Spillover
#           of Community Norms
#
# Authors: Mario D. Molina, Victor Nee, and Hakan Holm
#


# INSTALL and LOAD libraries
packages = c("stargazer", "effects", "MASS")
new_packages = packages[ !( packages %in% installed.packages()[,"Package"] ) ]
if( length(new.packages) ) {
  install.packages(new_packages)
  invisible(lapply(packages, library, character.only=TRUE))
} else {
  invisible(lapply(packages, library, character.only=TRUE))
}

# set working directory 
setwd("./.")

# create empty folder to store all results
dir.create("./results")


#### Y A N G Z I - D E L T A   S T U D Y  
############################################
source("./yangzi-delta_study.R")

#### M T U R K   E X P E R I M E N T
############################################
source("./mturk_study.R")

#### C R E D A M O   E X P E R I M E N T
############################################
source("./credamo_study.R")
