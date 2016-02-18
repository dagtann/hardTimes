## === Heidelberg Performanz Papier ========================
## === Master script =======================================
## === (c) DT, 08.2012 =====================================
## =========================================================
rm(list=ls())                             ## Clear Workspace
if(Sys.info()['user'] == 'dag') {
  options(help_type = 'html')
  pathData <- "/Users/dag/Dropbox/data/dd/data"
  pathCode <- "/Users/dag/github/hardTimes/r"
  pathOut <- "/Users/dag/Dropbox/data/dd/out/heidelberg"
}
## =========================================================
## --- Global functions ------------------------------------
# paneldata.lags(A, "person", "year", c("v1","v2"), lags=1:4)
# Given data A with key variables "person" and "year" create 
# take variables v1 and create lagged versions 1:4 
# Source: 
# Ajay Narottam Shah (14 Aug 2005): [R] Panel data handling
# (lags, growth rates), 
# http://tolstoy.newcastle.edu.au/R/help/05/08/10380.html,
# last access: 08-21-2012
paneldata.lags <- function(X, unitvar, timevar, lagvars, 
  lags=1) {   
  stopifnot(length(lagvars)>=1) 
  X <- X[order(X[,timevar]),]     # in case it's not sorted.
  innertask <- function(Y, lagvars, lags) {
    E <- labels <- NULL 
    for (v in lagvars) {
      for (i in lags) {
        E <- cbind(E, Lag(Y[,v], i))
      }
    labels <- c(labels, paste(v, ".l", lags, sep=""))
    } 
    colnames(E) <- labels 
    cbind(Y, E) 
  }
do.call("rbind", by(X, X[,unitvar], innertask, lagvars, 
  lags)) 
}
# REQUIRES package::Hmisc
## =========================================================

## -- Load data --------------------------------------------
library(foreign)
dd <- read.dta(
  file.path(pathData, "base", "ddv1_qog_cnts_archigos.dta"),
  convert.underscore = TRUE, convert.factor = TRUE,
  convert.dates = TRUE
)
detach(package:foreign)

## --- Prepare and merge GWF Regime codings ----------------
source(
  file.path(pathCode, "buildData", "GWFmerge.R"), echo = TRUE
)

## --- Prepare and merge Penn World Table ------------------
source(
  file.path(pathCode, "buildData", "PWTmerge.R"), echo = TRUE
)

## --- Define Variables ------------------------------------
source(
  file.path(pathCode, "buildData", "VarDef.R"), echo = TRUE
)
save.image(file = file.path(pathOut, 'base.RData'))

## --- Model development -----------------------------------
source(
  file.path(pathCode, 'buildModel', '01_defineEconomicShockVariables.R'),
  echo = TRUE
)
# source(
#   file.path(pathCode, 'buildModel', '02_ModelExploration.R'),
#   echo = TRUE
# )
## Documentation gap: A series of models uses a variable 
## that frankly doesn't exist. Since these models have the 
## same structure as those presented in the paper I assume
## that I simply changed the variable name at some point.

## --- Model deployment ------------------------------------
source(
  file.path(pathCode, 'buildModel', '03_ModelDeployment.R'),
  echo = TRUE
)
## END
