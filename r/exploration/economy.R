## =========================================================
## === Heidelberg: Economic performance ====================
## Compute time lags
library(Hmisc)
dd.anal <- paneldata.lags(dd.anal, "cowcode", "year",
  c("pwt7.grgdpch", "pwt7.rgdpch", "pwt7.grpos", 
  "pwt7.grneg"), lags= 1:10)
detach(package:Hmisc)
# ----------------------------------------------------------
# --- Sample distribution ----------------------------------
# --- Growth in GDP per Capita -----------------------------
summary(dd.anal$pwt7.grgdpch)           # Full sample Growth
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -50.950  -1.369   2.170   2.004   5.613  84.200     935
dd.1981 <- subset(dd, year >= 1981)
summary(dd.1981$pwt7.grgdpch)       # Reduced sample Growth
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -50.950  -1.796   1.812   1.498   5.162  84.200     150 
plot(density(na.omit(dd.anal$pwt7.grgdpch)))
lines(density(na.omit(dd.1981$pwt7.grgdpch)), col = 2)
# sample distribution 1981 - 2008 basically identical to
# full sample
# --- GDP per Capita ---------------------------------------
