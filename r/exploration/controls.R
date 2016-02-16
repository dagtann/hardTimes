## =========================================================
## === Heidelberg: Control variables =======================
# ----------------------------------------------------------
# --- a) regional controls ---------------------------------
dd.anal <- within(dd.anal, {
  fsu <- ifelse(  # former soviet union (including mongolia)
    ht.region == "1. Eastern Europe and post Soviet Union",
    1, 0)
  fsu <- factor(fsu, levels = c(0, 1), 
    labels = c("NO", "YES"))
  mena <- ifelse(                              # MENA region
    ht.region == "3. North Africa & the Middle East", 1, 0)
  mena <- factor(mena, levels = c(0, 1), 
    labels = c("NO", "YES"))}
)
apply(dd.anal[, c("fsu", "mena")], 2, table)
# full sample
with(dd.anal, {table(type, fsu)})         # captures PAR/PER
with(dd.anal, {table(type, mena)})   # all types, mostly MON
# 1981 sample
dd.1981 <- subset(dd.anal, year >= 1981)
with(dd.1981, {table(type, fsu, exclude = NULL)}) #  PAR/PER
with(dd.1981, {table(type, mena, exclude = NULL)})     # all
# ----------------------------------------------------------
# --- b) leader turnover -----------------------------------
summary(dd.anal$archigos.eheadn)
with(dd.anal, {table(type, archigos.eheadn)})
with(
  dd.1981, {table(type, archigos.eheadn, 
  exclude = NULL)}
)
#           archigos.eheadn
# type         1   2   3   4   5 <NA>
#   PARTY    813  77   7   1   0  100
#   MILITARY 182  30   9   5   0   16
#   MONARCHY 170  11   0   0   0   30
#   PERSONAL 488  26   6   1   1   88
#   <NA>       0   0   0   0   0    0
library(psych)
with(dd.anal, {
  biserial(eheads, reg.fail)}
)
table(dd.anal$eheads, dd.anal$reg.fail)
tmp <- subset(dd.anal, year>=1981)
with(tmp, {
  biserial(eheads, as.factor(reg.fail))}
)
with(tmp, { table(eheads, reg.fail, exclude = NULL)})
with(tmp, { cor(eheads, reg.fail)})
# ----------------------------------------------------------
# --- c) gdp per capita  -----------------------------------
# see economy.R
# ----------------------------------------------------------
# --- d) fuel exports  -------------------------------------
summary(dd.anal$wdi.fe)
nrow(dd.anal)
summary(dd.1981$wdi.fe)
nrow(dd.1981) # Variable has 50 percent missings!
with(dd.1981, {plot(year, wdi.fe)})
mis <- is.na(dd.1981$wdi.fe)
table(dd.1981$cowcode, mis)
table(dd.1981$ht.region, mis)
cor(dd.1981$year, mis)
cor(dd.1981$cowcode, mis)
# Missings unsystematically effect all regions, years and
# countries. Is control by region sufficient?
# ----------------------------------------------------------
# ---- table for paper -------------------------------------
apply(dd.1981[, c("mena", "fsu")], 2, summary)