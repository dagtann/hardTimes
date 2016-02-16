## =========================================================
## === Heidelberg: Repression figures ======================
# --- Repression -------------------------------------------
summary(dd.anal[, c("ciri.pf", "ciri.physint2")])
# The full scale is covered, thus some regimes exert 0 
# repression, ciri.pf has median 6, physint2 has median 4
# in general autocracies seem to rely more on pf 
# restrictions
#   ciri.pf      ciri.physint2  
# Min.   :0.000   Min.   :0.000  
# 1st Qu.:4.000   1st Qu.:3.000  
# Median :6.000   Median :4.000  
# Mean   :5.763   Mean   :4.141  
# 3rd Qu.:7.000   3rd Qu.:6.000  
# Max.   :8.000   Max.   :8.000  
# NA's   :2406    NA's   :2426
attach(dd.anal)   
by(
  dd.anal[, c("ciri.pf", "ciri.physint2")],
  INDICES = type,
  FUN = summary
)
# Regime type ranking: 
# PF: PAR 6; MIL 6; PER 6; MON 7 -- PAR/PER have min 0
# Physint2: MON 3; PER 4; PAR 4; MIL 5; -- all have min 0
# Unlikely that there is coding error
tmp <- aggregate(
  dd.anal[, c("ciri.pf", "ciri.physint2")],
  by = list(type = type, year = year),
  FUN = median,
  na.rm = TRUE
)
detach(dd.anal)
scatterplot(jitter(ciri.pf, 2) ~ year | type, 
  smooth = TRUE, reg.line = FALSE, data= tmp)
scatterplot(jitter(ciri.physint2, 2) ~ year | type, 
  smooth = TRUE, reg.line = FALSE, data= tmp)
attach(dd.anal)
par(mfrow = c(1, 4))
boxplot(ciri.pf[type == "PARTY"] ~
  year[type == "PARTY"], 
  horizontal = TRUE, main = "PARTY")
boxplot(ciri.pf[type == "PERSONAL"] ~
  year[type == "PERSONAL"], 
  horizontal = TRUE, main = "PERSONAL")
boxplot(ciri.pf[type == "MILITARY"] ~
  year[type == "MILITARY"], 
  horizontal = TRUE, main = "MILITARY")
boxplot(ciri.pf[type == "MONARCHY"] ~
  year[type == "MONARCHY"], 
  horizontal = TRUE, main = "MONARCHY")
# Problem: Physint2 decreasing sample median over all regime
# types from 1980 to 1995 and increasing ever since.
# median pf is almost linearly increasing for all regime 
# types. At the same time sample variance in both measures
# seems to be decreasing over time.
# --- Repression level conditional on failure --------------
tmp <- aggregate(                    # Difference in general
  dd.anal[, c("ciri.pf", "ciri.physint2")],
  by = list(type = type, fail = reg.fail),
  FUN = median,
  na.rm = TRUE
)
#     type fail ciri.pf ciri.physint2
# -----------------------------------
#    PARTY    0     6.0           4.0 # No Fail
# MILITARY    0     6.0           5.0
# MONARCHY    0     7.0           3.0
# PERSONAL    0     6.0           4.0
# -----------------------------------
#    PARTY    1     4.0           4.0 # On Fail
# MILITARY    1     5.0           5.0
# MONARCHY    1     4.5           6.5
# PERSONAL    1     4.0           4.0
tmp <- aggregate(
  dd.anal[, c("ciri.pf", "ciri.physint2")],
  by = list(type = type, year = year, 
    fail = reg.fail),
  FUN = median,
  na.rm = TRUE
)
detach(dd.anal); attach(tmp)

tmp <- tmp[tmp$fail == 1, ]
scatterplot(jitter(ciri.pf, 1) ~ year | type, 
  smooth = TRUE, reg.line = FALSE, data= tmp)
scatterplot(jitter(ciri.physint2, 1) ~ year | type, 
  smooth = TRUE, reg.line = FALSE, data= tmp)
detach(tmp); attach(dd.anal)
tmp <- dd.anal[reg.fail == 1, ]
detach(dd.anal); attach(tmp)
by(
  tmp[, c("ciri.physint2", "ciri.pf")],
  INDICES = list(type = type),
  FUN = summary
)
detach(tmp)
# Regime medians on failure:
# Physint2 PAR 4 ; MON 6.5; MIL 5; PER 4 
# PF PAR 4 ; MON 4.5; MIL 5; PER 4
# On failure regime types cover full range of both 
# indicators with the exception of Monarchies (PF 3 - 6; 
# Physint 5 - 8)
# --- Time dependency in repression ------------------------
library(Hmisc)
dd.anal <- paneldata.lags(dd.anal, "cowcode", "year",
  c("ciri.pf", "ciri.physint2"), lags = 1:10)
detach(package:Hmisc)
library(psych)
cor(
  dd.anal[, c("ciri.pf", 
  paste("ciri.pf.l", seq(1, 10), sep = ""))],
  method = "pearson",
  use = "pairwise.complete.obs"
) 
cor(
  dd.anal[, c("ciri.physint2", 
  paste("ciri.physint2.l", seq(1, 10), sep = ""))],
  method = "pearson",
  use = "pairwise.complete.obs"
)
biserial(dd.anal[, c("ciri.pf", 
  paste("ciri.pf.l", seq(1, 10), sep = ""), 
  "ciri.physint2", 
  paste("ciri.physint2.l", seq(1, 10), sep = ""))], 
  dd.anal$reg.fail)
dd.1981 <- subset(dd.anal, year >= 1981)
biserial(dd.1981[, c("ciri.pf", # 1981 subsample correlation 
  paste("ciri.pf.l", seq(1, 10), sep = ""), 
  "ciri.physint2", 
  paste("ciri.physint2.l", seq(1, 10), sep = ""))], 
  dd.1981$reg.fail)
detach(package:psych)
# Correlation in year of breakdown strongest, 
# .pf more strongly correlated with breakdown. 
# ==========================================================
# --- vioplots for paper -----------------------------------
attach(dd.1981)
x1.1 <- ciri.pf[type == "PARTY"   ]
x2.1 <- ciri.pf[type == "MONARCHY"]
x3.1 <- ciri.pf[type == "MILITARY"]
x4.1 <- ciri.pf[type == "PERSONAL"]
x1.2 <- ciri.physint2[type == "PARTY"   ]
x2.2 <- ciri.physint2[type == "MONARCHY"]
x3.2 <- ciri.physint2[type == "MILITARY"]
x4.2 <- ciri.physint2[type == "PERSONAL"]
detach(dd.1981)

dev.new()
png("./projects/heidelberg/RepressionVio.png")
par(mfrow = c(1, 2), bty="n", mar = c(5, 5, 1, 0) + 0.1)
plot(
  x = c(0, 8), y = c(0.5, 4.5), xaxt = "n", yaxt = "n",
  type = "n", xlab = "", ylab = "", 
  sub = "Political Freedoms"
)
abline(                                         # setup grid
  v = seq(0, 8, 1), 
  col = "lightgray",     
  lty = "dotted"
) 
abline(h = c(1:4), col = "lightgray", lty = "dotted")
vioplot(
  na.omit(x1.1), na.omit(x2.1), na.omit(x3.1), 
  na.omit(x4.1), 
  horizontal = TRUE, 
  col = "grey80", 
  add = TRUE
)
axis(                                    # y-axis categories
  side = 2, at=c(1:4), 
  labels = c("Party", "Monarchy",
  "Military", "Personal"), las = 1
)
axis(side = 1, at = seq(0,8,1), las = 1)
par(mar = c(5, 3, 1, 2) + 0.1)
plot(
  x = c(0, 8), y = c(0.5, 4.5), xaxt = "n", yaxt = "n",
  type = "n", xlab = "", ylab = "", 
  sub = "Physical Integrity"
)
abline(                                         # setup grid
  v = seq(0, 8, 1), 
  col = "lightgray",     
  lty = "dotted"
) 
abline(h = c(1:4), col = "lightgray", lty = "dotted")
vioplot(
  na.omit(x1.2), na.omit(x2.2), na.omit(x3.2), 
  na.omit(x4.2), 
  horizontal = TRUE, 
  col = "grey80", 
  add = TRUE
)
axis(side = 1, at = seq(0,8,1), las = 1)
dev.off()
## =========================================================
## === Purges ==============================================
with(dd.anal, {summary(cnts.domestic5)})
plot(density(dd.anal$cnts.domestic5, na.rm = TRUE))
symbox(dd.anal$cnts.domestic5+1)
with(dd.anal, {boxplot(cnts.domestic5~type)})
with(dd.anal, {
  by(cnts.domestic5,
    INDICES = list(type = type),
    summary)}
)
summary(aov(dd.anal$cnts.domestic5 ~ dd.anal$type))
dd.1981 <- subset(dd.anal, year >= 1981)
with(dd.1981, {summary(aov(cnts.domestic5~type))})
with(dd.1981, plot(density(cnts.domestic5, na.rm = TRUE)) 
summary(dd.1981$cnts.domestic5)