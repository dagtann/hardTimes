## =========================================================
## === Heidelberg: Cooptation Measures =====================
## --- Lparty2/Lparty (Gandhi 2008) ------------------------
##  --- 1) Sample statistics -------------------------------
dd.anal <- within(dd.anal, {            # reclass to numeric
  lparty2.num <- as.numeric(lparty2)}
)
# 1 ... no leg
# 2 ... leg, no party
# 3 ... leg, regime party only
# 4 ... leg, multiple parties
attach(dd.anal)
mytable <- table(type, lparty2)
plot(mytable, las = 1)
by(lparty2.num, 
  INDICES = list(Regimetype = type),
  FUN = summary
)
# Regimetype: PARTY
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   3.000   3.000   3.279   4.000   4.000      21 
# ------------------------------------------------------------ 
# Regimetype: MILITARY
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   1.000   2.000   2.318   4.000   4.000      17 
# ------------------------------------------------------------ 
# Regimetype: MONARCHY pwt7.grgdpch.l3  

#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   2.000   2.000   2.146   2.000   4.000      17 
# ------------------------------------------------------------ 
# Regimetype: PERSONAL
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   2.000   3.000   2.899   4.000   4.000      20
boxplot(lparty2.num~type, horizontal = TRUE)
mytable <- table(dd.anal$lparty2, dd.anal$type)
mytable
barplot(prop.table(mytable, 2), col=1:4)
legend(x = "top", legend = c(1:4), fil = 1:4, horiz = TRUE)
# Subtle differences in spread and central tendency
# All types cover entrie scale, PER/PAR center on 3, 
# MIL/MON center on 2, MIL/MON right skewed, 
# PAR/PER left skewed, Modal categories: PAR 3; MIL 1; 
# MON 2; PER 4.
# --- Temporal dynamics: change in institutional setup -----
# change = range in lparty2 by regime in country
detach(dd.anal)
myrange <- function(x) {             # set up range function
  min <- min(x)
  max <- max(x)
  range <- max - min
}
attach(dd.anal)
tmp <- aggregate(lparty2.num,        # range by ctry & spell
  by = list(cowcode = cowcode, spellno = spellno,
    type = type),
  FUN = myrange
)
detach(dd.anal)
mytable <- table(tmp$type,tmp$x)
addmargins(prop.table(mytable, 1)) # tbl range
barplot(prop.table(mytable, 2), 
  legend = rownames(mytable),
  args.legend = list(x = "top", horiz = TRUE, xpd = TRUE,
    inset = -0.15))
#               0     1     2     3   Sum
#  PARTY    0.300 0.250 0.188 0.263 1.000
#  MILITARY 0.351 0.014 0.108 0.527 1.000
#  MONARCHY 0.111 0.333 0.056 0.500 1.000
#  PERSONAL 0.404 0.096 0.149 0.351 1.000
# Reading example: 26.3 percent of dominant party regimes
# moved 3 points on the lparty2 scale during their entire
# existence.
plot(table(tmp$type,tmp$x))
# All regime types experienced dramatic changes in 
# institutional, tendency: MIL/PER change over entire range 
# of scale or no change at all, MON changes of 1 or 3
# points most important; PAR: all changes in scale almost
# equally likely
# --- Instiutional setup - failure vs non-failure ----------
tmp <- dd.anal[dd.anal$reg.fail == 1, ]
attach(tmp)
by(lparty2.num,
   INDICES = list(type = type),
   FUN = summary
)
detach(tmp)
table(tmp$lparty2, tmp$type)
# Regimes that failed cover the whole scale,
# most often regimes have either no leg or leg with multiple
# parties, no imminent ranking of regime types possible,
# unintuitive codings become more relevant -- 26 party (50%)
# failures have no legislature at all which is highly 
# implausible following Geddes' regime type coding
# --- Polit10 - Cabinet size -------------------------------
# Whole sample
attach(dd.anal)
boxplot(cnts.polit10~year+type)
# Universal trend: cabinets become larger over time
tmp <- aggregate(        # compute variance by year and type
  cnts.polit10,
  by = list(year = year, type = type),
  FUN = var,
  na.rm = TRUE
)
tmp <- na.omit(tmp)
detach(dd.anal); attach(tmp)
par(mfrow = c(1,4))
for (i in levels(type)) {
  plot(year[type == i], x[type == i], main = i)
  lines(lowess(year[type == i], x[type==i]), col = 2)
}
# variance drastically changes over time: decreasing in PAR
# with spike in 1980s, increasing in MIL, MON/PER first 
# decreasing (1970s), than strictly increasing
# --> decreasing heterogeneity in PAR, increasing 
# heterogeneity in MIL/PER/MON
detach(tmp); rm(tmp)
# --- Autocorrelation in cabinet data ----------------------
library(Hmisc)
dd.anal <- paneldata.lags(dd.anal,
  "cowcode", "year", "cnts.polit10", lags = 1:10
)
detach(package:Hmisc)
cor(
  dd.anal[, c("cnts.polit10", 
  paste("cnts.polit10.l", seq(1,10,1), sep = ""))],
  method = "pearson",
  use = "pairwise.complete.obs"
)
# CNTS shows strong but visibly decreasing autocorrelation.
# r almost linearly decreases from 0.91 when lag == 1 to
# 0.47 when lag == 10. Control for time dependency is
# absolutely necessary
# --- Violin plots for regime types ------------------------
attach(dd.anal)                             # overall sample
x1.1 <- log(cnts.polit10+1)[type == "PARTY"   ]
x2.1 <- log(cnts.polit10+1)[type == "MONARCHY"]
x3.1 <- log(cnts.polit10+1)[type == "MILITARY"]
x4.1 <- log(cnts.polit10+1)[type == "PERSONAL"]
vioplot(na.omit(x1.1), na.omit(x2.1), na.omit(x3.1), 
  na.omit(x4.1), horizontal = TRUE)
dd.1981 <- dd.anal[year >= 1981, ]     # year >= 1981 sample
detach(dd.anal); 
attach(dd.1981)
x1.1 <- cnts.polit10[type == "PARTY"   ]
x2.1 <- cnts.polit10[type == "MONARCHY"]
x3.1 <- cnts.polit10[type == "MILITARY"]
x4.1 <- cnts.polit10[type == "PERSONAL"]
by(cnts.polit10,
  INDICES = list(type = type),
  FUN = summary)
detach(dd.1981)
# type: PARTY
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00   21.00   27.00   28.04   31.00  109.00      50 
# ---------------------------------------------------------- 
# type: MILITARY
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   11.00   18.00   21.00   22.86   26.00   47.00       9 
# ---------------------------------------------------------- 
# type: MONARCHY
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00   19.00   23.00   23.13   28.00   38.00      14 
# ---------------------------------------------------------- 
# type: PERSONAL
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    0.00   19.00   23.00   24.21   29.00   42.00      44 
# # --- Plot distribution since 1981 -----------------------
# xlabels <- c(1, 5, seq(10,120,10))     # Testing log scale
# xticks <- log(xlabels)                 # and labels
dev.new()
png("./projects/heidelberg/CoopVio.png")         # Save plot
par(mar = c(5, 5, 1, 2) + 0.1, bty = "n")     # cut margins, 
                                                # toggle box

plot(                                    # create empty plot
  x = c(0, 120), y = c(0.5, 4.5),         
  type = "n", xlab = "", ylab = "", 
  xaxt = "n", yaxt = "n")
abline(v = seq(0,120,20), col = "lightgray",           # setup grid
  lty = "dotted") 
abline(h = c(1:4), col = "lightgray", lty = "dotted")
vioplot(              # give violinplot for each regime type
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
axis(side = 1, at = seq(0,120,20), las = 1)   # x-axis ticks
mtext("Number of Ministers in Cabinet",        # add x-label
  side=1, line=3, cex.lab=1, las=1)
dev.off()
# ----------------------------------------------------------
# --- Clean up ---------------------------------------------
rm(list = c("x1.1", "x2.1", "x3.1", "x4.1"))
par(mar = c(5, 4, 4, 2) + 0.1, mfrow = c(1,1), bty = "o")