## =========================================================
## === PrepAnal ============================================
## 1) Lag all variables by one year
setwd("C:/Users/tanneberg/Dropbox/data/dd")
rm(list=ls())
load(file = "./projects/heidelberg/base.RData")
library(Hmisc)
dd.anal <- paneldata.lags(dd.anal, "cowcode", "year", 
  c("type",                                        # Regtype
  "ciri.physint2", "ciri.pf", "fh.cl", "fh.pr", # repression
  "lparty2", "lparty",                        # institutions
  "pwt7.rgdpch", "pwt7.grgdpch",                   # economy
  "cnts.polit10",                              # co-optation
  "wdi.fe",
  "eelections", "lelections", "coldwar", "pwt7.kg",
  "pwt7.grrgdpl.cor", "pwt7.rgdpl.cor", "gd.ptss"),
  lags=1:3
)
detach(package:Hmisc)
with(dd.anal, levels(lparty2))
with(dd.anal, table(lparty2.l1))
dd.anal <- within(dd.anal, {
    lparty2.l1 <- factor(lparty2.l1,
      levels = c(1, 2, 3),
      labels = c("L1Pa", "NoL", "LmPa")
    )
    lparty2.l1 <- relevel(lparty2.l1, ref = "L1Pa")
    lparty2.diff <- ifelse(
      as.numeric(lparty2) != as.numeric(lparty2.l1), 1, 0)
  }
)
with(dd.anal, table(reg.fail, lparty2.diff))
with(dd.anal, table(type))
dd.anal <- within(dd.anal, {
    type.l1 <- factor(type.l1,
      levels = c(1, 2, 3, 4),
      labels = c("PARTY", "MILITARY", "MONARCHY", "PERSONAL")
    )
    type.l1 <- relevel(type.l1, ref = "PARTY")
  }
)
with(dd.anal, table(type.l1))
dd.anal <- within(dd.anal, {
    coldwar.l1 <- factor(coldwar.l1,
      levels = c(1, 2),
      labels = c("NO", "YES")
    )
  }
)
# --- Sample statistics ------------------------------------
dd.1981 <- subset(dd.anal, year >= 1981)   # df for analysis
nrow(dd.1981)                                   ## 2061 cyrs
library(plyr)
count(dd.1981[, c("cowcode","spellno")])       # 160 regimes
count(dd.1981[, "cowcode"])                  # 110 countries
detach(package:plyr)
# --- Survival time distribution ---------------------------
test <- with(dd.1981, {
  aggregate(
    cbind(reg.fail, spellrn) ~ cowcode + spellno,
    FUN = max,
    data = dd.1981
    )
  }
)
names(test)
table(test$reg.fail)                ##    102 failure events
mean(test$reg.fail)               ## 63.75 percent of spells
summary(test$spellrn)
                                           ## Survival times
            ## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
            ## 1.00    6.00   11.00   12.88   19.00   28.00 
library(survival)
sfit <- survfit(              
  Surv(time = spellrn,
    event = reg.fail
  )~1,
  data = test                 
)
summary(sfit)                 ## 10 yrs median survival time
plot(sfit)
detach(package:survival)      ## decreasing hazard over time
rm(list=c("sfit", "test"))
## --- Economic shocks -------------------------------------
summary(dd.1981[, c("pwt7.grrgdpl.cor", "pwt7.grrgdpl.cor.l1")])
with(dd.1981, {
  boxplot(pwt7.grrgdpl.cor.l1 ~ cowcode)
  }
)            
with(dd.1981, {
  boxplot(
    by(pwt7.grrgdpl.cor.l1,
    INDICES = cowcode,
    FUN = sd,
    na.rm = TRUE
    )
  )}
)
             ## both visualisation:
             ## tremendous differences in median, spread, sd
             ## -> filter justified
## Filter for economic shocks
## Conditions:
## 1. year of shock: delta gr(i,t) < -2 ppa
## 2. after shock gr(i,t) < 2
## Indicator variable:
## shock(t,t-2) == 1 if shock at t | t-1 | t-2, 
## and 0 otherwise
## --- Filter growth (Petrova/Bates 2011) ------------------
dd.1981 <- within(dd.1981, {
  delta.grgdp <- pwt7.grrgdpl.cor - pwt7.grrgdpl.cor.l1
  tag1 <- ifelse(delta.grgdp < -2.0, 1, 0)
  tag2 <- ifelse(pwt7.grrgdpl.cor < 2.0, 1, 0)
  tag3 <- ifelse(tag1 == 1 & tag2 == 1, 1, 0)
  }
)
mean(dd.1981$delta.grgdp, na.rm = TRUE)
summary(dd.1981[, c("delta.grgdp", "tag1", "tag2", "tag3")])
library(Hmisc)
dd.1981 <- paneldata.lags(
  dd.1981, "cowcode", "year",
  "tag3",
  lags = 1:2
)
detach(package:Hmisc)
dd.1981 <- within(dd.1981, {
    eshock <- ifelse(tag3 == 1 | tag3.l1 == 1 |
      tag3.l2 == 1, 1, 0)
    eshock <- factor(eshock,
      levels = c(0, 1),
      labels = c("NO", "YES")
    )
    tag3.l1 <- factor(tag3.l1,
      levels = c(0, 1),
      labels = c("NO", "YES")
    )
  }
)
table(dd.1981$eshock, exclude = NULL)
table(dd.1981$tag3.l1, exclude = NULL)
with(dd.1981, {
  table(ht.region, eshock, exclude = NULL)
  }
)               ## follows Petrova, Bates regional patterns, 
                                  ## but has way more events
with(dd.1981, table(eheads.fail, eshock))
with(dd.1981, table(eheads.fail, tag3.l1))
## Tables indicate weak association between eheads.fail and
## economic shock indicators
## --- Alternative filter ----------------------------------
## Indicator for low/negative growth at t-1
## Drop grgdpch threshold: a drop from 8 to 5 might be as
## important as a drop from 1 to -2 given the populations
## expectations with regard to the regime
## split information at 0: preliminary threshold,
## demonstrate robustness
dd.1981 <- within(dd.1981, {
    grneg.m2 <- ifelse(pwt7.grrgdpl.cor.l1 < -2, 1, 0)
    grneg.m2 <- factor(grneg.m2,
      levels = c(0, 1),
      labels = c("NO", "YES")
    )
    grneg.m15 <- ifelse(pwt7.grrgdpl.cor.l1 < -1.5, 1, 0)
    grneg.m15 <- factor(grneg.m15,
      levels = c(0, 1),
      labels = c("NO", "YES")
    )
    grneg.m1 <- ifelse(pwt7.grrgdpl.cor.l1 < -1, 1, 0)
    grneg.m1 <- factor(grneg.m1,
      levels = c(0, 1),
      labels = c("NO", "YES")
    )
    grneg.m05 <- ifelse(pwt7.grrgdpl.cor.l1 < -0.5, 1, 0)
    grneg.m05 <- factor(grneg.m05,
      levels = c(0, 1),
      labels = c("NO", "YES")
    )
    grneg.0 <- ifelse(pwt7.grrgdpl.cor.l1 < 0, 1, 0)
    grneg.0 <- factor(grneg.0,
      levels = c(0, 1),
      labels = c("NO", "YES")
    )
    grneg.05 <- ifelse(pwt7.grrgdpl.cor.l1 < .5, 1, 0)
    grneg.05 <- factor(grneg.05,
      levels = c(0, 1),
      labels = c("NO", "YES")
    )
    grneg.1 <- ifelse(pwt7.grrgdpl.cor.l1 < 1, 1, 0)
    grneg.1 <- factor(grneg.1,
      levels = c(0, 1),
      labels = c("NO", "YES")
    )
    grneg.15 <- ifelse(pwt7.grrgdpl.cor.l1 < 1.5, 1, 0)
    grneg.15 <- factor(grneg.15,
      levels = c(0, 1),
      labels = c("NO", "YES")
    )
    grneg.2 <- ifelse(pwt7.grrgdpl.cor.l1 < 2, 1, 0)
    grneg.2 <- factor(grneg.2,
      levels = c(0, 1),
      labels = c("NO", "YES")
    )
  }
)
apply(dd.1981[, grep(patter = "grneg.", names(dd.1981))], 2,
  table)
## Growth in gdp correcting for delta in gov spending
dd.1981 <- within(dd.1981,
  delta.kg <- pwt7.kg - pwt7.kg.l1
)
summary(dd.1981$delta.kg)
with(dd.1981, 
  cor(delta.grgdp, delta.kg, 
    use = "complete.obs"
  )
)
with(dd.1981, plot(delta.grgdp, delta.kg))
# the smaller the growth rate, the larger the government
# spending
## --- Accumulated growth experience indicators ------------
dd.1981 <- within(dd.1981, {
  pos.dum <- ifelse(pwt7.grgdpch > 0, 1, 0)
  pos.dum <- ifelse(is.na(pos.dum) == TRUE, 0, pos.dum)
  neg.dum <- ifelse(pwt7.grgdpch < 0, 1, 0)
  neg.dum <- ifelse(is.na(neg.dum) == TRUE, 0, neg.dum)
  }
)                              ## dummies for pos/neg growth
summary(dd.1981[, c("pwt7.grgdpch", "pos.dum", "neg.dum")])
library(Hmisc)            ## define spells of pos/neg growth
dd.1981 <- paneldata.lags(dd.1981, "cowcode", "year",
  c("pos.dum", "neg.dum"), lags = 1)
detach(package:Hmisc)
dd.1981 <- within(dd.1981, {
  pos.dum.l1 <- ifelse(pos.dum == 1 & 
    is.na(pos.dum.l1) == TRUE, 1, pos.dum.l1)
  pos.dum.l1 <- ifelse(pos.dum == 0 & 
    is.na(pos.dum.l1) == TRUE, 0, pos.dum.l1)
  neg.dum.l1 <- ifelse(neg.dum == 1 & 
    is.na(neg.dum.l1) == TRUE, 1, neg.dum.l1)
  neg.dum.l1 <- ifelse(neg.dum == 0 & 
    is.na(neg.dum.l1) == TRUE, 0, neg.dum.l1)
  pos2neg.tag <- ifelse(neg.dum == 1 & pos.dum.l1 == 1, 1, 0)
  neg2pos.tag <- ifelse(pos.dum == 1 & neg.dum.l1 == 1, 1, 0)
  }
)
summary(dd.1981[, c("pos.dum.l1", "neg.dum.l1")])
summary(dd.1981[, c("pos2neg.tag", "neg2pos.tag")])
dd.1981 <- dd.1981[with(dd.1981, {order(cowcode, year)}), ]
library(plyr)
dd.1981 <- ddply(          # count spell within regime spell
  dd.1981,
  .variables = c("cowcode", "spellno"),
  transform,
  pos.spell = cumsum(pos2neg.tag)
)
names(dd.1981)
dd.1981 <- ddply(   # count consecutive obs within pos.spell
  dd.1981,
  .variables = c("cowcode", "spellno", "pos.spell"),
  transform,
  pos.run = cumsum(pos.dum)
)
dd.1981 <- ddply(          # count spell within regime spell
  dd.1981,
  .variables = c("cowcode", "spellno"),
  transform,            # no f*cking clue why, but necessary
  neg.spell = cumsum(neg2pos.tag)
)
dd.1981 <- ddply(   # count consecutive obs within pos.spell
  dd.1981,
  .variables = c("cowcode", "spellno", "neg.spell"),
  transform,            # no f*cking clue why, but necessary
  neg.run = cumsum(neg.dum)
)
summary(dd.1981[, c("pos.spell", "pos.run", "neg.spell", 
  "neg.run")])
detach(package:plyr)
exclude <- names(dd.1981) %in% c("pos.dum.l1", "pos.dum",
  "pos.spell", "pos2neg.tag", "neg.dum", "neg.dum.l1", "
  neg.spell", "neg2pos.tag")
dd.1981 <- dd.1981[!exclude]
rm(exclude)
summary(dd.1981[, c("pos.run", "neg.run")])
boxplot(dd.1981[, c("pos.run", "neg.run")])
dd.1981 <- dd.1981[with(dd.1981, order(cowcode, year)),]
## View(dd.1981[, c("ctryname", "year", "pwt7.grrgdpl.cor",
##  "pos.run", "neg.run")])
## pos.run and neg.run give the number of consecutive years 
## with negative or positive growth
## --- compute weighted growth -----------------------------
dd.1981 <- within(dd.1981, {
  pwt7.grgdpch.wght <- ifelse(
    pwt7.grgdpch < 0, pwt7.grgdpch*neg.run,
        ifelse(
          pwt7.grgdpch > 0, pwt7.grgdpch*pos.run, 
            pwt7.grgdpch
        )
    )
  }
)
summary(dd.1981$pwt7.grgdpch.wght)
## lag weighted growth
library(Hmisc)
dd.1981 <- paneldata.lags(dd.1981, "cowcode", "year",
  "pwt7.grgdpch.wght", lags = 1)
detach(package:Hmisc)
View(dd.1981[, c("ctryname", "year", "pwt7.grgdpch.wght",
  "pos.run", "neg.run", "pwt7.grgdpch", 
  "pwt7.grgdpch.wght.l1")])
## =========================================================
## === Model development ===================================
library(car)
row.names(dd.1981) <- paste(dd.1981$cowcode, dd.1981$year, 
  sep = ":")
## Baseline model: economic downturn & controls
fit1 <- glm(reg.fail ~ pwt7.grrgdpl.cor.l1 +
  spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("reg.fail", "pwt7.grrgdpl.cor.l1", 
      "spellrn")
    ]
  )
)
summary(fit1)
residualPlots(fit1)
marginalModelPlots(fit1)      # heavy outliers in grgdpch.l1
influenceIndexPlot(fit1, vars = c("Cook", "hat"), id.n = 3)
quantile(dd.1981$pwt7.grrgdpl.cor.l1, 
  probs = c(0.025, 0.975), 
  na.rm = TRUE
) # -15.22767 : 16.12527
fit1a <- update(
  fit1, . ~ .,
  data = na.omit(
    dd.1981[dd.1981$pwt7.grrgdpl.cor.l1 >= -15.22767 &
      dd.1981$pwt7.grrgdpl.cor.l1 <= 16.12527, 
      c("reg.fail", "pwt7.grrgdpl.cor.l1", "spellrn")
    ]
  )
)
summary(fit1a)
residualPlots(fit1a)
marginalModelPlots(fit1a)
compareCoefs(fit1, fit1a)
fit1b <- glm(
  reg.fail ~
  pwt7.grrgdpl.cor.l1 + 
  type.l1 + log(pwt7.rgdpl.cor.l1) + mena + fsu + coldwar.l1 +
  spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("reg.fail", "pwt7.grrgdpl.cor.l1", 
      "pwt7.rgdpl.cor.l1", "mena", "fsu", "coldwar.l1", 
      "type.l1", "spellrn")]
  )
)
summary(fit1b)
fit1c <- update(fit1b, . ~ .,
  data = na.omit(
    dd.1981[dd.1981$pwt7.grrgdpl.cor.l1 >= -15.22767 &
      dd.1981$pwt7.grrgdpl.cor.l1 <= 16.12527, 
      c("reg.fail", "pwt7.grrgdpl.cor.l1", 
      "pwt7.rgdpl.cor.l1", "mena", "fsu", "coldwar.l1", 
      "type.l1", "spellrn")
    ]
  )
)
summary(fit1c)
residualPlots(fit1c)
compareCoefs(fit1b, fit1c)
fit1d <- glm(
  reg.fail ~
  pwt7.grrgdpl.cor.l1 + 
  ciri.physint2.l1 + ciri.pf.l1 + 
  lparty2.l1 + type.l1 +
  log(pwt7.rgdpl.cor.l1) + mena + fsu + coldwar.l1 +
  spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[dd.1981$pwt7.grrgdpl.cor.l1 >= -15.22767 &
      dd.1981$pwt7.grrgdpl.cor.l1 <= 16.12527, 
      c("reg.fail", "pwt7.grrgdpl.cor.l1", 
      "pwt7.rgdpl.cor.l1", "mena", "fsu", "coldwar.l1", 
      "spellrn", "ciri.physint2.l1", 
      "ciri.pf.l1", "type.l1",
      "lparty2.l1")]
  )
)
summary(fit1d)
residualPlots(fit1d)
marginalModelPlots(fit1d)
compareCoefs(fit1c, fit1d)
quantile(dd.1981$cnts.polit10.l1, probs = c(0.025, 0.975), na.rm = TRUE)
## Truncating the distribution alters time dependency and
## increases the effect of grgdpch, but
## grgdpch stays insignificant
## --- Qualitative indicators ------------------------------
## Eshock filter
fit2 <- glm(
  eheads.fail ~ eshock +
    spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("eheads.fail", "eshock", "spellrn")]
  )
)
summary(fit2)
## Petrova/Bates Filter not significant -- as shown in paper
fit3 <- glm(
  eheads.fail ~ tag3.l1 +
    spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("eheads.fail", "tag3.l1", "spellrn")]
  )
)
summary(fit3)
## Petrova/Bates Filter at t-1 not significant
## negative/slow growth indicators
fit4 <- glm(
  eheads.fail ~ grneg.m2 +
    spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("eheads.fail", "grneg.m2", "spellrn")]
  )
) 
summary(fit4) # not significant
fit4a <- glm(
  eheads.fail ~ grneg.m15 +
    spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("eheads.fail", "grneg.m15", "spellrn")]
  )
)
summary(fit4a) # not significant
fit4b <- glm(
  eheads.fail ~ grneg.m1 +
    spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("eheads.fail", "grneg.m1", "spellrn")]
  )
)
summary(fit4b) # not significant
fit4c <- glm(
  eheads.fail ~ grneg.m05 +
    spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("eheads.fail", "grneg.m05", "spellrn")]
  )
)
summary(fit4c) # not significant
fit4d <- glm(
  eheads.fail ~ grneg.0 +
    spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("eheads.fail", "grneg.0", "spellrn")]
  )
)
summary(fit4d)
## grneg.0 at p > 0.04983
fit4e <- glm(
  eheads.fail ~ grneg.05 +
    spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("eheads.fail", "grneg.05", "spellrn")]
  )
)
summary(fit4e)
## grneg.05 at p > 0.083
fit4c <- glm(
  eheads.fail ~ grneg.1 +
    spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("eheads.fail", "grneg.1", "spellrn")]
  )
)
summary(fit4c)
## grneg.1 at p > 0.049
fit4d <- glm(
  eheads.fail ~ grneg.15 +
    spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("eheads.fail", "grneg.15", "spellrn")]
  )
)
summary(fit4d)
## grneg.15 at p > 0.05
fit4e <- glm(
  eheads.fail ~ grneg.2 +
    spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("eheads.fail", "grneg.2", "spellrn")]
  )
)
summary(fit4e)
## grneg.2 insignificant
## --- sustained growth experience -------------------------
## weighted growth
fit5 <- glm(reg.fail ~ pwt7.grrgdpl.wght.l1 +
  spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("reg.fail", "pwt7.grrgdpl.wght.l1", 
      "spellrn")
    ]
  )
)
summary(fit5)
residualPlots(fit5)
marginalModelPlots(fit5)      # heavy outliers in grgdpch.l1
influenceIndexPlot(fit1, vars = c("Cook", "hat"), id.n = 3)
quantile(dd.1981$pwt7.grrgdpl.wght.l1, 
  probs = c(0.025, 0.975), 
  na.rm = TRUE
) # -31.14919 : 89.53607
fit5a <- update(
  fit5, . ~ .,
  data = na.omit(
    dd.1981[dd.1981$pwt7.grrgdpl.wght.l1 >= -31.14919 &
      dd.1981$pwt7.grrgdpl.wght.l1 <= 89.53607, 
      c("reg.fail", "pwt7.grrgdpl.wght.l1", "spellrn")
    ]
  )
)
summary(fit5a)
residualPlots(fit5a)
marginalModelPlots(fit5a)
compareCoefs(fit5, fit5a)
fit5b <- glm(
  reg.fail ~
  pwt7.grrgdpl.wght.l1 + 
  type.l1 + log(pwt7.rgdpl.cor.l1) + mena + fsu + coldwar.l1 +
  spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[, c("reg.fail", "pwt7.grrgdpl.wght.l1", 
      "pwt7.rgdpl.cor.l1", "mena", "fsu", "coldwar.l1", 
      "type.l1", "spellrn")]
  )
)
summary(fit5b)
fit5c <- update(fit5b, . ~ .,
  data = na.omit(
    dd.1981[dd.1981$pwt7.grrgdpl.wght.l1 >= -31.14919 &
      dd.1981$pwt7.grrgdpl.wght.l1 <= 89.53607, 
      c("reg.fail", "pwt7.grrgdpl.wght.l1", 
      "pwt7.rgdpl.cor.l1", "mena", "fsu", "coldwar.l1", 
      "type.l1", "spellrn")
    ]
  )
)
summary(fit5c)
fit5c <- update(fit5b, . ~ .,
  data = na.omit(
    dd.1981[dd.1981$pwt7.grrgdpl.wght.l1 >= -31.14919 &
      dd.1981$pwt7.grrgdpl.wght.l1 <= 89.53607, 
      c("reg.fail", "pwt7.grrgdpl.wght.l1", 
      "pwt7.rgdpl.cor.l1", "mena", "fsu", "coldwar.l1", 
      "type.l1", "spellrn")
    ]
  )
)

residualPlots(fit5c)
marginalModelPlots(fit5c)
compareCoefs(fit5b, fit5c)
fit5d <- glm(
  reg.fail ~
  I(-1*pwt7.grrgdpl.wght.l1) + 
  ciri.physint2.l1 + ciri.pf.l1 + 
  lparty2.l1 + type.l1 +
  log(pwt7.rgdpl.cor.l1) + mena + fsu + coldwar.l1 +
  spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(dd.1981[,
      c("reg.fail", "pwt7.grrgdpl.wght.l1", 
      "pwt7.rgdpl.cor.l1", "mena", "fsu", "coldwar.l1", 
      "spellrn", "ciri.physint2.l1", 
      "ciri.pf.l1", "type.l1",
      "lparty2.l1")]
  )
)
summary(fit5d)
residualPlots(fit5d)
marginalModelPlots(fit5d)
fit5e <- update(fit5d, . ~ .,
  data = na.omit(
    dd.1981[(dd.1981$pwt7.grrgdpl.wght.l1 >= -31.14919 &
      dd.1981$pwt7.grrgdpl.wght.l1 <= 89.53607) &
      dd.1981$cnts.polit10.l1 > 0, 
      c("reg.fail", "pwt7.grrgdpl.wght.l1", 
      "pwt7.rgdpl.cor.l1", "mena", "fsu", "coldwar.l1", 
      "spellrn", "ciri.physint2.l1", 
      "ciri.pf.l1", "type.l1",
      "lparty2.l1")
    ]
  )
)
summary(fit5e)
compareCoefs(fit5d, fit5e)
step(fit5e, direction = "backward" )
residualPlots(fit5e)
marginalModelPlots(fit5e)
## =========================================================
## === Model presentation ==================================
## drop conceptually unsatisfying controls:
## fsu, mena, coldwar
## use truncated sample (pwt7.grrgdpl.wght.l1)
## --- Summary statistics ----------------------------------
quantile(dd.1981$pwt7.grgdpch.l1, 
  prob = c(0.001, 0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 
    0.99, 0.999), 
  na.rm = TRUE
)
summary(dd.1981$pwt7.grgdpch.wght.l1)
sd(dd.1981$pwt7.grgdpch.wght.l1, na.rm = TRUE)
quantile(dd.1981$pwt7.grgdpch.wght.l1, 
  prob = c(0.001, 0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 
    0.99, 0.999), 
  na.rm = TRUE
)
## --- Baseline --------------------------------------------
fitPres.a <- glm(
  reg.fail ~ 
  I(-1*pwt7.grgdpch.wght.l1) +
  spellrn + I(spellrn^2) + I((spellrn^3)/1000),
  family=binomial(link=logit),
  data = na.omit(
    dd.1981[dd.1981$pwt7.grgdpch.wght.l1 >= -27.72891 &
       dd.1981$pwt7.grgdpch.wght.l1 <=  82.69569,
      c("reg.fail", "pwt7.grgdpch.wght.l1", "spellrn",
        "pwt7.rgdpch.l1", "type.l1",
       "ciri.pf.l1", "ciri.physint2.l1", "lparty2.l1")
    ]
  )
)
summary(fitPres.a)
logLik(fitPres.a)
residualPlots(fitPres.a)
marginalModelPlots(fitPres.a)      # heavy outliers in grgdpch.l1
influenceIndexPlot(fitPres.a, 
  vars = c("Cook", "hat"), 
  id.n = 3
)
exclude <- c("435:2005", "438:2008", "732:1987")
View(dd.1981[rownames(dd.1981) %in% exclude,]) ## all events
## --- Adding basic controls -------------------------------
fitPres.b <- update(fitPres.a,
  . ~ . + log(pwt7.rgdpch.l1) + type.l1,
  data = na.omit(
    dd.1981[dd.1981$pwt7.grgdpch.wght.l1 >= -27.72891 &
       dd.1981$pwt7.grgdpch.wght.l1 <=  82.69569,
      c("reg.fail", "pwt7.grgdpch.wght.l1", "spellrn",
        "pwt7.rgdpch.l1", "type.l1", "pwt7.grgdpch.l1",
       "ciri.pf.l1", "ciri.physint2.l1", "lparty2.l1")
    ]
  )
)
summary(fitPres.b)
logLik(fitPres.b)
residualPlots(fitPres.b)
marginalModelPlots(fitPres.b)
influenceIndexPlot(fitPres.b, 
  vars = c("Cook", "hat"), 
  id.n = 3
)
exclude <- c("435:2005", "790:1991", "790:2006")
View(dd.1981[rownames(dd.1981) %in% exclude,]) ## all events
## --- Adding focal controls -------------------------------
fitPres.c <- update(fitPres.b,
  . ~ . + ciri.physint2.l1 + ciri.pf.l1 + lparty2.l1,
  data = na.omit(
    dd.1981[dd.1981$pwt7.grgdpch.wght.l1 >= -27.72891 &
       dd.1981$pwt7.grgdpch.wght.l1 <=  82.69569,
      c("reg.fail", "pwt7.grgdpch.wght.l1", "spellrn",
        "pwt7.rgdpch.l1", "type.l1", "pwt7.grgdpch.l1",
       "ciri.pf.l1", "ciri.physint2.l1", "lparty2.l1")
    ]
  )
)
summary(fitPres.c)
residualPlots(fitPres.c)
marginalModelPlots(fitPres.c)
crPlots(fitPres.c, "I(-1*pwt7.grgdpch.wght.l1)")
influenceIndexPlot(fitPres.c,
  vars = c("Cook", "hat"),
  id.n = 3
)
exclude <- c("435.2005", "790:1991", "790:2006")
View(dd.1981[rownames(dd.1981) %in% exclude,]) ## all events
logLik(fitPres.c)
fitPres.d <- update(fitPres.b,
  . ~ . + ciri.physint2.l1 + ciri.pf.l1,
  data = na.omit(
    dd.1981[dd.1981$pwt7.grgdpch.wght.l1 >= -27.72891 &
       dd.1981$pwt7.grgdpch.wght.l1 <=  82.69569,
      c("reg.fail", "pwt7.grgdpch.wght.l1", "spellrn",
        "pwt7.rgdpch.l1", "type.l1", "pwt7.grgdpch.l1",
       "ciri.pf.l1", "ciri.physint2.l1", "lparty2.l1")
    ]
  )
)
summary(fitPres.d)





compareCoefs(fitPres.c, 
  update(
    fitPres.c,
    . ~ .,
    data = na.omit(
        dd.1981[,
        c("reg.fail", "pwt7.grgdpch.wght.l1", "spellrn",
        "pwt7.rgdpch.l1", "type.l1", "pwt7.grgdpch.l1",
        "ciri.pf.l1", "ciri.physint2.l1", "lparty2.l1")
        ]
    )
  )
)

### Anova tests for embedded models
anova(fitPres.a,fitPres.b, test = "Chisq")
anova(fitPres.b,fitPres.c, test = "Chisq")
anova(fitPres.a,fitPres.c, test = "Chisq")



## --- Missing data problems -------------------------------
library(mice)
md.pattern( dd.1981[dd.1981$pwt7.grrgdpl.wght.l1 >= -31.14919 &
      dd.1981$pwt7.grrgdpl.wght.l1 <= 89.53607, 
       c("cowcode","year", "reg.fail", "pwt7.grrgdpl.wght.l1", "spellrn",
        "pwt7.rgdpl.cor.l1", "type.l1",
       "ciri.pf.l1", "gd.ptss.l1", "lparty2.l1")
    ]
)
md.pattern( dd.1981[dd.1981$pwt7.grrgdpl.wght.l1 >= -31.14919 &
      dd.1981$pwt7.grrgdpl.wght.l1 <= 89.53607, 
       c("ciri.pf.l1", "gd.ptss.l1")
    ]
)
detach(package:VIM)
library(VIM)
aggr(dd.1981a[
  dd.1981a$pwt7.grrgdpl.wght.l1 >= -31.14919 &
    dd.1981a$pwt7.grrgdpl.wght.l1 <= 89.53607, 
  c("cowcode","year", "reg.fail", "pwt7.grrgdpl.wght.l1", "spellrn",
    "pwt7.rgdpl.cor.l1", "type.l1",
    "ciri.pf.l1", "gd.ptss.l1", "lparty2.l1")
  ],
  prop = FALSE,
  numbers = TRUE
)
matrixplot(dd.1981[
  dd.1981$pwt7.grrgdpl.wght.l1 >= -31.14919 &
    dd.1981$pwt7.grrgdpl.wght.l1 <= 89.53607, 
  c("cowcode","year", "reg.fail", "pwt7.grrgdpl.wght.l1", 
    "spellrn", "pwt7.rgdpl.cor.l1", "type.l1",
    "ciri.pf.l1", "gd.ptss.l1", "lparty2.l1")
  ]
)
dd.1981a <- within(dd.1981, {
  ciri.pf.l1 <- ifelse(is.na(ciri.pf.l1) == TRUE, 
    ciri.pf.l2, ciri.pf.l1)
  ciri.physint2.l1 <- ifelse(is.na(ciri.physint2.l1) == TRUE,
    ciri.physint2.l2, ciri.physint2.l1)
  }
)




