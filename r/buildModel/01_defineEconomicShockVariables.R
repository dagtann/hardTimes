# This script defines additional variables for the analysis
# and lags them by 1 year.

# Lag all variables by one year ----------------------------
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

with(dd.anal, levels(lparty2)) # generate factor from lagged
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

## Sample statistics ---------------------------------------
dd.1981 <- subset(dd.anal, year >= 1981)   # df for analysis
nrow(dd.1981)                                   ## 2061 cyrs
with(dd.1981,                                  
  length(unique(paste(cowcode, spellno, sep = ":")))
)                                              # 160 regimes
with(dd.1981, length(unique(cowcode)))       # 110 countries

## Survival time distribution ------------------------------
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

# Economic shocks ------------------------------------------
## Is filtering required? ----------------------------------
summary(dd.1981[, c("pwt7.grrgdpl.cor", "pwt7.grrgdpl.cor.l1")])
with(dd.1981, {
  boxplot(pwt7.grrgdpl.cor.l1 ~ cowcode)
  }
)            
with(dd.1981, {
  boxplot(
    by(
      pwt7.grrgdpl.cor.l1, INDICES = cowcode, FUN = sd,
      na.rm = TRUE
    )
  )
  }
)
             ## both visualisations:
             ## tremendous differences in median, spread, sd
             ## -> filter justified

## --- Filter growth (Petrova/Bates 2011) ------------------
## Definition:
## 1. year of shock: delta gr(i,t) < -2 ppa
## 2. after shock gr(i,t) < 2
## Indicator variable:
## shock(t,t-2) == 1 if shock at t | t-1 | t-2, 
## and 0 otherwise
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
    eshock <- ifelse(
      tag3 == 1 | tag3.l1 == 1 | tag3.l2 == 1, 1, 0
    )
    eshock <- factor(
      eshock, levels = c(0, 1), labels = c("NO", "YES")
    )
    tag3.l1 <- factor(
      tag3.l1, levels = c(0, 1), labels = c("NO", "YES")
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
## expectations
## split information at 0: preliminary threshold,
## demonstrate robustness using other thresholds
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
dd.1981 <- within(dd.1981, delta.kg <- pwt7.kg - pwt7.kg.l1)
summary(dd.1981$delta.kg)
with(dd.1981, 
  cor(delta.grgdp, delta.kg, use = "complete.obs")
)
with(dd.1981, plot(delta.grgdp, delta.kg))
# the smaller the growth rate, the larger the government
# spending

## Accumulated growth experience indicators ----------------
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
  transform,                    # no clue why, but necessary
  neg.spell = cumsum(neg2pos.tag)
)
dd.1981 <- ddply(   # count consecutive obs within pos.spell
  dd.1981,
  .variables = c("cowcode", "spellno", "neg.spell"),
  transform,                    # no clue why, but necessary
  neg.run = cumsum(neg.dum)
)
summary(dd.1981[, c("pos.spell", "pos.run", "neg.spell", 
  "neg.run")])
detach(package:plyr)

exclude <- names(dd.1981) %in% c(
  "pos.dum.l1", "pos.dum", "pos.spell", "pos2neg.tag",
  "neg.dum", "neg.dum.l1", "neg.spell", "neg2pos.tag"
)
dd.1981 <- dd.1981[!exclude]
rm(exclude)

summary(dd.1981[, c("pos.run", "neg.run")])
boxplot(dd.1981[, c("pos.run", "neg.run")])
dd.1981 <- dd.1981[with(dd.1981, order(cowcode, year)),]
# View(dd.1981[, c("ctryname", "year", "pwt7.grrgdpl.cor",
#  "pos.run", "neg.run")])
# pos.run and neg.run give the number of consecutive years 
# with negative or positive growth

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

library(Hmisc)                         # lag weighted growth
dd.1981 <- paneldata.lags(
  dd.1981, "cowcode", "year", "pwt7.grgdpch.wght", lags = 1)
detach(package:Hmisc)
View(dd.1981[, 
  c("ctryname", "year", "pwt7.grgdpch.wght", "pos.run", 
    "neg.run", "pwt7.grgdpch", "pwt7.grgdpch.wght.l1")
  ]
)

# --- housekeeping -----------------------------------------
save.image(file.path(pathOut, 'base_addedEshock.RData'))
## END