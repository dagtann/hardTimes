# This script runs and inspects a series of models testing
# how economic shocks impact prospects for regime survival.
# ==========================================================
row.names(dd.1981) <- with(dd.1981,    # readable row names.
  paste(cowcode, year, sep = ":")
)

library(car)
# Baseline model: economic downturn & controls -------------
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
## Summary:
## Truncating the distribution alters time dependency and
## increases the effect of grgdpch, but grgdpch stays
## insignificant.

## Qualitative indicators ----------------------------------
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
## grneg.2 insignificant, Qualitative filters not informative

## sustained growth experience -----------------------------
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
## END