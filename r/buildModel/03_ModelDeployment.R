
library('car')
load(file.path(pathOut, 'base_addedEshock.RData'))
row.names(dd.1981) <- with(dd.1981,    # readable row names.
  paste(cowcode, year, sep = ":")
)
## === Model presentation ==================================
## dropped conceptually unsatisfying controls:
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

## Anova tests for nested models ---------------------------
anova(fitPres.a,fitPres.b, test = "Chisq")
anova(fitPres.b,fitPres.c, test = "Chisq")
anova(fitPres.a,fitPres.c, test = "Chisq")
## END