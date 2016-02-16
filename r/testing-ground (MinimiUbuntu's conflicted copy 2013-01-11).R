## =========================================================
## === PrepAnal ============================================
## Region
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
## 1) Lag all variables by one year
library(Hmisc)
dd.anal <- paneldata.lags(dd.anal, "cowcode", "year", 
  c("type",                                        # Regtype
  "ciri.physint2", "ciri.pf",                   # repression
  "lparty",                                   # institutions
  "pwt7.rgdpch", "pwt7.grgdpch", "unna.grgdp",     # economy
  "cnts.polit10",                              # co-optation
  "wdi.fe",
  "eelections", "lelections"),
  lags=1
)
detach(package:Hmisc)
# --- Test run ---------------------------------------------
fit1 <-  glm(             # time dependency model & controls
  reg.fail ~ pwt7.grgdpch.l1 + 
  type + log(pwt7.rgdpch.l1) + fsu + mena + eelections.l1 + 
  coldwar +  
  cum.Fail + spellrn + I(spellrn^2) + I(spellrn^3),
  family = binomial(link=logit),
  data = subset(dd.anal, year >= 1981)
)
summary(fit1)
fit2 <-  glm(             # time dependency model & controls
  reg.fail ~ pwt7.grgdpch.l1 + 
  ciri.physint2.l1 + ciri.pf.l1 +
  cnts.polit10.l1 +
  type + log(pwt7.rgdpch.l1) + fsu + mena + lparty.l1 + 
  coldwar +  
  cum.Fail + spellrn + I(spellrn^2) + I(spellrn^3),
  family = binomial(link=logit),
  data = subset(dd.anal, year >= 1981)
)
summary(fit2)
library(car)

# paneldata.lags(A, "person", "year", c("v1","v2"), lags=1:4) 

"lparty",                                   # institutions
  "pwt7.rgdpch", "pwt7.grgdpch",                   # economy
  "cnts.polit10",                              # co-optation
  "un.region",                                    # controls
  "un.region.name", "wdi.fe", "ht.region", 
  "eelections", "lelections", "coldwar")