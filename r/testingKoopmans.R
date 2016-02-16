
dd.1981 <- subset(dd.anal, year >= 1981)
# --- Distribution -----------------------------------------
with(dd.1981, {
    boxplot(pwt7.grgdpch.l1, horizontal = TRUE)
  }
)
with(dd.1981, {
  boxplot(cnts.polit10.l1, horizontal = TRUE)
  }
)
with(dd.1981, {
  boxplot(ciri.pf.l1, horizontal = TRUE)
  }
)
with(dd.1981, {
  boxplot(ciri.physint2.l1, horizontal = TRUE)
  }
)
# --- Correlation ------------------------------------------
?cor
options(digits = 3)
cor(dd.1981[, c("pwt7.grgdpch.l1", "cnts.polit10.l1", 
  "ciri.pf.l1", "ciri.physint2.l1")],
  method = "pearson",
  use = "pairwise.complete.obs"
)
fit1 <- glm(
  reg.fail ~ unna.grgdp.l1 + 
    cnts.polit10.l1 +
    ciri.pf.l1 +
    ciri.physint2.l1,
    family=binomial(link=logit),
    data = dd.1981
)
with(dd.1981, {
  by(pwt7.grgdpch.l1,
    INDICES = reg.fail,
    FUN = summary)
  })
dd.1981  <- within(dd.1981, {
  recession <- ifelse(unna.grgdp.l1 <= 0, 1, 0)
  recession <- factor(recession,
    levels = c(0,1),
    labels = c("No", "Yes")
    )
  }
)
mean(dd.1981$recession, na.rm = TRUE)
fit1 <- glm(
  reg.fail ~ recession + 
    cnts.polit10.l1 +
    ciri.pf.l1 +
    ciri.physint2.l1,
    family=binomial(link=logit),
    data = dd.1981
)
summary(fit1)
plot(allEffects(fit1))
fit2 <- glm(
  reg.fail ~ recession*cnts.polit10.l1 + 
    recession*ciri.pf.l1 +
    recession*ciri.physint2.l1,
    family=binomial(link=logit),
    data = dd.1981
)
summary(fit2)
library(effects)
plot(effect("recession*cnts.polit10.l1", fit2,
  focal.predictors = "recession",
  xlevels = list(cnts.polit10.l1 = c(13, 24, 46))))
with(dd.1981, {
  quantile(cnts.polit10.l1, 
    prob = c(0.025, 0.5, 0.975),
    na.rm = TRUE
    )
  }
)
summary(fit2)
cor(dd.anal[, c("pwt7.grgdpch.l1", "unna.grgdp.l1")],
  method = "pearson",
  use = "pairwise.complete.obs"
)