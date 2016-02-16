## =========================================================
## === Heidelberg: Variable Definitions ====================
## === (c) DT, 01.2013 =====================================

## --- Numerical regime codes ------------------------------
dd <- within(dd, { 
  gwf.regimetype.num <- NA      # Recode gwf.regimetype to num
  gwf.regimetype.num[gwf.regimetype=="indirect military"] <- 0
  gwf.regimetype.num[gwf.regimetype=="military"]          <- 1
  gwf.regimetype.num[gwf.regimetype=="military-personal"] <- 2
  gwf.regimetype.num[gwf.regimetype=="monarchy"]          <- 3
  gwf.regimetype.num[gwf.regimetype=="oligarchy"]         <- 4
  gwf.regimetype.num[gwf.regimetype=="party"]             <- 5
  gwf.regimetype.num[gwf.regimetype=="party-military"]    <- 6
  gwf.regimetype.num[gwf.regimetype=="party-personal"]    <- 7
  gwf.regimetype.num[gwf.regimetype=="personal"]          <- 8
  gwf.regimetype.num[gwf.regimetype==
    "party-military-personal"]                            <- 9
  gwf.next.num <- NA                  # recode gwf.next to num
  gwf.next.num[gwf.next == "indirect military"]          <-  0  
  gwf.next.num[gwf.next == "military"]                   <-  1  
  gwf.next.num[gwf.next == "milpersonal"]                <-  2  
  gwf.next.num[gwf.next == "monarchy"]                   <-  3  
  gwf.next.num[gwf.next == "oligarchy"]                  <-  4  
  gwf.next.num[gwf.next == "party"]                      <-  5  
  gwf.next.num[gwf.next == "spmilitary"]                 <-  6  
  gwf.next.num[gwf.next == "sppersonal"]                 <-  7  
  gwf.next.num[gwf.next == "personal"]                   <-  8  
  gwf.next.num[gwf.next == "democracy"]                  <- 90 
  gwf.next.num[gwf.next == "foreign-occupied"]           <- 91 
  gwf.next.num[gwf.next == "not-independent"]            <- 92 
  gwf.next.num[gwf.next == "provisional"]                <- 93 
  gwf.next.num[gwf.next == "tthreat"]                    <- 94 
  gwf.next.num[gwf.next == "warlord"]                    <- 95 
  gwf.next.num[gwf.next == "warlord/foreign-occupied"]   <- 96
  gwf.reg.collapsed <- NA              # Collapse regime types
  gwf.reg.collapsed <- ifelse(gwf.regimetype.num >= 4 &  
    gwf.regimetype.num <= 7 | gwf.regimetype.num == 9, 1, 
    gwf.reg.collapsed)                                 # PARTY
  gwf.reg.collapsed <- ifelse(gwf.regimetype.num >= 0 &
    gwf.regimetype.num <= 2, 2, gwf.reg.collapsed)  # MILITARY
  gwf.reg.collapsed <- ifelse(gwf.regimetype.num == 3, 3, 
    gwf.reg.collapsed)                              # MONARCHY
  gwf.reg.collapsed <- ifelse(gwf.regimetype.num == 8, 4,
    gwf.reg.collapsed)                              # PERSONAL
  gwf.reg.collapsed <- factor(gwf.reg.collapsed,
    levels = c(1, 2, 3, 4),
    labels = c("PARTY", "MILITARY", "MONARCHY", "PERSONAL"))
##
## party-hybrids and oligarchies grouped with dominant-party
## dictatorships and military-personalist hybrids and 
## indirect military regimes grouped with military 
## dictatorships -- following GWF 2012
## Exactly reproduces Geddes original collapsed codings in
## GWF dummies.
##
  gwf.next.collapsed <- NA
  gwf.next.collapsed <- ifelse(gwf.next.num >= 0 &
    gwf.next.num <= 2, 1, gwf.next.collapsed)       # MILITARY
  gwf.next.collapsed <- ifelse(gwf.next.num == 3, 2, 
    gwf.next.collapsed)                             # MONARCHY
  gwf.next.collapsed <- ifelse(gwf.next.num >= 4 &  
    gwf.next.num <= 7 | gwf.next.num == 9, 3, 
    gwf.next.collapsed)                                # PARTY
  gwf.next.collapsed <- ifelse(gwf.next.num == 8, 4,
    gwf.next.collapsed)                             # PERSONAL
  gwf.next.collapsed <- ifelse(gwf.next.num == 90, 5,
    gwf.next.collapsed)                            # DEMOCRACY
  gwf.next.collapsed <- ifelse(gwf.next.num > 90, 6,
    gwf.next.collapsed)                                # OTHER
  gwf.next.collapsed <- factor(gwf.next.collapsed,
    levels = c(1, 2, 3, 4, 5, 6),
    labels = c("MILITARY", "MONARCHY", "PARTY", "PERSONAL",
      "DEMOCRACY", "OTHER"))
  }
)
addmargins(table(dd$gwf.reg.collapsed)) 
# MILITARY MONARCHY    PARTY PERSONAL      Sum 
#      575      557     2210     1103     4445 
table(dd$gwf.next.collapsed)
# MILITARY  MONARCHY     PARTY  PERSONAL DEMOCRACY     OTHER 
#      732        17       250       678      1198       470 
table(dd$gwf.reg.collapsed,dd$gwf.next.collapsed)
#           MILITARY MONARCHY PARTY PERSONAL DEMOCRACY OTHER
#  MILITARY      103        0    43       14       302    65
#  MONARCHY       46        6    38       76        44     7
#  PARTY         206        0    10      193       647   103
#  PERSONAL      126        0    62      165       142   250
table(dd$gwf.reg.collapsed, dd$gwf.geddes.collapsed)
table(dd$gwf.geddes.collapsed, dd$gwf.fail, exclude = NULL)

# --- Compute DV -------------------------------------------
dd <- within(dd, {   # Compute DV excluding cases of foreign
  reg.fail <- gwf.fail  # occupation, lost independence, and
                   # instances of non-autocracy are excluded
  tag <- ifelse(gwf.nonautocracy != "NA", 1, 0) 
       # NOTE: != "NA" IS NO MISTAKE!
       # 3182 non-atr foreign occupation & lost independence
  tag[tag == 0 & 
     (gwf.next.num == 91 | gwf.next.num == 92 | 
     gwf.next.num == 96)] <- 1        # 91: 31; 92: 1; 96: 0 
  tag <- ifelse(is.na(dd$gwf.nonautocracy) == TRUE, 1, tag)
  }                                  # ctry not coded in gwf
)
summary(dd$tag)
addmargins(table(dd$tag))
test <- dd[dd$tag==0, ]
nrow(test)
summary(test$reg.fail)
table(test$reg.fail, exclude = NULL)
barplot(             # Regime failures before and after 1990
  prop.table(table(test$reg.fail, test$year), 2), 
  beside = FALSE,
  ylim = c(0, 1),
  las = 2
)                  # roughly equal var before and after 1990
addmargins(
  table(test$gwf.geddes.collapsed, test$reg.fail, 
    exclude=NULL)
)
#              0    1 <NA>
#  PARTY    2151   54    0
#  MILITARY  498   75    0
#  MONARCHY  545   12    0
#  PERSONAL 1004   74    0
#  <NA>        0    0    0
rm(test)
## =========================================================

## === Define vars =========================================
## --- Economy ---------------------------------------------
## pwt7.grgdpch <- growth in gdp per capita
## pwt7.rgdpch <- gdp per capita
## growth rates corrected for government expenditure
dd <- within(dd, {
  pwt7.rgdpl.cor <- pwt7.rgdpl - (pwt7.rgdpl*(pwt7.kg/100))
  }
)
summary(dd[, c("pwt7.rgdpl", "pwt7.kg", "pwt7.rgdpl.cor")])
head(dd[, c("pwt7.rgdpl", "pwt7.kg", "pwt7.rgdpl.cor")])

library(Hmisc)
dd <- paneldata.lags(
  dd, "cowcode", "year", "pwt7.rgdpl.cor", lags = 1
)
detach(package:Hmisc)
dd <- within(dd, {
 pwt7.grrgdpl.cor <- (pwt7.rgdpl.cor/pwt7.rgdpl.cor.l1*100)-100  
 }
)  
summary(dd[, c("pwt7.grrgdpl.cor", "pwt7.grgdpch")])

## --- Repression ------------------------------------------
## --- (A) Physical Integrity Index ------------------------
## This is an additive index constructed from the Torture 
## (ciri_tort), Extrajudicial Killing (ciri_kill), Political 
## Imprisonment (ciri_polpris), and Disappearance indicators
## (ciri_disap). It ranges from 0 (no government respect for 
## these four rights) to 8 (full government respect for these 
## four rights). (Details on its construction and use can be
## found in Cingranelli and Richards 1999).
## --> reverse scale for analysis
## 0 least repression, 8 most repression
library(car)
dd <- within(dd, {
  ciri.physint2 <- recode(ciri.physint, 
    "0 = 8; 1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 
    7 = 1; 8 = 0")}
)
detach(package:car)
summary(dd$ciri.physint2)
## --- (B) Political Freedoms ------------------------------
library(car)
dd <- within (dd, {
  ciri.speech2 <- ifelse(           # str to num and reverse
    ciri.speech == "0. Complete", 2, 
     ifelse(ciri.speech == "1. Some", 1, 
      ifelse(ciri.speech == "2. None", 0, NA)))
  ciri.worker2 <- ifelse(           # str to num and reverse
    ciri.worker == "0. Severely restricted", 2,
     ifelse(ciri.worker == "1. Somewhat restricted", 1,
      ifelse(ciri.worker == "2. Fully protected", 0, NA)))
  ciri.elecsd2 <- recode(ciri.elecsd, "0=2; 1=1; 2=0") # reversed
  ciri.assn2 <- ifelse(             # str to num and reverse
    ciri.assn == "0. Severely restricted or denied completely to all citizens", 2,
     ifelse(ciri.assn == "1. Limited for all citizens or severly restricted or denied for selected groups", 1, 
      ifelse(ciri.assn == "2. Virtually unrestricted and freely enjoyed by practically all citizens", 0, NA)))}
)
dd$ciri.pf <- apply(                   ## row-wise sum index
  dd[, c("ciri.speech2", "ciri.worker2", "ciri.elecsd2", 
    "ciri.assn2")], 1, sum, na.rm = FALSE
) # if one measurement misses, the entire index misses
apply(
  dd[, c("ciri.speech2", "ciri.worker2", "ciri.elecsd2",
  "ciri.assn2", "ciri.pf")], 2, summary
)
#        ciri.speech2 ciri.worker2 ciri.elecsd2 ciri.assn2 ciri.pf
# Min.           0.00          0.0         0.00        0.0     0.0
# 1st Qu.        0.00          0.0         0.00        0.0     1.0
# Median         1.00          1.0         1.00        1.0     4.0
# Mean           0.98          1.1         0.87        0.9     3.8
# 3rd Qu.        2.00          2.0         2.00        2.0     6.0
# Max.           2.00          2.0         2.00        2.0     8.0
# NA's        4814.00       4815.0      4814.00     4814.0  4825.0
detach(package:car)

## --- Cooptation variables --------------------------------
## cnts.polit10 <- cabinet size
## lparty <- Gandhi legislatures & parties

# ==========================================================
## === (2) Control variables ===============================
## --- Fuel exports as % of merchandise export -------------
summary(dd$wdi.fe, exclude = NULL)  # non-sensical obs >100%
boxplot(dd$wdi.fe~dd$year)    # problem obs limited to <1975
boxplot(dd$wdi.fe~dd$gwf.geddes.collapsed)   # 1 problem obs
# no action taken, ciri omits years before 1981
# --- Cold war --------------------------------------------
dd <- within(dd, {
    coldwar <- ifelse(year >= 1990, 0, 1)
    coldwar <- factor(coldwar, 
      levels = c(0, 1), labels = c("NO", "YES")
    )
  }
)
summary(dd$coldwar)

## --- Elections -------------------------------------------
## exselec -- executive elections
## 1.  Direct
## 2.  Indirect
## 3.  Nonelective 
## legselec -- legislative elections
## 0.  No legislature
## 1.  Non-€elective legislature
## 2.  Elective
dd <- within(dd, {
  lelections <- ifelse(legselec == 2, 1, 0)
  lelections <- factor(lelections,
    levels = c(0, 1),
    labels = c("NO", "YES")
  )
  eelections <- ifelse(exselec <= 2, 1, 0)
  eelections <- factor(eelections,
    levels = c(0, 1),
    labels = c("NO", "YES")
  )
  }
)
with(dd, table(exselec, eelections, exclude = NULL))
with(dd, table(legselec, lelections, exclude = NULL))
head(dd[with(dd, {exselec == 0}), c("ctryname", "year")])
## 1 Coding error: Benin 1963, no consequences for analysis

## --- regional controls -----------------------------------
dd <- within(dd, {
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
dd <- within(dd, {
  lparty2 <- factor(lparty,
    levels = c(0, 1, 2),
    labels = c("NoL", "L1Pa", "LmPa")
  )
  lparty2 <- relevel(lparty2, ref = "L1Pa")
  }
)

## --- Alternative dv's ------------------------------------
dd <- within(dd, {
    eheads.fail <- ifelse(eheads > 0, 1, 0)
#    eheads.fail <- factor(eheads.fail,
#      levels = c(0, 1),
#      labels = c("NO", "YES")
#    )
    eheads.Ifail <- ifelse(archigos.eheadIsum > 0, 1, 0)
#    eheads.Ifail <- factor(eheads.Ifail,
#      levels = c(0, 1),
#      labels = c("NO", "YES")
#    )
  }
)
## =========================================================
## === DF for analysis =====================================
vars <- c("cowcode","cowcode2", "year", "ctryname", 
  "reg.fail",                                           # DV
  "gwf.geddes.collapsed",                          # Regtype
  "ciri.physint2", "ciri.pf", "fh.cl", "fh.pr", # repression
  "lparty2", "lparty",                        # institutions
  "pwt7.rgdpch", "pwt7.grgdpch",                   # economy
  "cnts.polit10",                              # co-optation
  "un.region",                       # controls & additional
  "un.region.name", "wdi.fe", "ht.region",   
  "eelections", "lelections", "coldwar", "mena", "fsu",
  "pwt7.pop", "gwf.next.num", "eheads.fail", "eheads.Ifail",
  "pwt7.kg", "pwt7.rgdpl", "pwt7.rgdpl.cor", 
  "pwt7.grrgdpl.cor", "gd.ptss"
)
names(dd[names(dd) %in% vars])
dd.anal <- dd[dd$tag==0 & dd$year >= 1981, vars]
rm(list = c("vars"))

## === Time Dependency =====================================
## --- Past failures -- do future depend on past events? ---
dd.anal <- dd.anal[with(dd.anal, {order(cowcode, year)}), ]
library(plyr)
dd.anal <- ddply(
  dd.anal,
  .variables = "cowcode",
  transform,             # no clue why, won't work otherwise
  cum.Fail = cumsum(reg.fail)
)

library(Hmisc)
dd.anal <- paneldata.lags( # lag so cum.Fail+1 in year AFTER fail!
  dd.anal, "cowcode", "year", c("cum.Fail"), 1
)
detach(package:Hmisc)

dd.anal$cum.Fail.l1[is.na(dd.anal$cum.Fail.l1) == TRUE] <- 0
exclude <- names(dd.anal) %in% c("cum.Fail")
dd.anal <- dd.anal[!exclude]
rm(exclude)

library(reshape)
dd.anal <- rename(dd.anal, c(cum.Fail.l1="cum.Fail"))
detach(package:reshape)
summary(dd.anal$cum.Fail)
boxplot(dd.anal$cum.Fail~dd.anal$gwf.geddes.collapsed)

# --- Polynomials -- Carter/Signorino 2010 -----------------
attach(dd.anal)
byspell <- by(dd.anal[,"cum.Fail"], 
              INDICES=cowcode,
              FUN=rle  # extract regime spell type & length
)
detach(dd.anal)
# byspell == list of 
# a) spell type ([[,"values"]]) &
# b) length ([[,"lengths"]])
# byspell is ordered by cowcode & year
# ----------------------------------------------------------
# Convert [[,"lengths"]] to vector of regime types
spellln <- NULL # spell length
for(i in names(byspell))
  spellln <- append(spellln,
              byspell[[c(i,"lengths")]], # extract lengths &
              after=length(spellln) # append to vector
)
# Convert [[,"values"]] to vector of regime types 
# necessary for consistency
# Compute running number of spell in cowcode2 as sequence
# from 1 to number of elements in [[,"values"]]
spellno <- NULL 
for(i in names(byspell))
  spellno <- append(spellno,
                    seq(
                        1:length(
                                 byspell[[c(i,"values")]]
                                 )
                        ),
                     after=length(spellno)
)
# Combine variables to df and clean
testdf <- data.frame(spellln, spellno)
rm(list=c("spellln", "spellno", "byspell", "i"))
# Inflate df according to length of individual regime spells
testdf <- testdf[rep(1:nrow(testdf),times=testdf$spellln),]
# Previous command interferes with row names which 
# are given running suffixes according to the number of 
# copies (e.g "1.24" indicates row 1 copy 24)
# --> modified rownames indicate running spell length
# ----------------------------------------------------------
# Extract row.names and convert to variable indicating
# running spell length
runningrow <-row.names(testdf) # string vector of row names
rownames(testdf) <- NULL # reset modified rownames in df
runningpos <- strsplit(runningrow,".",fixed=TRUE) 
# split string to list containing original row number
# and running index of copy
spellrn <- sapply(runningpos, "[", 2)
# extract index of copied entries as numeric vector
spellrn <- as.numeric(spellrn)
spellrn[is.na(spellrn)] <- 0
# NAs result from the original row number where there
# is no second element to extract with sapply()
# recoded to 0 
spellrn <- spellrn+1 # adjust running spell length to match
                     # spellln
testdf <- data.frame(testdf,spellrn)
rm(list=c("runningrow","runningpos"))
# ----------------------------------------------------------
# combine df's
dd.anal <- data.frame(dd.anal, testdf)
rm(list=c("testdf","spellrn"))
dd.anal <- within(dd.anal, {
  spellrn2 <- spellrn^2
  spellrn3 <- spellrn^3}
)
# spellln, spellln2, and spellln3 give time-dependency
# polynomials
library(reshape)
dd.anal <- rename(dd.anal, c(gwf.geddes.collapsed = "type"))
detach(package:reshape)
row.names(dd.anal) <- paste(
  dd.anal$cowcode, dd.anal$year, sep = ":"
)
## END 