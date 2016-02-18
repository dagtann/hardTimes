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
## END