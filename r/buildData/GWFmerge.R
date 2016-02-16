## Heidelberg: merge GWF with DD
## =========================================================
library(foreign)
gwf <- read.dta(
  file.path(pathData, 'base', 'GWF2012.dta'),
  convert.underscore = TRUE,
  convert.dates      = TRUE,
  convert.factors    = TRUE
)
detach(package:foreign)
# --- Generate cowcode2.1 ----------------------------------
gwf <- within(gwf, {                  # double-check codings
  cowcode2.1 <- NA
  cowcode2.1[cowcode==341] <- 341
  cowcode2.1[cowcode==212] <- 212
  cowcode2.1[cowcode==395] <- 395
  cowcode2.1[cowcode==700] <- 700
  cowcode2.1[cowcode==339] <- 339
  cowcode2.1[cowcode==615] <- 615
  cowcode2.1[cowcode==540] <- 540
  cowcode2.1[cowcode==160] <- 160
  cowcode2.1[cowcode==371] <- 371
  cowcode2.1[cowcode==900] <- 900
  cowcode2.1[cowcode==305] <- 305
  cowcode2.1[cowcode==373] <- 373
  cowcode2.1[cowcode==692] <- 692
  cowcode2.1[cowcode==771] <- 771
  cowcode2.1[cowcode==370] <- 370
  cowcode2.1[cowcode==211] <- 211
  cowcode2.1[cowcode==434] <- 434
  cowcode2.1[cowcode==760] <- 760
  cowcode2.1[cowcode==145] <- 145
  cowcode2.1[cowcode==346] <- 346
  cowcode2.1[cowcode==571] <- 571
  cowcode2.1[cowcode==140] <- 140
  cowcode2.1[cowcode==355] <- 355
  cowcode2.1[cowcode==439] <- 439
  cowcode2.1[cowcode==516] <- 516
  cowcode2.1[cowcode==811] <- 811
  cowcode2.1[cowcode==471] <- 471
  cowcode2.1[cowcode==20 ] <- 20
  cowcode2.1[cowcode==482] <- 482
  cowcode2.1[cowcode==483] <- 483
  cowcode2.1[cowcode==155] <- 155
  cowcode2.1[cowcode==710] <- 710
  cowcode2.1[cowcode==100] <- 100
  cowcode2.1[cowcode==581] <- 581
  cowcode2.1[cowcode==484] <- 484
  cowcode2.1[cowcode==490] <- 490
  cowcode2.1[cowcode==94 ] <- 94
  cowcode2.1[cowcode==437] <- 437
  cowcode2.1[cowcode==344] <- 344
  cowcode2.1[cowcode==40 ] <- 40
  cowcode2.1[cowcode==352] <- 352
  cowcode2.1[cowcode==316] <- 316
  cowcode2.1[cowcode==315] <- 315
  cowcode2.1[cowcode==390] <- 390
  cowcode2.1[cowcode==522] <- 522
  cowcode2.1[cowcode==42 ] <- 42
  cowcode2.1[cowcode==130] <- 130
  cowcode2.1[cowcode==651] <- 651
  cowcode2.1[cowcode==92 ] <- 92
  cowcode2.1[cowcode==411] <- 411
  cowcode2.1[cowcode==531] <- 531
  cowcode2.1[cowcode==366] <- 366
  cowcode2.1[cowcode==530] <- 530
  cowcode2.1[cowcode==530] <- 530
  cowcode2.1[cowcode==950] <- 950
  cowcode2.1[cowcode==375] <- 375
  cowcode2.1[cowcode==220] <- 220
  cowcode2.1[cowcode==481] <- 481
  cowcode2.1[cowcode==420] <- 420
  cowcode2.1[cowcode==372] <- 372
  cowcode2.1[cowcode==255] <- 260
  cowcode2.1[cowcode==255 & year>=1990] <- 255
  cowcode2.1[cowcode==265] <- 265
  cowcode2.1[cowcode==452] <- 452
  cowcode2.1[cowcode==350] <- 350
  cowcode2.1[cowcode==90 ] <- 90
  cowcode2.1[cowcode==438] <- 438
  cowcode2.1[cowcode==404] <- 404
  cowcode2.1[cowcode==110] <- 110
  cowcode2.1[cowcode==41 ] <- 41
  cowcode2.1[cowcode==91 ] <- 91
  cowcode2.1[cowcode==310] <- 310
  cowcode2.1[cowcode==750] <- 750
  cowcode2.1[cowcode==850] <- 850
  cowcode2.1[cowcode==630] <- 630
  cowcode2.1[cowcode==645] <- 645
  cowcode2.1[cowcode==205] <- 205
  cowcode2.1[cowcode==666] <- 666
  cowcode2.1[cowcode==325] <- 325
  cowcode2.1[cowcode==51 ] <- 51
  cowcode2.1[cowcode==740] <- 740
  cowcode2.1[cowcode==663] <- 663
  cowcode2.1[cowcode==705] <- 705
  cowcode2.1[cowcode==501] <- 501
  cowcode2.1[cowcode==731] <- 731
  cowcode2.1[cowcode==732] <- 732
  cowcode2.1[cowcode==690] <- 690
  cowcode2.1[cowcode==703] <- 703
  cowcode2.1[cowcode==812] <- 812
  cowcode2.1[cowcode==367] <- 367
  cowcode2.1[cowcode==660] <- 660
  cowcode2.1[cowcode==570] <- 570
  cowcode2.1[cowcode==450] <- 450
  cowcode2.1[cowcode==620] <- 620
  cowcode2.1[cowcode==368] <- 368
  cowcode2.1[cowcode==343] <- 343
  cowcode2.1[cowcode==580] <- 580
  cowcode2.1[cowcode==553] <- 553
  cowcode2.1[cowcode==820] <- 820
  cowcode2.1[cowcode==432] <- 432
  cowcode2.1[cowcode==435] <- 435
  cowcode2.1[cowcode==590] <- 590
  cowcode2.1[cowcode==70 ] <- 70
  cowcode2.1[cowcode==359] <- 359
  cowcode2.1[cowcode==712] <- 712
  cowcode2.1[cowcode==600] <- 600
  cowcode2.1[cowcode==541] <- 541
  cowcode2.1[cowcode==775] <- 775
  cowcode2.1[cowcode==565] <- 565
  cowcode2.1[cowcode==790] <- 790
  cowcode2.1[cowcode==210] <- 210
  cowcode2.1[cowcode==920] <- 920
  cowcode2.1[cowcode==93 ] <- 93
  cowcode2.1[cowcode==436] <- 436
  cowcode2.1[cowcode==475] <- 475
  cowcode2.1[cowcode==385] <- 385
  cowcode2.1[cowcode==698] <- 698
  cowcode2.1[cowcode==770] <- 770
  cowcode2.1[cowcode==95 ] <- 95
  cowcode2.1[cowcode==910] <- 910
  cowcode2.1[cowcode==150] <- 150
  cowcode2.1[cowcode==135] <- 135
  cowcode2.1[cowcode==840] <- 840
  cowcode2.1[cowcode==290] <- 290
  cowcode2.1[cowcode==235] <- 235
  cowcode2.1[cowcode==694] <- 694
  cowcode2.1[cowcode==360] <- 360
  cowcode2.1[cowcode==365] <- 365
  cowcode2.1[cowcode==365 & year>=1991] <- 374
  cowcode2.1[cowcode==517] <- 517
  cowcode2.1[cowcode==670] <- 670
  cowcode2.1[cowcode==433] <- 433
  cowcode2.1[cowcode==345] <- 345
  cowcode2.1[cowcode==345 & year>=1991 & year<=2005] <- 347
  cowcode2.1[cowcode==345 & year>=2006] <- 342
  cowcode2.1[cowcode==451] <- 451
  cowcode2.1[cowcode==830] <- 830
  cowcode2.1[cowcode==317] <- 317
  cowcode2.1[cowcode==349] <- 349
  cowcode2.1[cowcode==520] <- 520
  cowcode2.1[cowcode==560] <- 560
  cowcode2.1[cowcode==230] <- 230
  cowcode2.1[cowcode==780] <- 780
  cowcode2.1[cowcode==625] <- 625
  cowcode2.1[cowcode==572] <- 572
  cowcode2.1[cowcode==380] <- 380
  cowcode2.1[cowcode==225] <- 225
  cowcode2.1[cowcode==652] <- 652
  cowcode2.1[cowcode==713] <- 713
  cowcode2.1[cowcode==702] <- 702
  cowcode2.1[cowcode==510] <- 510
  cowcode2.1[cowcode==800] <- 800
  cowcode2.1[cowcode==860] <- 860
  cowcode2.1[cowcode==461] <- 461
  cowcode2.1[cowcode==52 ] <- 52
  cowcode2.1[cowcode==616] <- 616
  cowcode2.1[cowcode==640] <- 640
  cowcode2.1[cowcode==701] <- 701
  cowcode2.1[cowcode==500] <- 500
  cowcode2.1[cowcode==369] <- 369
  cowcode2.1[cowcode==696] <- 696
  cowcode2.1[cowcode==200] <- 200
  cowcode2.1[cowcode==2  ] <- 2
  cowcode2.1[cowcode==165] <- 165
  cowcode2.1[cowcode==704] <- 704
  cowcode2.1[cowcode==101] <- 101
  cowcode2.1[cowcode==816] <- 816
  cowcode2.1[cowcode==816 & year>=1976] <- 818
  cowcode2.1[cowcode==817] <- 817
  cowcode2.1[cowcode==678] <- 678
  cowcode2.1[cowcode==678 & year>=1990] <- 679
  cowcode2.1[cowcode==680] <- 680
  cowcode2.1[cowcode==551] <- 551
  cowcode2.1[cowcode==552] <- 552 
  }
)
dev.new()
plot(
  gwf$cowcode2, gwf$cowcode2.1, 
  type="l", 
  main="Double-Check Country Codes")
cor(gwf[,c("cowcode2", "cowcode2.1")], use="complete.obs")
sapply(gwf[, c("cowcode2", "cowcode2.1")], summary)

table(gwf$gwf.country[is.na(gwf$cowcode2.1) == TRUE]) 
# NA ctry's?

# --- Convert Geddes dummies to factor ---------------------
gwf <- within(gwf, {
  gwf.geddes.collapsed <- NA
  gwf.geddes.collapsed <- ifelse(
    gwf.party==1, 1,                                 # PARTY
      ifelse(gwf.military==1, 2,                  # MILITARY
        ifelse(gwf.monarchy==1, 3,                # MONARCHY
          ifelse(gwf.personal==1, 4,              # PERSONAL
            gwf.geddes.collapsed
          )
        )
      )
  )
  gwf.geddes.collapsed <- factor(gwf.geddes.collapsed,
    levels = c(1, 2, 3, 4),
    labels = c("PARTY", "MILITARY", "MONARCHY", "PERSONAL"))
  }
)
summary(gwf$gwf.geddes.collapsed)
# ----------------------------------------------------------
library(ETLUtils)
dd <- matchmerge(
  x     = dd,
  y     = gwf,
  by.x  = c("cowcode2", "year"),
  by.y  = c("cowcode2", "year"),
  all.x = TRUE,
  add.columns = c("gwf.regimetype",    # Geddes Double Check
    "gwf.fail",                         # Dependent Variable
    "gwf.next",         # Exclude foreign occupation from DV
    "gwf.duration",                    # in case interesting
    "gwf.geddes.collapsed",            # Geddes Double Check
    "gwf.prior",                 # Exclude democracy failure
    "gwf.party",          # Regime dummies for density plots
    "gwf.military",
    "gwf.monarchy",
    "gwf.personal",
    "gwf.nonautocracy"), 
  check.duplicates = TRUE,
  trace = TRUE
)
# --- Tidy workspace ---------------------------------------
detach(package:ETLUtils)
detach(package:ff)
detach(package:bit)
rm(gwf)
## END