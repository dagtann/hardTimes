## Heidelberg: Merge DD with Penn World Tables
## ---------------------------------------------------------
pwt <- read.csv(
  file.path(pathData, "base", "pwt71_w_country_names.csv"),
  header = TRUE
)
pwt <- within(pwt, {
  cowcode2 <- NA
  cowcode2[isocode=="AFG"] <- 700
  cowcode2[isocode=="ALB"] <- 339
  cowcode2[isocode=="DZA"] <- 615
  cowcode2[isocode=="AGO"] <- 540
  cowcode2[isocode=="ATG"] <- 58
  cowcode2[isocode=="ARG"] <- 160
  cowcode2[isocode=="ARM"] <- 371
  cowcode2[isocode=="AUS"] <- 900
  cowcode2[isocode=="AUT"] <- 305
  cowcode2[isocode=="AZE"] <- 373
  cowcode2[isocode=="BHS"] <- 31
  cowcode2[isocode=="BHR"] <- 692
  cowcode2[isocode=="BGD"] <- 771
  cowcode2[isocode=="BRB"] <- 53
  cowcode2[isocode=="BLR"] <- 370
  cowcode2[isocode=="BEL"] <- 211
  cowcode2[isocode=="BLZ"] <- 80
  cowcode2[isocode=="BEN"] <- 434
  cowcode2[isocode=="BTN"] <- 760
  cowcode2[isocode=="BOL"] <- 145
  cowcode2[isocode=="BIH"] <- 346
  cowcode2[isocode=="BWA"] <- 571
  cowcode2[isocode=="BRA"] <- 140
  cowcode2[isocode=="BRN"] <- 835
  cowcode2[isocode=="BGR"] <- 355
  cowcode2[isocode=="BFA"] <- 439
  cowcode2[isocode=="BDI"] <- 516
  cowcode2[isocode=="KHM"] <- 811
  cowcode2[isocode=="CMR"] <- 471
  cowcode2[isocode=="CAN"] <- 20
  cowcode2[isocode=="CPV"] <- 402
  cowcode2[isocode=="CAF"] <- 482
  cowcode2[isocode=="TCD"] <- 483
  cowcode2[isocode=="CHL"] <- 155
  cowcode2[isocode=="CH2"] <- 710               # Use China2
  cowcode2[isocode=="COL"] <- 100
  cowcode2[isocode=="COM"] <- 581
  cowcode2[isocode=="ZAR"] <- 490
  cowcode2[isocode=="COG"] <- 484
  cowcode2[isocode=="CRI"] <- 94
  cowcode2[isocode=="CIV"] <- 437
  cowcode2[isocode=="HRV"] <- 344
  cowcode2[isocode=="CUB"] <- 40
  cowcode2[isocode=="CYP"] <- 352
  cowcode2[isocode=="CZE"] <- 316
  cowcode2[isocode=="DNK"] <- 390
  cowcode2[isocode=="DJI"] <- 522
  cowcode2[isocode=="DMA"] <- 54
  cowcode2[isocode=="DOM"] <- 42
  cowcode2[isocode=="ECU"] <- 130
  cowcode2[isocode=="EGY"] <- 651
  cowcode2[isocode=="SLV"] <- 92
  cowcode2[isocode=="GNQ"] <- 411
  cowcode2[isocode=="ERI"] <- 531
  cowcode2[isocode=="EST"] <- 366
  cowcode2[isocode=="ETH"] <- 530
  cowcode2[isocode=="FJI"] <- 950
  cowcode2[isocode=="FIN"] <- 375
  cowcode2[isocode=="FRA"] <- 220
  cowcode2[isocode=="GAB"] <- 481
  cowcode2[isocode=="GMB"] <- 420
  cowcode2[isocode=="GEO"] <- 372
  cowcode2[isocode=="GER"] <- 255
  cowcode2[isocode=="GHA"] <- 452
  cowcode2[isocode=="GRC"] <- 350
  cowcode2[isocode=="GRD"] <- 55
  cowcode2[isocode=="GTM"] <- 90
  cowcode2[isocode=="GIN"] <- 438
  cowcode2[isocode=="GNB"] <- 404
  cowcode2[isocode=="GUY"] <- 110
  cowcode2[isocode=="HTI"] <- 41
  cowcode2[isocode=="HND"] <- 91
  cowcode2[isocode=="HUN"] <- 310
  cowcode2[isocode=="ISL"] <- 395
  cowcode2[isocode=="IND"] <- 750
  cowcode2[isocode=="IDN"] <- 850
  cowcode2[isocode=="IRN"] <- 630
  cowcode2[isocode=="IRQ"] <- 645
  cowcode2[isocode=="IRL"] <- 205
  cowcode2[isocode=="ISR"] <- 666
  cowcode2[isocode=="ITA"] <- 325
  cowcode2[isocode=="JAM"] <- 51
  cowcode2[isocode=="JPN"] <- 740
  cowcode2[isocode=="JOR"] <- 663
  cowcode2[isocode=="KAZ"] <- 705
  cowcode2[isocode=="KEN"] <- 501
  cowcode2[isocode=="KIR"] <- 946
  cowcode2[isocode=="KOR"] <- 732
  cowcode2[isocode=="KWT"] <- 690
  cowcode2[isocode=="KGZ"] <- 703
  cowcode2[isocode=="LAO"] <- 812
  cowcode2[isocode=="LVA"] <- 367
  cowcode2[isocode=="LBN"] <- 660
  cowcode2[isocode=="LSO"] <- 570
  cowcode2[isocode=="LBR"] <- 450
  cowcode2[isocode=="LBY"] <- 620
  cowcode2[isocode=="LTU"] <- 368
  cowcode2[isocode=="LUX"] <- 212
  cowcode2[isocode=="MKD"] <- 343
  cowcode2[isocode=="MDG"] <- 580
  cowcode2[isocode=="MWI"] <- 553
  cowcode2[isocode=="MYS"] <- 820
  cowcode2[isocode=="MDV"] <- 781
  cowcode2[isocode=="MLI"] <- 432
  cowcode2[isocode=="MLT"] <- 338
  cowcode2[isocode=="MHL"] <- 983
  cowcode2[isocode=="MRT"] <- 435
  cowcode2[isocode=="MUS"] <- 590
  cowcode2[isocode=="MEX"] <- 70
  cowcode2[isocode=="FSM"] <- 987
  cowcode2[isocode=="MDA"] <- 359
  cowcode2[isocode=="MNG"] <- 712
  cowcode2[isocode=="MNE"] <- 341
  cowcode2[isocode=="MAR"] <- 600
  cowcode2[isocode=="MOZ"] <- 541
  cowcode2[isocode=="NAM"] <- 565
  cowcode2[isocode=="NPL"] <- 790
  cowcode2[isocode=="NLD"] <- 210
  cowcode2[isocode=="NZL"] <- 920
  cowcode2[isocode=="NIC"] <- 93
  cowcode2[isocode=="NER"] <- 436
  cowcode2[isocode=="NGA"] <- 475
  cowcode2[isocode=="NOR"] <- 385
  cowcode2[isocode=="OMN"] <- 698
  cowcode2[isocode=="PAK"] <- 770
  cowcode2[isocode=="PLW"] <- 986
  cowcode2[isocode=="PAN"] <- 95
  cowcode2[isocode=="PNG"] <- 910
  cowcode2[isocode=="PRY"] <- 150
  cowcode2[isocode=="PER"] <- 135
  cowcode2[isocode=="PHL"] <- 840
  cowcode2[isocode=="POL"] <- 290
  cowcode2[isocode=="PRT"] <- 235
  cowcode2[isocode=="QAT"] <- 694
  cowcode2[isocode=="ROM"] <- 360
  cowcode2[isocode=="RUS"] <- 374
  cowcode2[isocode=="RWA"] <- 517
  cowcode2[isocode=="WSM"] <- 990
  cowcode2[isocode=="STP"] <- 403
  cowcode2[isocode=="SAU"] <- 670
  cowcode2[isocode=="SEN"] <- 433
  cowcode2[isocode=="SRB"] <- 342
  cowcode2[isocode=="SYC"] <- 591
  cowcode2[isocode=="SLE"] <- 451
  cowcode2[isocode=="SGP"] <- 830
  cowcode2[isocode=="SVK"] <- 317
  cowcode2[isocode=="SVN"] <- 349
  cowcode2[isocode=="SLB"] <- 940
  cowcode2[isocode=="SOM"] <- 520
  cowcode2[isocode=="ZAF"] <- 560
  cowcode2[isocode=="ESP"] <- 230
  cowcode2[isocode=="LKA"] <- 780
  cowcode2[isocode=="KNA"] <- 60
  cowcode2[isocode=="LCA"] <- 56
  cowcode2[isocode=="VCT"] <- 57
  cowcode2[isocode=="SDN"] <- 625
  cowcode2[isocode=="SUR"] <- 115
  cowcode2[isocode=="SWZ"] <- 572
  cowcode2[isocode=="SWE"] <- 380
  cowcode2[isocode=="CHE"] <- 225
  cowcode2[isocode=="SYR"] <- 652
  cowcode2[isocode=="TWN"] <- 713
  cowcode2[isocode=="TJK"] <- 702
  cowcode2[isocode=="TZA"] <- 510
  cowcode2[isocode=="THA"] <- 800
  cowcode2[isocode=="TLS"] <- 860
  cowcode2[isocode=="TGO"] <- 461
  cowcode2[isocode=="TON"] <- 955
  cowcode2[isocode=="TTO"] <- 52
  cowcode2[isocode=="TUN"] <- 616
  cowcode2[isocode=="TUR"] <- 640
  cowcode2[isocode=="TKM"] <- 701
  cowcode2[isocode=="UGA"] <- 500
  cowcode2[isocode=="UKR"] <- 369
  cowcode2[isocode=="ARE"] <- 696
  cowcode2[isocode=="GBR"] <- 200
  cowcode2[isocode=="USA"] <- 2
  cowcode2[isocode=="URY"] <- 165
  cowcode2[isocode=="UZB"] <- 704
  cowcode2[isocode=="VUT"] <- 935
  cowcode2[isocode=="VEN"] <- 101
  cowcode2[isocode=="VNM"] <- 818
  cowcode2[isocode=="YEM"] <- 679
  cowcode2[isocode=="ZMB"] <- 551
  cowcode2[isocode=="ZWE"] <- 552
  cowcode2[cowcode2 == 255 & year <= 1989] <- 260  # Germany
  cowcode2[cowcode2 == 374 & year < 1991] <- 365 #USSR/Russia
  # Yugoslavia/Serbia/Serbia-Montenegro
  cowcode2[cowcode2 == 342 & year < 1992] <- 345
  cowcode2[cowcode2 == 342 & year >= 1992 & 
    year <= 2005] <- 347
  cowcode2[cowcode2 == 818 & year < 1976] <- 816   # Vietnam
  cowcode2[cowcode2 == 679 & year < 1990] <- 678}    # Yemen
)
pwt[which(duplicated(pwt[, c("cowcode2", "year")])), 
  c("cowcode2", "isocode", "year")]
# Missing cowcode2 for: China1, Hong Kong, Macao, 
# Puerto Rico --> not in DD, therefore exclude
pwt <- pwt[is.na(pwt$cowcode2)==FALSE, ]

# merge data -----------------------------------------------

library(Hmisc)
pwt <- upData(pwt, 
  rename=list(rgdpch = "pwt7.rgdpch", 
    grgdpch = "pwt7.grgdpch", POP = "pwt7.pop",
    kg = "pwt7.kg", rgdpl = "pwt7.rgdpl"
  )
)
detach(package:Hmisc)

library(ETLUtils)
dd <- matchmerge(
  x = dd, y = pwt,
  by.x  = c("cowcode2", "year"), by.y  = c("cowcode2", "year"),
  all.x = TRUE,
  add.columns = c("pwt7.rgdpl", "pwt7.grgdpch",
    "pwt7.rgdpch", "pwt7.pop", "pwt7.kg"
  ), 
  check.duplicates = TRUE,
  trace = TRUE
)
# --- Tidy workspace ---------------------------------------
detach(package:ETLUtils)
detach(package:ff)
detach(package:bit)
rm(pwt)
## END