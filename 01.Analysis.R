library(stringr)
library(ape)
library(openxlsx)
source("Functions.R")
nameSet<-c(
  # Healthy_Crest
  "HC1",
  "HC2",
  "HC3",
  "HC4",
  # Healthy_Apex
  "HA1",
  "HA2",
  "HA3",
  "HA4",
  # Supercont_Crest
  "SC1",
  "SC2",
  "SC3",
  "SC4",
  # Supercont_Apex
  "SA1",
  "SA2",
  "SA3",
  "SA4",
  # MRONJ_Crest
   "MC1",
   "MC2",
   "MC3",
   "MC4",
  # MRONJ_Apex
   "MA1",
   "MA2",
   "MA4",
   "MA5",
  # BMP2_Crest
   "BMP2_C1",
   "BMP2_C2",
   "BMP2_C3",
   "BMP2_C4",
  # BMP2_Apex
   "BMP2_A1",
   "BMP2_A2",
   "BMP2_A4",
   "BMP2_A5"
  )
TotalVolume<-c( # geometry data
  128*274*16.0, # HC1
  180*250*22.0, # HC2
  270*149*33.0, # HC3
  118*97.0*33.0, # HC4
  304*227*20.0, # HA1
  249*164*33.0, # HA2
  91.7*99.9*33.0, # HA3
  107*89.2*43.0, # HA4
  99.1*106*23.0, # SC1
  94.6*81.4*25.0, # SC2
  127*83.3*26.0, # SC3
  1001*248*21.0, # SC4
  88.6*84.6*29.0, # SA1
  516*507*21.0, # SA2
  128*123*28.0, # SA3
  130*115*26.0, # SA4
  454*639*20.0, # MC1
  964*587*19.0, # MC2
  64.6*94.4*19, # MC3
  118*104*30.0, # MC4
  285*289*27.0, # MA1
  135*135*23.0, # MA2
  101*108*16.0, # MA4
  127*108*25.0, # MA5
  363*140*24.0, # BMP2_C1
  115*105*18.0, # BMP2_C2
  180*173*25.0, # BMP2_C3
  455*280*16.0, # BMP2_C4
  285*299*32.0, # BMP2_A1
  385*534*41.0, # BMP2_A2
  135*81.4*27.0, # BMP2_A4
  132*117*27 # BMP2_A5
  )
GroupID=c(
  "Healthy_Crest", 
  "Healthy_Apex",
  "Supercont_Crest",
  "Supercont_Apex",
  "MRONJ_Crest",
  "MRONJ_Apex",
  "BMP2_Crest",
  "BMP2_Apex"
) # group name
GroupReplicates<-c(
  4, # Healthy_Crest
  4, # Healthy_Apex
  4, # Supercont_Crest
  4, # Supercont_Apex
  4, # MRONJ_Crest
  4, # MRONJ_Apex
  4, # BMP2_Crest
  4  # BMP2_Apex
) # Sample size for each group

suffixC<-"_Cut" # Cut
suffixF<-"_Fill" # Fill
suffixN<-"_Nc" # Surface

Results<-Analysis.Osteocyte.Processes(nameSet=nameSet, TotalVolume=TotalVolume, NecleusLarger=1.1)
ExcelXlsxOut(Results)


