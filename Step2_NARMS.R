####Importing Data from NARMS####
NARMS_Cecal=read.csv("raw data/CVM-2019-NARMS-Cecal-Data.csv") #"raw data/ file name"
NARMS_RetailMeats=read.csv("raw data/CVM-2019-NARMS-RetailMeatsData.csv")


####Filtering Data for Cattle E. coli Isolates####

#selecting for cattle E. coli data
CattleCecalEColi<- NARMS_Cecal %>%
  filter(HOST_SPECIES== "Cattle") %>%
    filter(SPECIES== "coli") %>%
      filter(GENUS== "EC")

#selecting for ground beef E. coli data
GroundBeef<- NARMS_RetailMeats %>%
  filter(MEAT_TYPE=="GB") %>%
    filter(SPECIES== "coli") %>%
      filter(GENUS== "EC")

