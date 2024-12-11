####Importing Data from NARMS####
NARMS_Cecal=read.csv("raw data/CVM-2019-NARMS-Cecal-Data.csv") #"raw data/ file name"
NARMS_RetailMeats=read.csv("raw data/CVM-2019-NARMS-RetailMeatsData.csv")

####Filtering Data for Chicken E. coli Isolates####

#selecting for chicken E. coli data
ChickenCecalEColi<- NARMS_Cecal %>%
  filter(HOST_SPECIES== "Chickens") %>%
  filter(SPECIES== "coli") %>%
  filter(GENUS== "EC")

#selecting for retail meat chicken E. coli data
ChickenBreast<- NARMS_RetailMeats %>%
  filter(MEAT_TYPE=="CB") %>%
  filter(SPECIES== "coli") %>%
  filter(GENUS== "EC")

####Merge NARMS Chicken Datasets####

#first add dataset source tag 
ChickenCecalEColi<-mutate(ChickenCecalEColi, dataset_source="NARMS_cecal") 
ChickenBreast<-mutate(ChickenBreast, dataset_source="NARMS_retail_meat")


compare_df_cols(ChickenCecalEColi,ChickenBreast,  return= "mismatch") #used this to see which of the 8 columns are present in ChickenBreast dataframe but not in ChickenCecalEColi

#TO JOIN TWO DATAFRAES VERTICALLY (ADD ROWS), MUST HAVE THE SAME VARIABLE NAMES. ADDING VARIABLES IN ChickenBreast NOT IN ChickenCecalEColi AND SET TO NA
ChickenCecalEColi<-mutate(ChickenCecalEColi, BAC=NA) 
ChickenCecalEColi<-mutate(ChickenCecalEColi, BAC.Sign=NA) 
ChickenCecalEColi<-mutate(ChickenCecalEColi, BRAND=NA) 
ChickenCecalEColi<-mutate(ChickenCecalEColi, BRAND_NAME=NA) 
ChickenCecalEColi<-mutate(ChickenCecalEColi, ESTABLISHMENT_NUMBER=NA) 
ChickenCecalEColi<-mutate(ChickenCecalEColi, GROWTH=NA) 
ChickenCecalEColi<-mutate(ChickenCecalEColi, PLATE=NA) 
ChickenCecalEColi<-mutate(ChickenCecalEColi, SELLBY_DATE=NA) 

#Create a combined dataframe with both cecal and ChickenBreast data
NARMSChickenCombined <- rbind(ChickenCecalEColi, ChickenBreast)

#Replace Blanks and Spaces in Dataframe with NA (looks like NA was not always applied)#
NARMSChickenCombined[NARMSChickenCombined=="" | NARMSChickenCombined == " "] <- NA

#remove NARMS datasets with other organisms from environment
remove(NARMS_RetailMeats)
remove(NARMS_Cecal)

#Now that the dataframes are merged, remove the individual cecal and ChickenBreast dataframes
remove(ChickenCecalEColi)
remove(ChickenBreast)