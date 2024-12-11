#The distribution of MICs for each antimicrobial looked similar across the cattle associated E. coli isolates in the NARMS cecal and NARMS retail meats datasets in the histograms plotted in "Step2_AM_Histograms.R"
#Therefore, the cattle associated E. coli isolates in the NARMS cecal and NARMS retail meats datasets will be combined into one dataframe for building the models

#first add dataset source tag 
CattleCecalEColi<-mutate(CattleCecalEColi, dataset_source="NARMS cecal") 
GroundBeef<-mutate(GroundBeef, dataset_source="NARMS ground beef")


compare_df_cols(CattleCecalEColi,GroundBeef,  return= "all") #used this to see which of the 8 columns are present in GroundBeef dataframe but not in CattleCecalEColi

#TO JOIN TWO DATAFRAES VERTICALLY (ADD ROWS), MUST HAVE THE SAME VARIABLE NAMES. ADDING VARIABLES IN GroundBeef NOT IN CattleCecalEcoli AND SET TO NA
CattleCecalEColi<-mutate(CattleCecalEColi, BAC=NA) 
CattleCecalEColi<-mutate(CattleCecalEColi, BAC.Sign=NA) 
CattleCecalEColi<-mutate(CattleCecalEColi, BRAND=NA) 
CattleCecalEColi<-mutate(CattleCecalEColi, BRAND_NAME=NA) 
CattleCecalEColi<-mutate(CattleCecalEColi, ESTABLISHMENT_NUMBER=NA) 
CattleCecalEColi<-mutate(CattleCecalEColi, GROWTH=NA) 
CattleCecalEColi<-mutate(CattleCecalEColi, PLATE=NA) 
CattleCecalEColi<-mutate(CattleCecalEColi, SELLBY_DATE=NA) 

#Create a combined dataframe with both cecal and groundbeef data
NARMSCattleCombined <- rbind(CattleCecalEColi, GroundBeef)

#Replace Blanks and Spaces in Dataframe with NA (looks like NA was not always applied)#
NARMSCattleCombined[NARMSCattleCombined=="" | NARMSCattleCombined == " "] <- NA

#remove NARMS datasets with other organisms from environment
remove(NARMS_RetailMeats)
remove(NARMS_Cecal)

#Now that the dataframes are merged, remove the individual cecal and GroundBeef dataframes
remove(CattleCecalEColi)
remove(GroundBeef)