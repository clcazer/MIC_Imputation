
###########################Drop MIC values that cannot be resolved####################################################
NARMSChickenCombined_cleaned<-NARMSChickenCombined 

#The powerpoint in folder "MIC_sign_tables_Steps6,19,23" shows tables looking at signs and MIC values for all antimicrobials. Below are the two instances of MIC values that could not be resolved for reference.
table(NARMSChickenCombined $AZI, NARMSChickenCombined $AZI.Sign)
#There are both 42 isolates with =16 designation and 7 isolates with >16 designation. Cannot resolve the >16 isolates (i.e. is it the same as =32 or >32?)

table(NARMSChickenCombined $FOX, NARMSChickenCombined $FOX.Sign)
#There are both 176 isolates with =16 designation and 68 isolates with >16 designation. Cannot resolve the >16 isolates (i.e. is it the same as =32 or >32?)


######Drop rows with AZI values that cannot be resolved
#These are the 7 isolates with > 16 designation
for (x in 1:nrow(NARMSChickenCombined_cleaned)) {
  col.number.AZI<-match("AZI",names(NARMSChickenCombined_cleaned)) #gives column number of AZI
  col.number.AZI.sign<-match("AZI",names(NARMSChickenCombined_cleaned))-1 #gives column number of AZI. sign
  if (is.na(NARMSChickenCombined_cleaned[x,col.number.AZI])){
    print(x)
    x=x+1 #If the value is NA, skip to next index
  }
  else if (NARMSChickenCombined_cleaned[x,col.number.AZI.sign]==">") {
    print("> sign") #if the sign is >
    if (NARMSChickenCombined_cleaned[x,col.number.AZI]=="16"){
      print("and equals 16") #and if the value is 16
      NARMSChickenCombined_cleaned<- NARMSChickenCombined_cleaned[-c(x), ] #drop this row because the MIC value cannot be resolved
    } else {
      print("data in this row is fine") #otherwise advance to next index
    }
  }
  x=x+1
}


######Drop rows with FOX values that cannot be resolved
#These are the 68 isolates with > 16 designation
for (x in 1:nrow(NARMSChickenCombined_cleaned)) {
  col.number.FOX<-match("FOX",names(NARMSChickenCombined_cleaned)) #gives column number of FOX
  col.number.FOX.sign<-match("FOX",names(NARMSChickenCombined_cleaned))-1 #gives column number of FOX sign
  if (is.na(NARMSChickenCombined_cleaned[x,col.number.FOX])){
    print(x)
    x=x+1 #if the value is NA, skip to the next index
  }
  else if (NARMSChickenCombined_cleaned[x,col.number.FOX.sign]==">") {
    print("> sign") #if the sign is >
    if (NARMSChickenCombined_cleaned[x,col.number.FOX]=="16"){
      print("and equals 16") #and the value is 16
      NARMSChickenCombined_cleaned<- NARMSChickenCombined_cleaned[-c(x), ] #drop this row because the MIC value cannot be resolved
    } else {
      print("data in this row is fine") #otherwise advance to the next index
    }
  }
  x=x+1
}

remove(col.number.AZI, col.number.AZI.sign, col.number.FOX, col.number.FOX.sign,x)


nrow(NARMSChickenCombined_cleaned) #7260 isolates in dataframe after cleaning


###########################################Make AM variables ordinal for RFs########################################################
NARMSChickenCombined_ordinal<-NARMSChickenCombined_cleaned

#Make all AMs ordinals instead of numerics
NARMSChickenCombined_ordinal$AMC<-ordered(NARMSChickenCombined_ordinal$AMC, levels= c(1, 2, 4, 8, 16, 32))

NARMSChickenCombined_ordinal$AMP<-ordered(NARMSChickenCombined_ordinal$AMP, levels= c(1, 2, 4, 8, 16, 32))

NARMSChickenCombined_ordinal$AXO<-ordered(NARMSChickenCombined_ordinal$AXO, levels= c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64))

NARMSChickenCombined_ordinal$AZI<-ordered(NARMSChickenCombined_ordinal$AZI, levels= c(0.25, 0.5, 1, 2, 4, 8, 16, 32))

NARMSChickenCombined_ordinal$CHL<-ordered(NARMSChickenCombined_ordinal$CHL, levels= c(2, 4, 8, 16, 32))

NARMSChickenCombined_ordinal$CIP<-ordered(NARMSChickenCombined_ordinal$CIP, levels= c(0.015, 0.03, 0.06, 0.125, 0.25, 0.5, 1, 2, 4))

NARMSChickenCombined_ordinal$COT<-ordered(NARMSChickenCombined_ordinal$COT, levels= c(0.125, 0.25, 0.5, 1, 2, 4))

NARMSChickenCombined_ordinal$FIS<-ordered(NARMSChickenCombined_ordinal$FIS, levels= c(16, 32, 64, 128, 256))

NARMSChickenCombined_ordinal$FOX<-ordered(NARMSChickenCombined_ordinal$FOX, levels= c(0.5, 1, 2, 4, 8, 16, 32))

NARMSChickenCombined_ordinal$GEN<-ordered(NARMSChickenCombined_ordinal$GEN, levels= c(0.25, 0.5, 1, 2, 4, 8, 16))

NARMSChickenCombined_ordinal$TET<-ordered(NARMSChickenCombined_ordinal$TET, levels= c(4, 8, 16, 32))

NARMSChickenCombined_ordinal$TIO<-ordered(NARMSChickenCombined_ordinal$TIO, levels= c(0.125, 0.25, 0.5, 1, 2, 4, 8))

NARMSChickenCombined_ordinal$NAL<-ordered(NARMSChickenCombined_ordinal$NAL, levels= c(0.5, 1, 2, 4, 8, 16, 32))

NARMSChickenCombined_ordinal$KAN<-ordered(NARMSChickenCombined_ordinal$KAN, levels= c(8, 16, 32, 64))

NARMSChickenCombined_ordinal$Str<-ordered(NARMSChickenCombined_ordinal$Str, levels= c(2,4,8,16,32, 64))

NARMSChickenCombined_ordinal$AMI<-ordered(NARMSChickenCombined_ordinal$AMI, levels= c(0.5, 1, 2, 4, 8, 16))


