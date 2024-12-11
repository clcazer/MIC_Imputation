
###########################Drop MIC values that cannot be resolved####################################################
NARMSCattleCombined_cleaned<-NARMSCattleCombined 

#The powerpoint in folder "MIC_sign_tables_Steps6,19,23" shows tables looking at signs and MIC values for all antimicrobials. Below are the two instances of MIC values that could not be resolved for reference.
table(NARMSCattleCombined$AZI, NARMSCattleCombined$AZI.Sign)
#There are both 46 isolates with =16 designation and 5 isolates with >16 designation. Cannot resolve the >16 isolates (i.e. is it the same as =32 or >32?)

table(NARMSCattleCombined$FOX, NARMSCattleCombined$FOX.Sign)
#There are both 132 isolates with =16 designation and 5 isolates with >16 designation. Cannot resolve the >16 isolates (i.e. is it the same as =32 or >32?)

######Drop rows with AZI values that cannot be resolved
          #These are the 5 isolates with > 16 designation
for (x in 1:nrow(NARMSCattleCombined_cleaned)) {
  col.number.AZI<-match("AZI",names(NARMSCattleCombined_cleaned)) #gives column number of AZI
  col.number.AZI.sign<-match("AZI",names(NARMSCattleCombined_cleaned))-1 #gives column number of AZI. sign
  if (is.na(NARMSCattleCombined_cleaned[x,col.number.AZI])){
    print(x)
    x=x+1 #If the value is NA, skip to next index
  }
  else if (NARMSCattleCombined_cleaned[x,col.number.AZI.sign]==">") {
    print("> sign") #if the sign is >
    if (NARMSCattleCombined_cleaned[x,col.number.AZI]=="16"){
      print("and equals 16") #and if the value is 16
      NARMSCattleCombined_cleaned<- NARMSCattleCombined_cleaned[-c(x), ] #drop this row because the MIC value cannot be resolved
    } else {
      print("data in this row is fine") #otherwise advance to next index
    }
  }
  x=x+1
}
    

######Drop rows with FOX values that cannot be resolved
#These are the 5 isolates with > 16 designation
for (x in 1:nrow(NARMSCattleCombined_cleaned)) {
  print(x)
  col.number.FOX<-match("FOX",names(NARMSCattleCombined_cleaned)) #gives column number of FOX
  col.number.FOX.sign<-match("FOX",names(NARMSCattleCombined_cleaned))-1 #gives column number of FOX sign
  if (is.na(NARMSCattleCombined_cleaned[x,col.number.FOX])){
    print(x)
    x=x+1 #if the value is NA, skip to the next index
  }
  else if (NARMSCattleCombined_cleaned[x,col.number.FOX.sign]==">") {
    print("> sign") #if the sign is >
    if (NARMSCattleCombined_cleaned[x,col.number.FOX]=="16"){
      print("and equals 16") #and the value is 16
      NARMSCattleCombined_cleaned<- NARMSCattleCombined_cleaned[-c(x), ] #drop this row because the MIC value cannot be resolved
    } else {
      print("data in this row is fine") #otherwise advance to the next index
    }
  }
  x=x+1
}

remove(col.number.AZI, col.number.AZI.sign, col.number.FOX, col.number.FOX.sign,x)


nrow(NARMSCattleCombined_cleaned) #11713 isolates in dataframe after cleaning


###########################################Make AM variables ordinal for building RFs########################################################
NARMSCattleCombined_ordinal<-NARMSCattleCombined_cleaned

#Make all AMs ordinals instead of numerics
NARMSCattleCombined_ordinal$AMC<-ordered(NARMSCattleCombined_ordinal$AMC, levels= c(1, 2, 4, 8, 16, 32))

NARMSCattleCombined_ordinal$AMP<-ordered(NARMSCattleCombined_ordinal$AMP, levels= c(1, 2, 4, 8, 16, 32))

NARMSCattleCombined_ordinal$AXO<-ordered(NARMSCattleCombined_ordinal$AXO, levels= c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64))

NARMSCattleCombined_ordinal$AZI<-ordered(NARMSCattleCombined_ordinal$AZI, levels= c(0.25, 0.5, 1, 2, 4, 8, 16, 32))

NARMSCattleCombined_ordinal$CHL<-ordered(NARMSCattleCombined_ordinal$CHL, levels= c(2, 4, 8, 16, 32))

NARMSCattleCombined_ordinal$CIP<-ordered(NARMSCattleCombined_ordinal$CIP, levels= c(0.015, 0.03, 0.06, 0.125, 0.25, 0.5, 1, 2, 4))

NARMSCattleCombined_ordinal$COT<-ordered(NARMSCattleCombined_ordinal$COT, levels= c(0.125, 0.25, 0.5, 1, 2, 4))

NARMSCattleCombined_ordinal$FIS<-ordered(NARMSCattleCombined_ordinal$FIS, levels= c(16, 32, 64, 128, 256))

NARMSCattleCombined_ordinal$FOX<-ordered(NARMSCattleCombined_ordinal$FOX, levels= c(0.5, 1, 2, 4, 8, 16, 32))

NARMSCattleCombined_ordinal$GEN<-ordered(NARMSCattleCombined_ordinal$GEN, levels= c(0.25, 0.5, 1, 2, 4, 8, 16))

NARMSCattleCombined_ordinal$TET<-ordered(NARMSCattleCombined_ordinal$TET, levels= c(4, 8, 16, 32))

NARMSCattleCombined_ordinal$TIO<-ordered(NARMSCattleCombined_ordinal$TIO, levels= c(0.125, 0.25, 0.5, 1, 2, 4, 8))

NARMSCattleCombined_ordinal$NAL<-ordered(NARMSCattleCombined_ordinal$NAL, levels= c(0.5, 1, 2, 4, 8, 16, 32))

NARMSCattleCombined_ordinal$KAN<-ordered(NARMSCattleCombined_ordinal$KAN, levels= c(8, 16, 32, 64))

NARMSCattleCombined_ordinal$Str<-ordered(NARMSCattleCombined_ordinal$Str, levels= c(2,4,8,16,32, 64))

NARMSCattleCombined_ordinal$AMI<-ordered(NARMSCattleCombined_ordinal$AMI, levels= c(0.5, 1, 2, 4, 8, 16))
