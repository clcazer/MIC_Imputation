pdf("Histograms_Step3/histograms.pdf") 

#view the below histograms in the folder "Histograms_Step3"

####Graphing Method####
#Plots were made with concentration "bins" according to "Table 1: Concentration Ranges Used for Susceptibility Testing of Salmonella and E. coli" from NARMS in NARMS protocols folder
#If the AM was not listed in Table 1, concentration "bins" were determined by looking at the range of data

#Graphed such that histogram has custom breaks and consistent bin widths; gives warning however plot still shows up correctly

####AMC Histograms####
#AMC Histogram for Cecal data
binsAMC<- c(0,1,2,4,8,16,Inf)
plot(cut(CattleCecalEColi$AMC, binsAMC), ylim=c(0,5000), main="Amoxicillin-Clavulanic Acid Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#AMC Histogram for Ground Beef.... has custom breaks and consistent bin widths
binsAMC<- c(0,1,2,4,8,16,Inf) 
plot(cut(GroundBeef$AMC, binsAMC), ylim=c(0,3500),   main="Amoxicillin-Clavulanic Acid Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsAMC)

####AMI Histograms####
AMI2002_2003 <- GroundBeef %>%
  filter(Year<2004) #selects for years 2002 and 2003 when only measured concentrations 0.5,1,2,4

AMI2004_2019 <- GroundBeef %>%
  filter(Year>2003) #selects for years 2004-2019 when measured concentrations 0.5,1,2,4,8,16 or greater

#AMI Histogram for years 2004-2019 Ground Beef
binsAMI<- c(0,0.5,1,2,4,8,16, Inf) #endpoint would be infinity
plot(cut(AMI2004_2019$AMI, binsAMI), ylim=c(0,2000),   main="Amikacin Resistance in Ground Beef E.coli Isolates 2004-2019", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#AMI Histogram for years 2002-2003 Ground Beef
binsAMI<- c(0,0.5,1,2,Inf) #endpoint would be infinity
plot(cut(AMI2002_2003$AMI, binsAMI), ylim=c(0,700),   main="Amikacin Resistance in Ground Beef E.coli Isolates 2002-2003", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsAMI)
remove(AMI2002_2003)
remove(AMI2004_2019)

####AMP Histograms####
#AMP Histogram for Cecal data
binsAMP<- c(0,1,2,4,8,16,Inf)
plot(cut(CattleCecalEColi$AMP, binsAMP), ylim=c(0,3500),   main="Ampicillin Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#AMP Histogram for Ground Beef
binsAMP<- c(0,1,2,4,8,16,Inf) #endpoint would be infinity
plot(cut(GroundBeef$AMP, binsAMP), ylim=c(0,2500),   main="Ampicillin Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsAMP)

####AXO Histogram####
#AXO Histogram for Cecal data
binsAXO<- c(0,0.25,0.5,1,2,4,8,16,32,Inf)
plot(cut(CattleCecalEColi$AXO, binsAXO), ylim=c(0,7000),   main="Ceftriaxone Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#AXO Histogram for Ground Beef
binsAXO<- c(0,0.25,0.5,1,2,4,8,16,32,Inf) #endpoint would be infinity
plot(cut(GroundBeef$AXO, binsAXO), ylim=c(0,6000),   main="Ceftriaxone Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsAXO)

####AZI Histograms####
#AZI Histogram for Cecal data
binsAZI<- c(0,0.25,0.5,1,2,4,8,16,32,Inf)
plot(cut(CattleCecalEColi$AZI, binsAZI), ylim=c(0,6000),   main="Azithromycin Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#AZI Histogram for Ground Beef
binsAZI<- c(0,0.25,0.5,1,2,4,8,16,32,Inf) #endpoint would be infinity
plot(cut(GroundBeef$AZI, binsAZI), ylim=c(0,2000),   main="Azithromycin Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsAZI)

####CHL Histograms####
#CHL Histogram for Cecal data
binsCHL<- c(0,2,4,8,16,Inf)
plot(cut(CattleCecalEColi$CHL, binsCHL), ylim=c(0,6000),   main="Chloramphenicol Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#CHL Histogram for Ground Beef
binsCHL<- c(0,2,4,8,16,Inf) #endpoint would be infinity
plot(cut(GroundBeef$CHL, binsCHL), ylim=c(0,4000),   main="Chloramphenicol Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsCHL)

####CIP Histograms####
#CIP Histogram for Cecal data
binsCIP<- c(0,0.015,0.03,0.06,0.125,0.25,0.5,1,2,Inf)
plot(cut(CattleCecalEColi$CIP, binsCIP), ylim=c(0,8000),   main="Ciprofloxacin Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#CIP Histogram for Ground Beef
binsCIP<- c(0,0.015,0.03,0.06,0.125,0.25,0.5,1,2,Inf) #endpoint would be infinity
plot(cut(GroundBeef$CIP, binsCIP), ylim=c(0,5000),   main="Ciprofloxacin Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsCIP)

####COT Histograms####
#COT Histogram for Cecal data
binsCOT<- c(0,0.125,0.25,0.5,1,2,Inf)
plot(cut(CattleCecalEColi$COT, binsCOT), ylim=c(0,7000),   main="Trimethoprim-Sulfamethoxazole Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#COT Histogram for Ground Beef
binsCOT<- c(0,0.125,0.25,0.5,1,2,Inf) #endpoint would be infinity
plot(cut(GroundBeef$COT, binsCOT), ylim=c(0,5000),   main="Trimethoprim-Sulfamethoxazole Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsCOT)

####FIS Histograms####
#FIS Histogram for Cecal data
binsFIS<- c(0,16,32,64,128,Inf)
plot(cut(CattleCecalEColi$FIS, binsFIS), ylim=c(0,7000),   main="Sulfisoxazole Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#FIS Histogram for Ground Beef
binsFIS<- c(0,16,32,64,128,Inf) #endpoint would be infinity
plot(cut(GroundBeef$FIS, binsFIS), ylim=c(0,3500),   main="Sulfisoxazole Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsFIS)

####FOX Histograms####
#FOX Histogram for Cecal data
binsFOX<- c(0,0.5,1,2,4,8,16,Inf)
plot(cut(CattleCecalEColi$FOX, binsFOX), ylim=c(0,5000),   main="Cefoxitin Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#FOX Histogram for Ground Beef
binsFOX<- c(0,0.5,1,2,4,8,16,Inf) #endpoint would be infinity
plot(cut(GroundBeef$FOX, binsFOX), ylim=c(0,3500),   main="Cefoxitin Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsFOX)

####GEN Histograms####
#GEN Histogram for Cecal data
binsGEN<- c(0,0.25,0.5,1,2,4,8,Inf)
plot(cut(CattleCecalEColi$GEN, binsGEN), ylim=c(0,5000),   main="Gentamicin Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#GEN Histogram for Ground Beef
binsGEN<- c(0,0.25,0.5,1,2,4,8,Inf) #endpoint would be infinity
plot(cut(GroundBeef$GEN, binsGEN), ylim=c(0,3500),   main="Gentamicin Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsGEN)

####KAN Histograms####
#KAN Histogram for Cecal data
binsKAN<- c(0,8,16,32,Inf)
plot(cut(CattleCecalEColi$KAN, binsKAN), ylim=c(0,800),   main="Kanamycin Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#KAN Histogram for Ground Beef
binsKAN<- c(0,8,16,32,Inf) #endpoint would be infinity
plot(cut(GroundBeef$KAN, binsKAN), ylim=c(0,3500),   main="Kanamycin Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsKAN)

####MER Histograms####
#MER Histogram for Cecal data
binsMER<- c(0,0.06,0.125,0.25,0.5,1,2,Inf)
plot(cut(CattleCecalEColi$MER, binsMER), ylim=c(0,6000),   main="Meropenem Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#MER Histogram for Ground Beef
binsMER<- c(0,0.06,0.125,0.25,0.5,1,2,Inf) #endpoint would be infinity
plot(cut(GroundBeef$MER, binsMER), ylim=c(0,1000),   main="Meropenem Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsMER)

####NAL Histograms####
#NAL Histogram for Cecal data
binsNAL<- c(0,0.5,1,2,4,8,16,Inf)
plot(cut(CattleCecalEColi$NAL, binsNAL), ylim=c(0,6000),   main="Nalidixic Acid Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#NAL Histogram for Ground Beef
binsNAL<- c(0,0.5,1,2,4,8,16,Inf) #endpoint would be infinity
plot(cut(GroundBeef$NAL, binsNAL), ylim=c(0,4000),   main="Nalidixic Acid Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsNAL)

####TET Histograms####
#TET Histogram for Cecal data
binsTET<- c(0,4,8,16,Inf)
plot(cut(CattleCecalEColi$TET, binsTET), ylim=c(0,6000),   main="Tetracycline Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#TET Histogram for Ground Beef
binsTET<- c(0,4,8,16,Inf) #endpoint would be infinity
plot(cut(GroundBeef$TET, binsTET), ylim=c(0,4000),   main="Tetracycline Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsTET)

####TIO Histograms####
#TIO Histogram for Cecal data
binsTIO<- c(0,0.125,0.5,1,2,4,Inf)
plot(cut(CattleCecalEColi$TIO, binsTIO), ylim=c(0,2500),   main="Ceftiofur Resistance in Cattle Cecal E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#TIO Histogram for Ground Beef
binsTIO<- c(0,0.125,0.5,1,2,4,Inf) #endpoint would be infinity
plot(cut(GroundBeef$TIO, binsTIO), ylim=c(0,4000),   main="Ceftiofur Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsTIO)

####Str Histograms####
remove(Str2013_2019GB)#pulled wrong years out accidentally

Str2013 <- CattleCecalEColi %>%
  filter(Year<2014) #selects for year 2013 (first year started measuring Str) when only measured concentrations 32,64

Str2014_2019 <- CattleCecalEColi %>%
  filter(Year>2013) #selects for years 2014-2019 when measured concentrations 2,4,8,16,32,64 or greater

#Str Histogram for Cecal data 2013
binsStr2013<- c(0,32,Inf)
plot(cut(Str2013$Str, binsStr2013), ylim=c(0,600),   main="Streptomycin Resistance in Cattle Cecal E.coli Isolates in 2013", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#Str Histogram for Cecal data 2014-2019
binsStr2014_2019<- c(0,2,4,8,16,32,Inf)
plot(cut(Str2014_2019$Str, binsStr2014_2019), ylim=c(0,4000),   main="Streptomycin Resistance in Cattle Cecal E.coli Isolates 2014-2019", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(Str2013)
remove(Str2014_2019)
remove(binsStr2013)
remove(binsStr2014_2019)

Str2002_2012GB <- GroundBeef %>%
  filter(Year<2013) # only measured concentrations 32,64

Str2013_2019GB <- GroundBeef %>%
  filter(Year>2012) #selects for years 2014-2019 when measured concentrations 2,4,8,16,32,64 or greater

#Str Histogram for Ground Beef 2003-2013
binsStr2002_2012GB<- c(0,32,Inf)
plot(cut(Str2002_2012GB$Str, binsStr2002_2012GB), ylim=c(0,4000),   main="Streptomycin Resistance in Ground Beef E.coli Isolates in 2002-2012", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

#Str Histogram for Ground Beef 2013-2019
binsStr2013_2019GB<- c(0,2,4,8,16,32,Inf) #endpoint would be infinity
plot(cut(Str2013_2019GB$Str, binsStr2013_2019GB), ylim=c(0,1000),   main="Streptomycin Resistance in Ground Beef E.coli Isolates 2013-2019", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(Str2002_2012GB)
remove(Str2013_2019GB)
remove(binsStr2002_2012GB)
remove(binsStr2013_2019GB)
remove(binsStr2002_2013GB)
remove(binsStr2014_2019)

####SMX Histograms####
#SMX Histogram for Ground Beef
binsSMX<- c(0,16,32,64,128,256,Inf) #endpoint would be infinity
plot(cut(GroundBeef$SMX, binsSMX), ylim=c(0,600),   main="Sulfamethoxazole Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsSMX)

####CEP Histograms####
#CEP Histogram for Ground Beef
binsCEP<- c(0,2,4,8,16,Inf) #endpoint would be infinity
plot(cut(GroundBeef$CEP, binsCEP), ylim=c(0,400),   main="Cephalothin Resistance in Ground Beef E.coli Isolates", ylab="Frequency", xlab= "Minimum Inhibitory Concentration")

remove(binsCEP)

dev.off() 
