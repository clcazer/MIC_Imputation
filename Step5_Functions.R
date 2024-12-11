####Function for calculating +/- 1 MIC category accuracy from a confusion matrix####
#input a confusion matrix as the argument
#create the confusion matrix by using table(dataframe$varaiable, predict(RFname))

PlusorMinus1_ConMat_Accuracy<- function(ConMat) {
  print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  list_MIC_Category<- list() #empty list
  list_accuracy<-list() #empty list
  for (x in 1:ncol(ConMat)) {
    j=x-1 #j is the column number corresponding to 1 MIC category below
    k=x+1 #k is the column number corresponding to 1 MIC category above
    if (x==1)
      total_accurate<-ConMat[x,x]+ConMat[k,x]
    else if (x==ncol(ConMat))
      total_accurate<-ConMat[x,x]+ConMat[j,x]
    else
      total_accurate<-ConMat[x,x]+ConMat[j,x]+ ConMat[k,x]
    
    percent_accurate<-total_accurate/sum(ConMat[,x])
    list_accuracy[length(list_accuracy)+1]<-percent_accurate
    list_MIC_Category[length(list_MIC_Category)+1]<-colnames(ConMatdf)[x]
    x=x+1
  }
  #export result to dataframe
  accuracy_test<-data.frame(unlist(list_MIC_Category),unlist(list_accuracy))           #creates dataframe out of 2 lists
  #to name tthe columns use names() function
  names(accuracy_test)<- c("MIC isolate category","Proportion_of_Isolates")
  View(accuracy_test)
  
  remove(percent_accurate)
  remove(list_accuracy)
  remove(list_MIC_Category)
  remove(j)
  remove(k)
  remove(x)
  remove(total_accurate)
 }


####Function for calculating OVERALL +/- 1 MIC category accuracy from a confusion matrix####
#input a confusion matrix as the argument
#create the confusion matrix by using table(dataframe$varaiable, predict(RFname))

PlusorMinus1_overall<- function(ConMat) {
  print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  middle_accurate<-0
  for (x in 1:ncol(ConMat)) {
    j=x-1
    k=x+1
    if (x==1){
      start_accurate<-ConMat[x,x]+ConMat[k,x]
    }
    else if (x==ncol(ConMat)){
      end_accurate<-ConMat[x,x]+ConMat[j,x]
    }
    else {
      middle_accurate<-middle_accurate+ ConMat[x,x]+ConMat[j,x]+ ConMat[k,x]
      }
    x=x+1
  }

  total_accurate<- start_accurate+end_accurate+middle_accurate
  overall_accuracy<-total_accurate/sum(ConMat)*100
  
  print("The overall +/- 1 MIC category accuracy is")
  cat(overall_accuracy, "%")
  
  
  remove(start_accurate, end_accurate, middle_accurate)
  remove(j)
  remove(k)
  remove(x)
  remove(total_accurate, overall_accuracy)
}


####Function for calculating the exact MIC category accuracy####

ConMat_Accuracy<- function(ConMat) {
  print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  list_MIC_Category<- list() #empty list
  list_accuracy<-list() #empty list
  for (x in 1:ncol(ConMat)) {
    total_accurate<-ConMat[x,x]
    percent_accurate<-total_accurate/sum(ConMat[,x])
    list_accuracy[length(list_accuracy)+1]<-percent_accurate
    list_MIC_Category[length(list_MIC_Category)+1]<-colnames(ConMatdf)[x]
    x=x+1
  }
  #export result to dataframe
  accuracy_cat<-data.frame(unlist(list_MIC_Category),unlist(list_accuracy)) #creates dataframe out of 2 lists
  #to name tthe columns use names() function
  names(accuracy_cat)<- c("MIC isolate category","Proportion_of_Isolates")
  View(accuracy_cat)
  
  remove(percent_accurate)
  remove(list_accuracy)
  remove(list_MIC_Category)
  remove(x)
  remove(total_accurate)
}


####Function for calculating the positive predictive value of a model for a given MIC category####

PositivePredictiveValuebyMIC<- function(ConMat) {
  print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  list_MIC_Category<- list() #empty list
  list_PPV<-list() #empty list
  for (x in 1:nrow(ConMat)) {
    total_accurate<-ConMat[x,x]
    percent_accurate<-total_accurate/sum(ConMat[x, ])
    list_PPV[length(list_PPV)+1]<-percent_accurate
    list_MIC_Category[length(list_MIC_Category)+1]<-rownames(ConMatdf)[x]
    x=x+1
  }
  #export result to dataframe
  PPV_cat<-data.frame(unlist(list_MIC_Category),unlist(list_PPV)) #creates dataframe out of 2 lists
  #to name tthe columns use names() function
  names(PPV_cat)<- c("MIC isolate category","Positive Predictive Value")
  View(PPV_cat)
  
  remove(percent_accurate)
  remove(list_PPV)
  remove(list_MIC_Category)
  remove(x)
  remove(total_accurate)
}


####Function for calculating Susceptible vs Resistant Accuracy Calculation####
#enter breakpoint as "#", i.e. if the susceptible breakpoint is less than or equal to 16, enter "16" in quotes
#ConMat is a confusion matrix created by using table(predict(RFname),dataframe$variable) or using ConfusionMatrix() function
BreakpointClassificationAccuracy<- function(ConMat,breakpoint) {
  print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  Sbreakpoint_columnNum<-(match(breakpoint,names(ConMatdf)))
  Rbreakpoint_columnNum<-Sbreakpoint_columnNum + 1
  total_accurate<-0
  total_R<-0
  
  for (x in Rbreakpoint_columnNum:ncol(ConMat)) {
    for (y in Rbreakpoint_columnNum:ncol(ConMat)){
      total_accurate<-total_accurate+ConMat[x,y]
      y=y+1
    }
    total_R<-total_R + (sum(ConMat[,x]))
    x=x+1
  }
  
  ResistantIsolateAccuracy<- (total_accurate/total_R)*100
  print("The accuracy of the model in predicting a resistant isolate as resistant is")
  print(ResistantIsolateAccuracy)
  
  total_accurate<-0
  total_S<-0
  
  for (j in 1:Sbreakpoint_columnNum) {
    for (k in 1:Sbreakpoint_columnNum) {
      total_accurate<-total_accurate+ConMat[j,k]
      k=k+1
    }
    total_S<-total_S + (sum(ConMat[,j]))
    j=j+1
  }

  SusceptibleIsolateAccuracy<- (total_accurate/total_S)*100
  print("The accuracy of the model in predicting a susceptible isolate as susceptible is")
  print(SusceptibleIsolateAccuracy)
}


####Missing Data Analysis for Antimicrobials####
#For each AM, what percentage of total number of isolates were tested against that AM
#For a given AM, returns percent of 7176 isolates that were tested against that AM
MissingDataAnalysis_AMs<- function(NARMSdataframe) {
list_PercentageCompletion<- list() #empty list
list_AM_Names<-list() #empty list
#The 20:98 corresponds to the columns in the NARMSCattleCombined dataframes with the antimicrobial MIC values. Please change accordingly to your dataframe
for (x in 20:98){
  if (x%%2==0) #selects only even columns (have AMR data), odd column is AMR sign
  {PercentageAM<-(100-(sum(is.na(NARMSdataframe[x]))/nrow(NARMSdataframe)*100)) #calculates percent of isolates tested that have data/are not blank (100- %NA columns)
  print(colnames(NARMSdataframe[x]))
  print (PercentageAM)
  list_PercentageCompletion[[length(list_PercentageCompletion)+1]]<-PercentageAM #adds PercentageAM calculated above to list
  list_AM_Names[[length(list_AM_Names)+1]]<-colnames(NARMSdataframe[x])} #adds the corresponding AM name to a list
  else
    x=x+1
}
#imports results into a dataframe
AM_tested<-data.frame(unlist(list_AM_Names),unlist(list_PercentageCompletion)) #creates dataframe out of 2 lists
#to name tthe columns use names() function
names(AM_tested) = c("AM_Name","Percent_of_Isolates_Tested")

#Remove dummy variables created from environment
remove(list_PercentageCompletion)
remove(list_AM_Names)
remove(PercentageAM)
remove(x)


#view results
View(AM_tested)
}


####Missing Data Analysis for Categorical Variables####
#For each categorical variable, what percentage of the total number of isolates were tested against that categorical variable

MissingDataAnalysis_CategoricalVar<- function(NARMSdataframe) {
CategoricalVariables<- list("STATE","Month","Year", "dataset_source", "RAISING_CLAIM", "SOURCE_SPECIES_INFO", "SOURCE", "GROWTH", "ACQUISITION_DATE", "COUNTRY_OF_ORIGIN", "BRAND", "BRAND_NAME", "GENOTYPE", "HOST_SPECIES", "CUTS", "MEAT_TYPE")

list_PercentageCompletion<- list() #empty list
list_Cat_Names<-list() #empty list
for (varName in CategoricalVariables){
  {PercentageCategorical<-(100-(sum(is.na(NARMSdataframe[varName]))/nrow(NARMSdataframe)*100)) #calculates percent of isolates tested that have data/are not blank (100- %NA columns)
  print(varName) #this prints the variable names in the list CategoricalVariables
  print (PercentageCategorical) #this prints the percentage of isolates with data for the given variable
  list_PercentageCompletion[[length(list_PercentageCompletion)+1]]<-PercentageCategorical #adds PercentageCat calculated above to list
  list_Cat_Names[[length(list_Cat_Names)+1]]<-colnames(NARMSdataframe[varName])} #adds the corresponding column name (variable name) to a list
}
#imports results into a dataframe
MissingDataAnalysisCategoricalVar<-data.frame(unlist(list_Cat_Names),unlist(list_PercentageCompletion)) #creates dataframe out of 2 lists
#to name tthe columns use names() function
names(MissingDataAnalysisCategoricalVar) = c("Variable Name","Percent_of_Isolates_Tested")

#Remove dummy variables created from environment
remove(list_PercentageCompletion)
remove(list_Cat_Names)
remove(PercentageCategorical)
remove(varName)
remove(CategoricalVariables)

#view results
View(MissingDataAnalysisCategoricalVar)
}


####Very Major Error and Major Error####

#Very Major Error corresponds to an isolate with a lab-derived resistant MIC label being classified with a susceptible or intermediate MIC value by the model
#Major Error corresponds to an isolate with a lab-derived susceptible or intermediate MIC label being classified with a resistant MIC value by the model

#Please note that the function was mistakenly named MajorandMinorError, but the correct terminology is Very Major Error and Major Error defined above. The output of this function uses the correct terms

MajorandMinorError<- function(ConMat,breakpoint) {
  print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  Sbreakpoint_columnNum<-(match(breakpoint,names(ConMatdf)))
  Rbreakpoint_columnNum<-Sbreakpoint_columnNum + 1
  
  total_minor_error<-0
  total_S<-0
  
  for (x in 1:Sbreakpoint_columnNum) {
    for (y in Rbreakpoint_columnNum:nrow(ConMat)){
      total_minor_error<-total_minor_error+ConMat[y,x]
      y=y+1
    }
    total_S<-total_S + (sum(ConMat[,x]))
    x=x+1
  }

  MinorErrorRate<- (total_minor_error/total_S)*100
  print("Major Error: The probability of the model in predicting a susceptible or intermediate isolate as resistant is")
  cat(MinorErrorRate, "%") #cat function allows you to print multiple arguments on same line
  
  total_major_error<-0
  total_R<-0
  
  for (j in Rbreakpoint_columnNum:ncol(ConMat)) {
    for (k in 1:Sbreakpoint_columnNum) {
      total_major_error<-total_major_error+ConMat[k,j]
      k=k+1
    }
    total_R<-total_R + (sum(ConMat[,j]))
    j=j+1
  }

  MajorErrorRate<- (total_major_error/total_R)*100
  print("Very Major Error: The probability of the model in predicting a resistant isolate as susceptible or intermediate is")
  cat(MajorErrorRate, "%") #cat function allows you to print multiple arguments on the same line
}

####Plus or Minus 2 category accuracy######
PlusorMinus2_ConMat_Accuracy<- function(ConMat) {
  print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  list_MIC_Category<- list() #empty list
  list_accuracy<-list() #empty list
  for (x in 1:ncol(ConMat)) {
    j=x-1
    k=x+1
    h=x-2
    l=x+2
    if (x==1)
      total_accurate<-ConMat[x,x]+ConMat[k,x]+ConMat[l,x]
    else if (x==2)
      total_accurate<-ConMat[x,x]+ConMat[j,x]+ ConMat[k,x]+ ConMat[l,x]
    else if (x==ncol(ConMat))
      total_accurate<-ConMat[x,x]+ConMat[j,x]+ConMat[h,x]
    else if (x==(ncol(ConMat)-1))
      total_accurate<-ConMat[x,x]+ConMat[j,x]+ ConMat[k,x]+ ConMat[h,x]
    else
      total_accurate<-ConMat[x,x]+ConMat[j,x]+ ConMat[k,x]+ ConMat[h,x]+ ConMat[l,x]
    
    percent_accurate<-total_accurate/sum(ConMat[,x])
    list_accuracy[length(list_accuracy)+1]<-percent_accurate
    list_MIC_Category[length(list_MIC_Category)+1]<-colnames(ConMatdf)[x]
    x=x+1
  }
  #export result to dataframe
  accuracy_plusminus2<-data.frame(unlist(list_MIC_Category),unlist(list_accuracy))           #creates dataframe out of 2 lists
  #to name tthe columns use names() function
  names(accuracy_plusminus2)<- c("MIC isolate category","Proportion_of_Isolates")
  View(accuracy_plusminus2)
  
  remove(percent_accurate)
  remove(list_accuracy)
  remove(list_MIC_Category)
  remove(j)
  remove(k)
  remove(x)
  remove(total_accurate)
}

####Function for calculating OVERALL +/- 2 MIC category accuracy from a confusion matrix####
#input a confusion matrix as the argument
#create the confusion matrix by using table(dataframe$varaiable, predict(RFname))

PlusorMinus2_overall<- function(ConMat) {
  print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  middle_accurate<-0
  for (x in 1:ncol(ConMat)) {
    j=x-1
    k=x+1
    h=x-2
    l=x+2
    if (x==1){
      start_accurate<-ConMat[x,x]+ConMat[k,x]+ConMat[l,x]
    }
    else if (x==2){
      two_accurate<-ConMat[x,x]+ConMat[j,x]+ ConMat[k,x]+ ConMat[l,x]
    }
    else if (x==ncol(ConMat)){
      end_accurate<-ConMat[x,x]+ConMat[j,x]+ConMat[h,x]
    }
    else if (x==(ncol(ConMat)-1)){
      penult_accurate<-ConMat[x,x]+ConMat[j,x]+ ConMat[k,x]+ ConMat[h,x]
    }
    else {
      middle_accurate<-middle_accurate+ ConMat[x,x]+ConMat[j,x]+ ConMat[k,x]+ ConMat[h,x]+ ConMat[l,x]
    }
    x=x+1
  }
  
  total_accurate<- start_accurate+end_accurate+middle_accurate+penult_accurate+two_accurate
  overall_accuracy<-total_accurate/sum(ConMat)*100
  
  print("The overall +/- 2 MIC category accuracy is")
  cat(overall_accuracy, "%")
  
  
  remove(start_accurate, end_accurate, middle_accurate)
  remove(j)
  remove(k)
  remove(x)
  remove(total_accurate, overall_accuracy)
}

############Overall Plus or Minus 1 for Table Input##############
table_PlusorMinus1_overall<- function(ConMat) {
  #print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  middle_accurate<-0
  for (x in 1:ncol(ConMat)) {
    j=x-1
    k=x+1
    if (x==1){
      start_accurate<-ConMat[x,x]+ConMat[k,x]
    }
    else if (x==ncol(ConMat)){
      end_accurate<-ConMat[x,x]+ConMat[j,x]
    }
    else {
      middle_accurate<-middle_accurate+ ConMat[x,x]+ConMat[j,x]+ ConMat[k,x]
      #cat("middle accurate", middle_accurate) 
    }
    x=x+1
  }
  
  total_accurate<- start_accurate+end_accurate+middle_accurate
  overall_accuracy<-total_accurate/sum(ConMat)*100
  
  return(overall_accuracy)
  
  
  remove(start_accurate, end_accurate, middle_accurate)
  remove(j)
  remove(k)
  remove(x)
  remove(total_accurate, overall_accuracy)
}

#############Resistant Sensitivity function for table############

#enter breakpoint as "#", i.e. if the susceptible breakpoint is less than or equal to 16, enter "16" in quotes
#ConMat is a confusion matrix created by using table(predict(RFname),dataframe$variable) or using ConfusionMatrix() function
table_resistant_sensitivity<- function(ConMat,breakpoint) {
  #print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  Sbreakpoint_columnNum<-(match(breakpoint,names(ConMatdf)))
  Rbreakpoint_columnNum<-Sbreakpoint_columnNum + 1
  total_accurate<-0
  total_R<-0
  
  for (x in Rbreakpoint_columnNum:ncol(ConMat)) {
    for (y in Rbreakpoint_columnNum:ncol(ConMat)){
      total_accurate<-total_accurate+ConMat[x,y]
      y=y+1
    }
    total_R<-total_R + (sum(ConMat[,x]))
    x=x+1
  }

  ResistantIsolateAccuracy<- (total_accurate/total_R)*100
  return(round(ResistantIsolateAccuracy,0))
}

#########Sensitive Sensitivity for Table#################
#enter breakpoint as "#", i.e. if the susceptible breakpoint is less than or equal to 16, enter "16" in quotes
#ConMat is a confusion matrix created by using table(predict(RFname),dataframe$variable) or using ConfusionMatrix() function
table_susceptible_sensitivity<- function(ConMat,breakpoint) {
  #print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  Sbreakpoint_columnNum<-(match(breakpoint,names(ConMatdf)))
  Rbreakpoint_columnNum<-Sbreakpoint_columnNum + 1
  total_accurate<-0
  total_R<-0
  total_accurate<-0
  total_S<-0
  
  for (j in 1:Sbreakpoint_columnNum) {
    for (k in 1:Sbreakpoint_columnNum) {
      total_accurate<-total_accurate+ConMat[j,k]
      k=k+1
    }
    total_S<-total_S + (sum(ConMat[,j]))
    j=j+1
  }

  SusceptibleIsolateAccuracy<- (total_accurate/total_S)*100
  #print("The accuracy of the model in predicting a susceptible isolate as susceptible is")
  ifelse(round(SusceptibleIsolateAccuracy,0)==100, round((SusceptibleIsolateAccuracy-1),0), round(SusceptibleIsolateAccuracy,0))
}

############Very Major Error for Table###############
table_VeryMajorError<- function(ConMat,breakpoint) {
  #print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  Sbreakpoint_columnNum<-(match(breakpoint,names(ConMatdf)))
  Rbreakpoint_columnNum<-Sbreakpoint_columnNum + 1
  
  total_major_error<-0
  total_R<-0
  
  for (j in Rbreakpoint_columnNum:ncol(ConMat)) {
    for (k in 1:Sbreakpoint_columnNum) {
      total_major_error<-total_major_error+ConMat[k,j]
      k=k+1
    }
    total_R<-total_R + (sum(ConMat[,j]))
    j=j+1
  }

  MajorErrorRate<- (total_major_error/total_R)*100
  #print("Very Major Error: The probability of the model in predicting a resistant isolate as susceptible is")
  return(round(MajorErrorRate,0))
}

##################Major Error for Table####################
table_MajorError<- function(ConMat,breakpoint) {
  #print("this function requires the confusion matrix to be organized such that the predictions are the rows and actual values are the columns!!!")
  ConMatdf<-as.data.frame.array(ConMat)
  Sbreakpoint_columnNum<-(match(breakpoint,names(ConMatdf)))
  Rbreakpoint_columnNum<-Sbreakpoint_columnNum + 1
  
  total_minor_error<-0
  total_S<-0
  
  for (x in 1:Sbreakpoint_columnNum) {
    for (y in Rbreakpoint_columnNum:nrow(ConMat)){
      total_minor_error<-total_minor_error+ConMat[y,x]
      y=y+1
    }
    total_S<-total_S + (sum(ConMat[,x]))
    x=x+1
  }

  MinorErrorRate<- (total_minor_error/total_S)*100
  #print("Major Error: The probability of the model in predicting a susceptible isolate as resistant is")
  
  if (MinorErrorRate==0){
    return(round(MinorErrorRate,0))
  }
  if (MinorErrorRate<1){
    return(round(MinorErrorRate,2))
  }
  else{
    return(round(MinorErrorRate,0))
  }
}

