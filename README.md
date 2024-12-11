Antimicrobial minimum inhibitory concentrations can be imputed from phenotypic data using a random forest approach
GitHub Repository name: MIC_Imputation
Authors: Gayatri Anil, Joshua Glass, Abdolreza Mosaddegh, Casey Cazer
Date: December 11, 2024

License: https://creativecommons.org/licenses/by/4.0/

Computing Environment Details:
-Operating System: Windows 11
-Number of CPUs/Cores: 14 cores, intel(R) Core(TM) i7-1370P   1.90 GHz laptop
-Statistical Software Used: R version: 4.3.2, R studio version: 2024.09.1
-Time to complete computing in this environment: ~19 minutes

####################################################################################################
Data:
-See folder named "raw data"
-Both .xlsx and .csv versions of data are in this folder
-The data used in this study were created by the National Antimicrobial Resistance Monitoring System for Enteric Bacteria (NARMS). The data was downloaded from NARMS as .xlsx and saved as .csv for use in R. 
-Link to download data: https://www.fda.gov/animal-veterinary/national-antimicrobial-resistance-monitoring-system/integrated-reportssummaries
-Contact information for NARMS: NARMS@fda.hhs.gov

The data that support the findings of this study are freely available from the National Antimicrobial Resistance Monitoring System at https://www.fda.gov/animal-veterinary/national-antimicrobial-resistance-monitoring-system/integrated-reportssummaries. Downloaded copies of the data used in this study are present in the folder titled "raw data" in this reproducibility package. 

Food Producing Animals Cecal Data:
"CVM-2019-NARMS-Cecal-Data.xlsx": this data is from NARMS, downloaded January 9, 2023
"CVM-2019-NARMS-Cecal-Data.csv": this data is from NARMS, csv version of above file

Retail Meats Data:
"CVM-2019-NARMS-RetailMeatsData.xlsx": this data is from NARMS, downloaded January 9, 2023
"CVM-2019-NARMS-RetailMeatsData.csv": this data is from NARMS, csv version of above file

Data Dictionary from NARMS:
"CVM-2019-NARMS-DataDictionary.xlsx": this data is from NARMS, downloaded January 9, 2023

#################################################################################################################
To run code: 

-Please begin by opening the R project file "Final RF AJVR Scripts.Rproj"

-We have used the renv() package to preserve the versions of the various packages used in this project. 

-Then, please run the scripts in the listed order. For .R scripts, you can highlight all text in the script using ctrl+a and press RUN to run the entire script. For .qmd scripts, please move your cursor to the first line, click the dropdown option near RUN, and then select RUN all code chunks below.

Scripts:

##########Code Cleaning##############################
1. "Install_Packages_for_RF_Models.R": this loads the required packages and libraries for the following scripts

2. "NARMS.R": imports the data from NARMS and sorts it for the Cattle E.coli isolates

3. "AM_Histograms.R": plots histograms showing the MIC distribution of ground beef and cecal isolates for each antimicrobial. MIC distributions were similar so two datasets were combined in script 3 to make one larger dataframe.
-Histograms generated can be viewed in the folder "Histograms_Step3"

4. "Merge NARMS Cattle datasets.R": since the cecal and ground beef isolates had similar MIC distributions for each antimicrobial, these isolates were combined into one cattle E. coli dataframe on which all further analyses were conducted on

5. "Functions.R": functions were written for analyses to be used multiple times, like missing data analyses for antimicrobial and categorical variables and accuracy metrics

6. "AccountForSigninMICValues.R": for the antimicrobials measured on cattle isolates (see PowerPoint in folder "MIC_sign_tables_Steps6,19,23": "Look at Breakpoint Signs in NARMS data- white slides"), MIC values that could not be resolved due to changes in susceptibility testing protocols were dropped and antimicrobial predictor variables were set as ordinal factors

7. "MIC_Category_Merge.qmd": looks at the distribution of the number of isolates/observations in each MIC category for each AM and combines categories if needed to improve balance


###################Build RF Models####################################
The following scripts (scripts 8-17) create a random forest model for each of the ten antimicrobials tested and compute accuracy statistics for each model. Three different models were tried: building a RF on the original training data, building a RF on oversampled data so that there were the same number of observations in each MIC category, and building a RF model on training data that was balanced using a mix of oversampling and undersampling to obtain roughly the same number of observations in each MIC category. The C.perc proportions used for the mix were chosen such that the most common MIC value was undersampled and least common MIC values were oversampled to all finally have approximately the same number of observations in each category. The best performing model of the three methods is labeled as "final_modelname" and is listed last in the script. Thus, the accuracy statistics saved are those for the best performing model. The hyperparameter mtry was chosen by the model using its default method of being equal to approximately the square root of the number of predictors. ntree was set at =500 trees (the default in many studies) because we found that that was a high enough number of trees that adding more ntrees did not increase accuracy.

8. "Model_for_Each_CLSI_Class_Amikacin.qmd"
9. "Model_for_Each_CLSI_Class_amoxicillin-clavulanic_acid.qmd"
10. "Model_for_Each_CLSI_Class_Ampicillin.qmd"
11. "Model_for_Each_CLSI_Class_Cefoxitin.qmd"
12. "Model_for_Each_CLSI_Class_Ceftiofur.qmd"
13. "Model_for_Each_CLSI_Class_Chloramphenicol.qmd"
14. "Model_for_Each_CLSI_Class_Ciprofloxacin.qmd"
15. "Model_for_Each_CLSI_Class_Sulfisoxazole.qmd"
16. "Model_for_Each_CLSI_Class_Tetracycline.qmd"
17. "Model_for_Each_CLSI_Class_Trimethoprim-sulfamethoxazole.qmd"


#############Chicken Validation Scripts Order:#####################################
18. "Inport_NARMS_Chicken_Data.R": imports the Chicken E. coli data from NARMS cecal and retail meat and merges two datasets together to make one larger dataframe of chicken isolates

19.. "AccountForSigninMICValues_ChickenDataCleaning.R": for the antimicrobials measured (see PowerPoint in folder "MIC_sign_tables_Steps6,19,23":"Look at Breakpoint Signs in NARMS data"- yellow slides, MIC values that could not be resolved in chicken data due to changes in susceptibility testing protocols were dropped. Also antimicrobial predictor variables were set as ordinal factors with levels the same as in cattle data.

20. "MIC_Category_Merge_ChickenData.qmd": combines AM categories to match those used in cattle data; see "MIC_Category_Merge.qmd"

21. "RF_Validation_on_Chicken_Data.qmd": tests the random forest models created in scripts 8-17 on NARMS chicken data and computes its accuracy


##############Human Clinical Cases Validation Scripts Order:###########################
22. "Import_NARMS_Human_Data.R" imports the human E. coli 0157:H7 data from NARMS human clinical cases data

23. "AccountForSigninMICValues_HumanDataCleaning2.qmd" for the antimicrobials measured (see PowerPoint in folder "MIC_sign_tables_Steps6,19,23": "Look at Breakpoint Signs in NARMS data"- blue slides, MIC values that could not be resolved in human data due to changes in susceptibility testing protocols were dropped. Also antimicrobial predictor variables were set as ordinal factors with levels the same as in cattle data. 

24. "MIC_Category_Merge_HumanData.qmd": combines AM categories to match those used in cattle data

25. "RF_Validation_on_Human_Data.qmd": tests the random forest models created in scripts 8-17 on NARMS human clinical data and computes its accuracy

############################################################################################

26. "MakeFiguresandTables.qmd" : pulls accuracy statistics from previous files and compiles the tables found in the manuscript. You may view the compiled tables in the folder "Tables"

-At the end of this script, there is a code block that exports Tables 3 and 4 as .png files. The function used to export the tables requires Google Chrome to be installed on your computer. 
-If you do not have Google Chrome installed on your computer, please run all code sections except the last section titled "Export Tables 3 and 4 as .png files." You may still view the information in Tables 3 and 4 without exporting them by entering "View (table_3_df)" and "View (table_3_df)" in the R studio console.

-Table 2 in the manuscript specifies the predictor variables used in each model and is not recreated here. However, you may find the information present in Table 2 by clicking on the script building the given model (scripts 8-17) and seeing the predictor variables used in the sub-section "Build RF using Training Data" under the "BEST MODEL" heading. You may also find the information present in Table 2 by viewing the variable importance plots generated for each model, found in the folder "Variable_Importance_Plot_Figures"

27. "PlusorMinus2Category.qmd": this calculates the overall accuracy of each model and accuracy by category when considering +/-2 dilutions as the margin of error
