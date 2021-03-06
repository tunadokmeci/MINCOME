base_pay.data_revised.xlsx is the data file Gregory Mason from University of Manitoba sent us on Devember 11th, 2019, which is the last updated version of the Baseline-Payments data that can be found here : https://dataverse.lib.umanitoba.ca/dataset.xhtml?persistentId=doi:10.5203/FK2/R5YA5B
 
On the file sent by Prof. Mason, we have changed the variable names to make clear which ones referred to the male householder, 
and which ones referred to the female householder. This data is organized as cross-section, so that for each household we have one obserrvation, but variable values for 37 months. 
 
basepaypanel_revised.rds is the panel data version of base_pay.data_revised.xlsx, where for each household, we have 37 observations. The Stata code we used to make it into panel data can be found under tunadokmeci/MINCOME/Codes. We use this data only to detect increases in the number of children, and create an indicator variable for childbirth. We then add this variable to the cross section version of the data, and use it as our main data. 

familydata.xlsx includes information on each member in a household, and their relationship to household head. It was notcollected monthly, but 3-4 times max. per household. We use it to check whether the increases we see in the main data are due to children joining the household other than their own children, cousins, grandchildren, siblings etc. This data can be accessed on https://dataverse.lib.umanitoba.ca/dataset.xhtml?persistentId=doi:10.5203/FK2/X7BSPV. In the version we upload here, we changed the variable names we use. The full list of variables and their names can be found on: https://dataverse.lib.umanitoba.ca/dataset.xhtml?persistentId=doi:10.5203/FK2/VUA83U 

