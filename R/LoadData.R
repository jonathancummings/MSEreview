# MSE Problem Framing 
# Load MSE review data from Excel files into R
# 2/16/2021, JWC

##### 
# Meta Data and setup
# Description: Import excel file data to R to create a .RData file for use in other scripts

# Clear environment
rm(list = ls(all.names = TRUE)) # clear all objects includes hidden objects.
gc() # free up memory and report the memory usage.

# load libraries
library(tidyverse) # upgrade to base R
library(readxl) # read in excel files

# Obtain data tables
study<-read_xlsx("data/DB files - Excel/tblStudy.xlsx")
mgmt<-read_xlsx("data/DB files - Excel/tblStudyManagementTools.xlsx")
mgmt$fkStudyID<-parse_integer(mgmt$fkStudyID)
obj<-read_xlsx("data/DB files - Excel/tblStudyObjectives.xlsx")
obj$fkStudyID<-parse_integer(obj$fkStudyID)
fields<-read_xlsx("data/DB files - Excel/tblStudyFields.xlsx")

# save .RData file
save.image(file = "data/MSEreview.RData")
