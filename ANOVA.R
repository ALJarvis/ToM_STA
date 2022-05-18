######################################################################################################################################
##                                                    Data dictionary: 'TOM_STA'                                                    ##
##                                                                                                                                  ##
## social_inference = Parts 2 or 3 on TASIT                                                                                         ##
## q_no = question number ranging from 1-15 Part 2 & 1-16 Part 3                                                                    ##
## q_type = whether it was a think (1st order cognitive), feel (affective), do (2nd order cognitive), or say (control) question     ##
## q_response = whether the participants answered 1 (yes), 2 (no), or 3 (don't know)                                                ##  
## sub_category = whether exchange is 1 (sincere), 2 (simple sarcasm), 3 (paradoxical sarcasm), 4 (lies), 5 (sarcasm)               ##
## answer_correct = whether participants answered correctly (TRUE) of incorrectly (FALSE)                                           ##
## emotion = whether the participant was classed as having high or low emotion perceptual ability                                   ##
## ID = participant id                                                                                                              ##
##                                                                                                                                  ##
######################################################################################################################################

#Load packages
library(gtools)
library(psych)
library(tidyverse)
library(dplyr)
library(rstatix)
library(readxl)
library(ggpubr)
library(datarium)

#import dataset
library(readxl)
TOM_STA <- read_excel("C:/Users/jaral005/OneDrive - University of South Australia/Desktop/STA/Datasets/ToM_STA_datasets/TOM_STA.xlsx", 
                      na = "999")
View(TOM_STA)

#### Format dataframe ####

#Label dependent variables
dv1 = "do"