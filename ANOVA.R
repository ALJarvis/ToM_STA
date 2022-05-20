######################################################################################################################################
##                                                    Data dictionary: 'TOM_STA'                                                    ##
##                                                                                                                                  ##
## social_inference = Parts 2 or 3 on TASIT                                                                                         ##
## q_no = question number ranging from 1-15 Part 2 & 1-16 Part 3                                                                    ##
## q_type = whether it was a think (1st order cognitive), feel (affective), do (2nd order cognitive), or say (control) question     ##
## q_response = whether the participants answered 1 (yes) or 0 (no)                                                                 ##
## correct_response whether question answered correctly; 1 (yes), 2 (no)                                                            ##  
## answer_correct = whether participants answered correctly (TRUE) of incorrectly (FALSE)                                           ##
## emotion = whether the participant was classed as having high or low emotion perceptual ability                                   ##
## sub_category = whether exchange is 1 (sincere), 2 (simple sarcasm), 3 (paradoxical sarcasm), 4 (lies), 5 (sarcasm)               ##
## ID = participant id                                                                                                              ##
## mean_Endorse = average question response across all questions                                                                    ##
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

#### Think (first-order cognitive ToM) ANOVA ####

#### Format data

#create new dataframe which only includes think (first-order ToM) questions and rename think_df
TOM_STA_think<-TOM_STA[TOM_STA$q_type=='think',]
think_df<-TOM_STA[TOM_STA$q_type=='think',]

#calculate the average q_response across all questions for each participant & label this new variable 'meanEndorse
think_df <- think_df %>%
  filter(!is.na(q_response)) %>% # filter out na trials
  group_by(ID, emotion, q_type, sub_category) %>%
  summarize(meanEndorse = mean(q_response))

#### Check assumptions

#Check for extreme outliers
think_df %>%
  group_by(emotion, sub_category) %>%
  identify_outliers(meanEndorse)

#Check for normality
think_df %>%
  group_by(emotion, sub_category) %>%
  shapiro_test(meanEndorse) 

#Create QQ plots for each cell design
ggqqplot(think_df, "meanEndorse", ggtheme = theme_bw()) +
  facet_grid(emotion ~ sub_category, labeller = "label_both") 

#Check for homogeneity of variance
think_df %>%
  group_by(sub_category) %>%
  levene_test(meanEndorse ~ emotion) 


#### Run the ANOVA
think_df <- ungroup(think_df) #anova code wouldn't work until I ungrouped the data 

res.aov <- anova_test(
  data = think_df, dv = meanEndorse, wid = ID, 
  between = emotion, within = sub_category
)
get_anova_table(res.aov)


#Compute simple main effects
one.way <- think_df %>%
  group_by(sub_category) %>%
  anova_test(dv = meanEndorse, wid = ID, between = emotion) %>%
  get_anova_table() 
one.way


#### Do (second-order cognitive ToM) ANOVA ####

#### Format data

#create new dataframe which only includes do (second-order cognitive) questions and rename do_df
TOM_STA_do<-TOM_STA[TOM_STA$q_type=='do',]
do_df<-TOM_STA[TOM_STA$q_type=='do',]

#calculate the average q_response across all questions for each participant & label this new variable 'meanEndorse
do_df <- do_df %>%
  filter(!is.na(q_response)) %>% # filter out na trials
  group_by(ID, emotion, q_type, sub_category) %>%
  summarize(meanEndorse = mean(q_response))


#### Check assumptions

#Check for extreme outliers
do_df %>%
  group_by(emotion, sub_category) %>%
  identify_outliers(meanEndorse)

#Check for normality
do_df %>%
  group_by(emotion, sub_category) %>%
  shapiro_test(meanEndorse) 

#Create QQ plots for each cell design
ggqqplot(do_df, "meanEndorse", ggtheme = theme_bw()) +
  facet_grid(emotion ~ sub_category, labeller = "label_both") 

#Check for homogeneity of variance
do_df %>%
  group_by(sub_category) %>%
  levene_test(meanEndorse ~ emotion) 


#### Run the ANOVA
do_df <- ungroup(do_df) #anova code wouldn't work until I ungrouped the data 

res.aov <- anova_test(
  data = do_df, dv = meanEndorse, wid = ID, 
  between = emotion, within = sub_category
)
get_anova_table(res.aov)


#Compute simple main effects
one.way <- do_df %>%
  group_by(sub_category) %>%
  anova_test(dv = meanEndorse, wid = ID, between = emotion) %>%
  get_anova_table() 
one.way

#### Feel (affective ToM) ANOVA ####

#### Format data

#create new dataframe which only includes do (second-order cognitive) questions and rename feel_df
TOM_STA_do<-TOM_STA[TOM_STA$q_type=='do',]
feel_df<-TOM_STA[TOM_STA$q_type=='do',]

#calculate the average q_response across all questions for each participant & label this new variable 'meanEndorse
feel_df <- feel_df %>%
  filter(!is.na(q_response)) %>% # filter out na trials
  group_by(ID, emotion, q_type, sub_category) %>%
  summarize(meanEndorse = mean(q_response))


#### Check assumptions

#Check for extreme outliers
feel_df %>%
  group_by(emotion, sub_category) %>%
  identify_outliers(meanEndorse)

#Check for normality
feel_df %>%
  group_by(emotion, sub_category) %>%
  shapiro_test(meanEndorse)  

#Create QQ plots for each cell design
ggqqplot(feel_df, "meanEndorse", ggtheme = theme_bw()) +
  facet_grid(emotion ~ sub_category, labeller = "label_both") 

#Check for homogeneity of variance
feel_df %>%
  group_by(sub_category) %>%
  levene_test(meanEndorse ~ emotion)


#### Run the ANOVA
feel_df <- ungroup(feel_df) #anova code wouldn't work until I ungrouped the data 

res.aov <- anova_test(
  data = feel_df, dv = meanEndorse, wid = ID, 
  between = emotion, within = sub_category
)
get_anova_table(res.aov)


#Compute simple main effects
one.way <- feel_df %>%
  group_by(sub_category) %>%
  anova_test(dv = meanEndorse, wid = ID, between = emotion) %>%
  get_anova_table() 
one.way
