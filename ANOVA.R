####################################################################################################################################
##                                                    Data dictionary: 'TOM_STA'                                                  ##
##                                                                                                                                ##
## social_inference = Parts 2 or 3 on TASIT                                                                                       ##
## q_no = question number ranging from 1-15 Part 2 & 1-16 Part 3                                                                  ##
## q_type = whether it was a think (1st order cognitive), feel (affective), do (2nd order cognitive), or say (control) question   ##
## q_response = whether the participants answered 1 (yes) or 0 (no)                                                               ##
## correct_response = whether correct response was 1 (yes), 2 (no)                                                                ##  
## answer_correct = whether participants answered correctly (TRUE) of incorrectly (FALSE)                                         ##
## emotion = whether the participant was classed as having high or low emotion perceptual ability                                 ##
## sub_category = whether exchange is 1 (sincere), 2 (simple sarcasm), 3 (paradoxical sarcasm), 4 (lies), 5 (sarcasm)             ##
## ID = participant id                                                                                                            ##
## mean_Endorse = average question response across all questions                                                                  ##
##                                                                                                                                ##
####################################################################################################################################

#Load packages
library(gtools)
library(psych)
library(tidyverse)
library(dplyr)
library(rstatix)
library(readxl)
library(ggpubr)
library(datarium)
library(ggplot2)
library(emmeans)
library(afex)


#import dataset
library(readxl)
TOM_STA <- read_excel("C:/Users/jaral005/OneDrive - University of South Australia/Desktop/STA/Datasets/ToM_STA_datasets/TOM_STA.xlsx", 
                      na = "NA")
View(TOM_STA)

#### General formatting and creating df's ####

#convert variables into factors
TOM_STA$sub_category <- factor(TOM_STA$sub_category)
TOM_STA$correct_response <- factor(TOM_STA$correct_response)
TOM_STA$emotion <- factor(TOM_STA$emotion)

#Re-name from numbers to labels
levels(TOM_STA$emotion) <- c("high", "low")
levels(TOM_STA$sub_category) <- c("sincere", "simple_sar", "para_sar", "lies", "sarcasm")
levels(TOM_STA$correct_response) <- c("yes", "no")


#### Think only df
TOM_STA_think<-TOM_STA[TOM_STA$q_type=='think',]
think_df<-TOM_STA[TOM_STA$q_type=='think',]

#calculate the average q_response across all questions for each participant & label this new variable 'meanEndorse
think_df <- think_df %>%
  filter(!is.na(q_response)) %>% # filter out na trials
  group_by(ID, emotion, q_type, sub_category, correct_response) %>%
  summarize(meanEndorse = mean(q_response))

#### Do only df
TOM_STA_do<-TOM_STA[TOM_STA$q_type=='do',]
do_df<-TOM_STA[TOM_STA$q_type=='do',]

#calculate the average q_response across all questions for each participant & label this new variable 'meanEndorse
do_df <- do_df %>%
  filter(!is.na(q_response)) %>% # filter out na trials
  group_by(ID, emotion, q_type, sub_category, correct_response) %>%
  summarize(meanEndorse = mean(q_response))


#### Feel only df
TOM_STA_feel<-TOM_STA[TOM_STA$q_type=='feel',]
feel_df<-TOM_STA[TOM_STA$q_type=='feel',]

#calculate the average q_response across all questions for each participant & label this new variable 'meanEndorse
feel_df <- feel_df %>%
  filter(!is.na(q_response)) %>% # filter out na trials
  group_by(ID, emotion, q_type, sub_category, correct_response) %>%
  summarize(meanEndorse = mean(q_response))


#### Think (first-order cognitive ToM) ANOVA ####

#### Graph data
ggplot(data = think_df) +
  geom_bar(stat = "identity", mapping = aes(x = correct_response, y = meanEndorse, fill = correct_response)) +
  facet_wrap(emotion ~ sub_category, nrow = 2)


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
aov1 <- aov_ez("ID", "meanEndorse", think_df, between = c("emotion"), within = c("correct_response", "sub_category"))
nice(aov1, es="pes", correction = "GG")


#### Posthoc using tukey
mresponse1 <- emmeans(aov1, ~ correct_response) #main effect of correct_response
pairs(mresponse)

mcategory1 <- emmeans(aov1, ~ "sub_category") #main effect of sub_category
pairs(mcategory)

interaction1 <- emmeans(aov1, "sub_category", by = "correct_response") #interactions
pairs(interaction1)


#### Plot interactions
plot1 <- afex_plot(aov1, x = "sub_category", trace = "emotion", panel = "correct_response", mapping = c("color", "fill"))
plot1 + theme_light()




#### Do (second-order cognitive ToM) ANOVA ####

#### Graph data
ggplot(data = do_df) +
  geom_bar(stat = "identity", mapping = aes(x = correct_response, y = meanEndorse, fill = correct_response)) +
  facet_wrap(emotion ~ sub_category, nrow = 2)



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
aov2 <- aov_ez("ID", "meanEndorse", do_df, between = c("emotion"), within = c("correct_response", "sub_category"))
nice(aov2, es="pes", correction = "GG")


#### Posthoc using tukey
mresponse1 <- emmeans(aov2, ~ correct_response) #main effect of correct_response
pairs(mresponse)

mcategory1 <- emmeans(aov2, ~ "sub_category") #main effect of sub_category
pairs(mcategory)

interaction1 <- emmeans(aov2, "sub_category", by = "correct_response") #interactions
pairs(interaction1)


#### Plot interactions
plot2 <- afex_plot(aov2, x = "sub_category", trace = "emotion", panel = "correct_response", mapping = c("color", "fill"))
plot2 + theme_light()




#### Feel (affective ToM) ANOVA ####

#### Graph data
ggplot(data = feel_df) +
  geom_bar(stat = "identity", mapping = aes(x = correct_response, y = meanEndorse, fill = correct_response)) +
  facet_wrap(emotion ~ sub_category, nrow = 2)


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
aov3 <- aov_ez("ID", "meanEndorse", feel_df, between = c("emotion"), within = c("correct_response", "sub_category"))
nice(aov3, es="pes", correction = "GG")


#### Posthoc using tukey
mresponse1 <- emmeans(aov3, ~ correct_response) #main effect of correct_response
pairs(mresponse)

mcategory1 <- emmeans(aov3, ~ "sub_category") #main effect of sub_category
pairs(mcategory)

interaction1 <- emmeans(aov3, "sub_category", by = "correct_response") #interactions
pairs(interaction1)


#### Plot interactions
plot3 <- afex_plot(aov3, x = "sub_category", trace = "emotion", panel = "correct_response", mapping = c("color", "fill"))
plot3 + theme_light()
