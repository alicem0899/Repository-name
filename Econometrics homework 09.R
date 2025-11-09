# ------------------------------------------------------------------------------
# Author: Ali C. Erturk
# Data Created: 11-04-2025
# Course: Econ 4660, Fall 2025, U of Utah
# This code:
#   
#      Homework 09: statistical analysis
# ------------------------------------------------------------------------------

# preemptively clear environment
rm(list = ls())

# set working directory
setwd("C:/Users/Aertu/OneDrive/Documents/Econometrics 4560") # set working directory

# 1

baseline <- read.csv("C:/Users/Aertu/OneDrive/Documents/Econometrics 4560/baseline.csv") # import the data
follow <- read.csv("C:/Users/Aertu/OneDrive/Documents/Econometrics 4560/follow.csv") # import the data 
 

baseline$post <- 0 # add an indicator for pre 
follow$post <- 1   # add an indicator for post

long_data <- rbind(baseline, follow)

install.packages("tidyverse")

# Load the required library
library(dplyr)

str(long_data$maize_yield) # reveal type of data (character, numeric)

long_data$maize_yield <- gsub(",", "", long_data$maize_yield) # remove commas 

long_data$maize_yield <- as.numeric(long_data$maize_yield) # convert column maize_yield to numeric data 

# 2. 

maize_means <- long_data %>%  # Calculate the mean values for pre-treated group, pre-untreated group, post-treated group, post-untreated group
  group_by(post, treatment) %>%
  summarise(mean_yield = mean(maize_yield, na.rm = TRUE)) 

mean_yield_treatment_post <- 698.7860 # define 
mean_yield_treatment_pre <- 613.4167 # define
mean_yield_control_pre <- 718.1393   # define
mean_yield_control_post <- 591.7327  # define

pre_control_post_control_diff <- mean_yield_treatment_post - mean_yield_treatment_pre # calculate the post period minus the pre for the control group
pre_treatment_post_treatment_diff <- mean_yield_control_post - mean_yield_control_pre # calculate the post period minus the pre for the treatment group
Pre_treatment_minus_control_diff <- mean_yield_treatment_pre - mean_yield_control_pre # calculate the treatment minus control in the pre period
Post_treatment_minus_control_diff <- mean_yield_treatment_post - mean_yield_control_post # calculate the treatment minus control in the post period
pre_treatment_post_treatment_diff - pre_control_post_control_diff # calculate the change over time for the treatment minus the control

#3

long_data$interaction_dummy <- long_data$post * long_data$treatment # create an indicator dummy vector
         
model <- lm(maize_yield ~ post + treatment + interaction_dummy, data = long_data) #produce linear regression with only indicators
model_comp <- lm(maize_yield ~ post + treatment + interaction_dummy + head_age + head_female + head_education + hh_members + farm_size, data = long_data)
summary(model) # print visuals for regression with treatment and period indicators 
summary(model_comp) # print visuals for regression with treatment and period indicators and covariates 

# The second regression substantially reduces the estimated effect of the treatment, and modestly reduces the treatment coefficient's standard error.
# The post and interaction dummy variables also slightly reduce their standard errors. The interaction dummy becomes even more significant. 

# 4 

compromised_model <- lm(maize_yield ~ treatment + head_age + head_female + head_education + hh_members + farm_size, data = long_data) # produce regression
summary (compromised_model) # visualize statistics

# the compromised model lacking appropriate controls reflects significant bias in its estimates of the treatment which now tripple from the initial model and increase 
# roughly ten-fold from the second regression specification. In addition, the treatment effects are highly signficant, and the standard error falls 59.
# The reason for this is because the removal of the interaction dummy attributes positive effect to the treatment, while the removal of the post (period) indicator attributes negative
# effects to the treatment, but the interaction indicator attributes a much stronger positive effect, upwardly biasing the treatment effect. The interaction dummy is also positively correlated with 
# the treatment.