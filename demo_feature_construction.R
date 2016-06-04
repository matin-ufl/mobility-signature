#########################################################################################
#     Actigraphy features for predicting mobility disability in older adults            #
#                                                                                       #
#     Author: Matin Kheirkhahan                                                         #
#     Email: matin@cise.ufl.edu                                                         #
#     Date: Feb 16, 2016                                                                #
#                                                                                       #
#########################################################################################

#########################################################################################
# To construct features mentioned in the paper, load the functions in 'f01_functions'   #
# and call 'main.constructFeatures' function.                                           #
#                                                                                       #
# This function takes two arguments:                                                    #
#     1. AC.1s: the accelerometer data file. This file has to contain 1-sec data points #
#     2. PID: a unique identifier for the participant.                                  #
#                                                                                       #
# See the example below for a better understanding.                                     #
#                                                                                       #
#########################################################################################

library(PhysicalActivity)
source("LIFE toolbox/f01_functions.R")

# Loading acceleromter data (1-second epochs)
load("test.Rdata")
# Defining a unique identifier for the participant
PID <- 8115990
# Constructing time-domain acceleromter features
features.df <- main.constructFeatures(AC.1s, PID)