#########################################################################################
#     Actigraphy features for predicting mobility disability in older adults            #
#                                                                                       #
#     Author: Matin Kheirkhahan                                                         #
#     Email: matin@cise.ufl.edu                                                         #
#     Date: Jul 18, 2016                                                                #
#                                                                                       #
#########################################################################################

#########################################################################################
# This script is implemented specifically to construct variables for all the            #
# participants in LIFE folder.                                                          #
#########################################################################################

# This library is required to detect wear times
library(PhysicalActivity)

# To load the necessary functions, set the working directory to folder where this script is located.
setwd("~/Workspaces/R workspace/Mobility Signature Paper/mobility-signature/LIFE toolbox/")

# loading necessary functions
source("f01_functions.R")

# Now, set the directory to the folder where all the data files (LIFE) are located. This helps in expediting the loading process
setwd("~/../../Volumes/SHARE/ARRC/Active_Studies/ANALYSIS_ONGOING/LIFE Main data/LIFE accelerometry - second data - 10_26_15/")

# Loading mappings between HIDs (filenames) and PIDs ----> REF (data.frame)
load("PID_VC_HID.Rdata")

# Keeping a log for missing files
missingFiles.log <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(missingFiles.log) <- c("filename", "visit")

# Which visit do you want to consider?
#     0: baseline
#     6: F06
#    12: F12
#    24: F24
# Baseline ----------------------------------------
LIFE.df <- data.frame(matrix(nrow = 0, ncol = 65))
for(visit in c(0, 6, 12, 24)) {
     message(paste("Visit", visit, "is being considered. -----------------"))
     visit.files <- REF[REF$seq == visit, ]
     for(i in 1:nrow(visit.files)) {
          fileName <- paste("HID", visit.files$HID[i], ".Rdata", sep = "")
          message(paste("(", i, "/", nrow(visit.files), ") Reading ", fileName, sep = ""))
          if(file.exists(fileName)) {
               load(fileName)
               ppt.feature.df <- main.constructFeatures(AC.1s, PID = visit.files$pid[i])
               LIFE.df <- rbind(LIFE.df, data.frame(visit = visit,
                                                    ppt.feature.df))
          } else {
               message(paste(fileName, " not found"))
               missingFiles.log <- rbind(missingFiles.log, data.frame(filename = fileName, visit = visit))
          }
     }
}

save(LIFE.df, file = "../Mobility Signature Dataset/d01_LIFE_071816.Rdata")
save(missingFiles.log, file = "../Mobility Signature Dataset/log01_missingFiles_071816.Rdata")
