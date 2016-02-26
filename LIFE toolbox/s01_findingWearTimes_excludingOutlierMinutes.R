library(PhysicalActivity)

setwd("~/Workspaces/R workspace/Mobility Signature Paper/mobility-signature/LIFE toolbox/")
source("f01_functions.R")
# Data File selections ------------------------------------------

# Temporary for easier file selection
setwd("~/../../Volumes/aging/SHARE/ARRC/Active_Studies/ANALYSIS_ONGOING/LIFE Main data/LIFE accelerometry - second data - 10_26_15/")

# Select PID_VC_HID
clr()
load("PID_VC_HID.Rdata")

valid.files <- valid_participants(PID_VC_HID = REF, valid.days = 5)
rm(REF)

# Creating new files without outlier points
out.table <- data.frame(matrix(nrow = 0, ncol = 8))
colnames(out.table) <- c("PID", "startTimeStamp", "endTimeStamp", "days", "weekday", "start", "end", "duration")
for (i in 1:nrow(valid.files)) {
     PID <- valid.files$pid[i]
     HID <- paste("HID", valid.files$HID[i], ".RData", sep = "")
     load(HID)
     
     AC.1s <- append.VM(AC.1s, HID)
     wearTimes.info <- find.wearTime.exludeOutlier(AC.1s, sample.per.min = 60)
     out.table <- rbind(out.table, wearTimes.info)
     print(paste(i, " out of ", nrow(valid.files), " - Being processed... ", HID, " PID (", PID, ")", sep = ""))
}

colnames(out.table) <- c("PID", "startTimeStamp", "endTimeStamp", "days", "weekday", "start", "end", "duration")
save(out.table, file = "../Baseline weartimes - outliers excluded/wearTimes_table.RData")