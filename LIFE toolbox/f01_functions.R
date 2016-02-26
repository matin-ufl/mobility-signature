# Required functions for Circadian analysis

# Clear function
clr <- function() {cat(rep("\n", 50))}

# Those files' information which have the minimum valid days (10+ wear times)
valid_participants <- function(PID_VC_HID, valid.days = 5) {
     selectedPIDs <- PID_VC_HID[PID_VC_HID$seq == 0, ]
     selectedPIDs <- selectedPIDs[selectedPIDs$valid_days >= valid.days, ]
     selectedPIDs
}


# Find wear times
find.wearTime <- function(accelerometer.1s.data = AC.1s) {
     mydata1m <- dataCollapser(accelerometer.1s.data, TS = "TimeStamp", col = "VM", by = 60)
     data1m = wearingMarking(dataset = mydata1m,
                             frame = 90, 
                             perMinuteCts = 1,
                             TS = "TimeStamp",
                             cts = "VM", 
                             streamFrame = NULL, 
                             allowanceFrame= 2, 
                             newcolname = "wearing")
     
     a <- sumVct(data1m, id="sdata1m")
}

# Find wear times (with Miller outlier rejection)
find.wearTime.exludeOutlier <- function(AC.1s, sample.per.min = 60, PID) {
     AC.1m <- dataCollapser(AC.1s, TS = "TimeStamp", col = "VM", by = sample.per.min)
     data1m = wearingMarking(dataset = AC.1m,
                             frame = 90, 
                             perMinuteCts = 1,
                             TS = "TimeStamp",
                             cts = "VM", 
                             streamFrame = NULL, 
                             allowanceFrame= 2, 
                             newcolname = "wearing")
     steps <- dataCollapser(AC.1s, TS = "TimeStamp", col = "steps", by = sample.per.min)
     data1m$steps <- steps$steps
     
     # First step in defining outliers
     data1m$wearing[data1m$VM > 100000] <- "nw"
     data1m$wearing[data1m$steps > 180] <- "nw"
     
     # Second step: more complicated
     secondHighest.all <- data.frame(day = 1:max(data1m$days), minute.AC = rep(NA, max(data1m$days)))
     for (day in 1:max(data1m$days)) {
          dayStart.idx <- min(which(data1m$days == day))
          if(length(dayStart.idx) > 0) {
               dayEnd.idx <- max(which(data1m$days == day))
               if(length(dayEnd.idx) > 0) {
                    max.ac <- max(data1m$VM[dayStart.idx:dayEnd.idx])
                    valid.idx <- which(data1m$VM[dayStart.idx:dayEnd.idx] < max.ac)
                    if(length(valid.idx) > 0) {
                         valid.idx <- valid.idx + dayStart.idx - 1
                         secondHighest.all$minute.AC[day] <- max(data1m$VM[valid.idx])
                    }
               }
          }
     }
     
     # Now we can have both thresholds calculated: <"median + 3500" and "day + 1000">
     threshold.all <- median(secondHighest.all$minute.AC) + 3500
     for(day in 1:max(data1m$days)) {
          threshold.day <- secondHighest.all$minute.AC[day] + 1000
          dayStart.idx <- min(which(data1m$days == day))
          if(length(dayStart.idx) > 0) {
               dayEnd.idx <- max(which(data1m$days == day))
               if(length(dayEnd.idx) > 0) {
                    candidate.idx <- which(data1m$VM[dayStart.idx:dayEnd.idx] > threshold.all)
                    if(length(candidate.idx) > 0) {
                         candidate.idx <- candidate.idx + dayStart.idx - 1
                         outlier.idx <- which(data1m$VM[candidate.idx] > threshold.day)
                         if(length(outlier.idx) > 0) {
                              data1m$wearing[(outlier.idx + candidate.idx - 1)] <- "nw"
                         }
                    }
               }
          }
     }
     a <- sumVct(data1m, id = PID)
}


# Constructing and appending vector magnitude
append.VM <- function (accelerometer.data, ID) {
     result <- accelerometer.data
     if(!is.na(result$axis3[1])) {
          result$VM <- sqrt((result$axis1^2) + (result$axis2^2) + (result$axis3^2))
     } else if(!is.na(AC.1s$axis2[1])) {
          cat(paste("Participant (", ID, ") does not have axis.3 data.", sep = ""))
          result$VM <- sqrt((result$axis1^2) + (result$axis2^2))
     } else {
          cat(paste("Participant (", ID, ") does not have axis.3-axis.2 data.", sep = ""))
          result$VM <- result$axis1
     }
     result
}