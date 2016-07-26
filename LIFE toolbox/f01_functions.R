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
  VM <- rep(NA, nrow(accelerometer.data))
  if(!is.na(accelerometer.data$axis3[1])) {
    VM <- sqrt((accelerometer.data$axis1^2) + (accelerometer.data$axis2^2) + (accelerometer.data$axis3^2))
  } else if(!is.na(accelerometer.data$axis2[1])) {
    cat(paste("Participant (", ID, ") does not have axis.3 data.", sep = ""))
    VM <- sqrt((accelerometer.data$axis1^2) + (accelerometer.data$axis2^2))
  } else {
    cat(paste("Participant (", ID, ") does not have axis.3-axis.2 data.", sep = ""))
    VM <- accelerometer.data$axis1
  }
  VM
}

# Shrinking the data to 1 minute data points
shrink.to.minute.data <- function(accelerometer.second.data, columns = c("axis1", "axis2", "axis3", "VM")) {
  output <- dataCollapser(accelerometer.second.data, TS = "TimeStamp", col = columns[1], by = 60)
  for(cname in columns[2:length(columns)]) {
    temp <- dataCollapser(accelerometer.second.data, TS = "TimeStamp", col = cname, by = 60)
    output[, cname] <- temp[, cname]
  }
  output
}

# Shrinking the data to 1 hour data points
shrink.to.hour.data <- function(accelerometer.second.data, columns = c("axis1", "axis2", "axis3", "VM")) {
  output <- dataCollapser(accelerometer.second.data, TS = "TimeStamp", col = columns[1], by = 600)
  for(cname in columns[2:length(columns)]) {
    temp <- dataCollapser(accelerometer.second.data, TS = "TimeStamp", col = cname, by = 600)
    output[, cname] <- temp[, cname]
  }
  output
}

# Identifying bouts of activity
identify.bouts <- function(axis.1s) {
  MIN_ACTIVITY_THRESHOLD <- 100 # A minute should have more than 100 activity counts to be considered as a bout of activity.
  axis.1s[axis.1s < (MIN_ACTIVITY_THRESHOLD / 60)] <- 0 # Consequtively, a second should also have more than 100/60 activity count.
  MIN_ACTIVE_SECONDS <- 10 # Should at least this many seconds have activity counts > 0
  MIN_CONSEQ_ZEROS <- 30 # If this many consequtive seconds are all 0z, the bout has ended
  
  bouts <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(bouts) <- c("start.min", "end.min")
  startMin <- NA
  currMin <- 1
  numberOfConseqZeros <- 0
  for(i in seq(1, length(axis.1s), by = 60)) {
    minuteData <- axis.1s[i:min((i+59), length(axis.1s))]
    
    # If a minute has 10 seconds of non-zero activity and the minute has > 100 activity count, then a bout is either started or continued.
    if(length(which(minuteData > 0)) >= MIN_ACTIVE_SECONDS && sum(minuteData) > 100) {
      # If there is a bout before this, the bout is extended. Other than that, we mark the bout's start minute.
      if(is.na(startMin)) {
        startMin <- currMin         
      }
      
      # Check if the current minute is the last minute of the bout, or the bout is extended.
      for(s in 1:length(minuteData)) {
        # number of consequtive zeros (which could have some tail in the previous minute) are counted.
        if(minuteData[s] == 0) {
          numberOfConseqZeros <- numberOfConseqZeros + 1
        } else {
          # If we have more than 30 consequtive seconds of zero activity count, then the bout has ended.
          if(numberOfConseqZeros >= MIN_CONSEQ_ZEROS) { # Bout has ended
            # Check if there is any bout for that minute. If no start time is set, then there was no bout.
            if(!is.na(startMin)) {
              # If there is a start time, that means bout has ended here.
              bouts <- rbind(bouts, data.frame(start.min = startMin, end.min = currMin + 1))
              startMin <- NA
            }
          }
          numberOfConseqZeros <- 0 # Resetting the counter for 0 activity counts
        }
      }
    } else { 
      if(!is.na(startMin)) {
        # If there is a start time, that means bout has ended here.
        bouts <- rbind(bouts, data.frame(start.min = startMin, end.min = currMin + 1))
        startMin <- NA
      }
      numberOfConseqZeros <- 0 # Resetting the counter for 0 activity counts
    }
    
    # Next minute should be examined.
    currMin <- currMin + 1
  }
  
  bouts
}


# Calculating bout statistics: duration (minute) and activity counts per minute
bout.statistics <- function(axis_bouts, axis.1m) {
  bouts <- data.frame(matrix(nrow = 0, ncol = 2))
  if(nrow(axis_bouts) > 0) {
    for(i in 1:nrow(axis_bouts)) {
      bouts <- rbind(bouts,
                     data.frame(duration.minute = axis_bouts$end.min[i] - axis_bouts$start.min[i],
                                activityCount.per.minute = sum(axis.1m[axis_bouts$start.min[i]:(axis_bouts$end.min[i]-1)]) / (axis_bouts$end.min[i] - axis_bouts$start.min[i])))
    }
  }
  colnames(bouts) <- c("duration.minute", "activityCount.per.minute")
  bouts
}

# Calculating gap statistics: duration (minute)
gap.statistics <- function(axis_bouts) {
  gaps <- data.frame(matrix(nrow = 0, ncol = 2))
  if(nrow(axis_bouts) > 0) {
    for(i in 1:nrow(axis_bouts)) {
      gaps <- rbind(gaps,
                    data.frame(duration.minute = axis_bouts$end.min - axis_bouts$start.min))
    }
  }
  colnames(gaps) <- "duration.minute"
  gaps
}


# The main function: constructs features from 1-sec activity count data
# AC.1s: a dataframe containing 1-sec activity count data with columns <TimeStamp, axis1, axis2, axis3>
# PID: unique participant ID
main.constructFeatures <- function(AC.1s, PID) {
  accelerometerData <- AC.1s
  accelerometerData$VM <- append.VM(accelerometerData, PID)
  
  output <- data.frame(PID = PID, Number_of_valid_wear_times = NA, axis1_proportion = NA, axis2_proportion = NA, axis3_proportion = NA,
                       axis1_Total_number_of_bouts = NA, axis2_Total_number_of_bouts = NA, axis3_Total_number_of_bouts = NA, VM_Total_number_of_bouts = NA,
                       axis1_Number_of_bouts_per_wear_time_avg = NA, axis2_Number_of_bouts_per_wear_time_avg = NA, axis3_Number_of_bouts_per_wear_time_avg = NA, VM_Number_of_bouts_per_wear_time_avg = NA,
                       axis1_Number_of_bouts_per_wear_time_std = NA, axis2_Number_of_bouts_per_wear_time_std = NA, axis3_Number_of_bouts_per_wear_time_std = NA, VM_Number_of_bouts_per_wear_time_std = NA,
                       axis1_Activity_counts_per_minute_per_bout_avg = NA, axis2_Activity_counts_per_minute_per_bout_avg = NA, axis3_Activity_counts_per_minute_per_bout_avg = NA, VM_Activity_counts_per_minute_per_bout_avg = NA,
                       axis1_Activity_counts_per_minute_per_bout_std = NA, axis2_Activity_counts_per_minute_per_bout_std = NA, axis3_Activity_counts_per_minute_per_bout_std = NA, VM_Activity_counts_per_minute_per_bout_std = NA,
                       axis1_Bout_length_avg = NA, axis2_Bout_length_avg = NA, axis3_Bout_length_avg = NA, VM_Bout_length_avg = NA,
                       axis1_Bout_length_std = NA, axis2_Bout_length_std = NA, axis3_Bout_length_std = NA, VM_Bout_length_std = NA,
                       axis1_Gap_between_bouts_length_avg = NA, axis2_Gap_between_bouts_length_avg = NA, axis3_Gap_between_bouts_length_avg = NA, VM_Gap_between_bouts_length_avg = NA,
                       axis1_Gap_between_bouts_length_std = NA, axis2_Gap_between_bouts_length_std = NA, axis3_Gap_between_bouts_length_std = NA, VM_Gap_between_bouts_length_std = NA,
                       axis1_Activity_count_per_wear_time_avg = NA, axis2_Activity_count_per_wear_time_avg = NA, axis3_Activity_count_per_wear_time_avg = NA, VM_Activity_count_per_wear_time_avg = NA,
                       axis1_Activity_count_per_wear_time_std = NA, axis2_Activity_count_per_wear_time_std = NA, axis3_Activity_count_per_wear_time_std = NA, VM_Activity_count_per_wear_time_std = NA,
                       axis1_Bucket_for_half_life = NA, axis2_Bucket_for_half_life = NA, axis3_Bucket_for_half_life = NA, VM_Bucket_for_half_life = NA,
                       axis1_Counts_per_min_for_half_life = NA, axis2_Counts_per_min_for_half_life = NA, axis3_Counts_per_min_for_half_life = NA, VM_Counts_per_min_for_half_life = NA,
                       number_of_activityPeriods_per_wear_time = NA, activityPeriods_duration_avg = NA, activityPeriods_duration_median = NA, activityPeriods_duration_std = NA,
                       activity_count_in_activityPeriods_per_minute_avg = NA, activity_count_in_activityPeriods_per_minute_median = NA, activity_count_in_activityPeriods_per_minute_std = NA)
  
  # Some participants do not have any wear time but have accelerometer data! Should check for them, or the program crashes.
  wearTimes <- tryCatch({a <- find.wearTime.exludeOutlier(accelerometerData, PID = PID)},
                        error = function(cond) {
                          message(paste("PID (", PID, ") had no wear time! should be skipped."))
                          return(NA)
                        },
                        finally = {})
  if(is.na(wearTimes))
    return(output)
  wearTimes$axis1 <- NA # sum of activity counts in axis1
  wearTimes$axis2 <- NA # sum of activity counts in axis2
  wearTimes$axis3 <- NA # sum of activity counts in axis3
  wearTimes$VM <- NA # sum of activity counts in VM
  wearTimes$axis1_numberOfBouts <- NA # Number of bouts (see the manuscript for bout of activity description)
  wearTimes$axis2_numberOfBouts <- NA
  wearTimes$axis3_numberOfBouts <- NA
  wearTimes$VM_numberOfBouts <- NA
  
  # valid wear time is defined as having 10+ hours of wear time.
  validWearTimeThreshold <- 60 * 10 # 10 hours
  wearTimes <- wearTimes[wearTimes$duration >= validWearTimeThreshold, ]
  rm(validWearTimeThreshold)
  output$Number_of_valid_wear_times <- nrow(wearTimes)
  
  
  # Shrinking the data to minute data points
  accelerometerData.1m <- shrink.to.minute.data(accelerometerData)
  
  # Defining bouts of activity and gaps between them
  axis1_bouts <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(axis1_bouts) <- c("duration.minute", "activityCount.per.minute")
  axis2_bouts <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(axis2_bouts) <- c("duration.minute", "activityCount.per.minute")
  axis3_bouts <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(axis3_bouts) <- c("duration.minute", "activityCount.per.minute")
  VM_bouts <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(VM_bouts) <- c("duration.minute", "activityCount.per.minute")
  axis1_gaps <- data.frame(matrix(nrow = 0, ncol = 1))
  colnames(axis1_gaps) <- "duration.minute"
  axis2_gaps <- data.frame(matrix(nrow = 0, ncol = 1))
  colnames(axis2_gaps) <- "duration.minute"
  axis3_gaps <- data.frame(matrix(nrow = 0, ncol = 1))
  colnames(axis3_gaps) <- "duration.minute"
  VM_gaps <- data.frame(matrix(nrow = 0, ncol = 1))
  colnames(VM_gaps) <- "duration.minute"
  
  activityPeriods = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(activityPeriods) <- c("duration.minute", "activityCounts.per.minute")
  if(nrow(wearTimes) > 1) {
    for(i in 1:nrow(wearTimes)) {
      # Only wear time windows are considered
      wearTimeData.1m <- accelerometerData.1m[wearTimes$start[i]:wearTimes$end[i], ]
      startIdx <- which(as.character(accelerometerData$TimeStamp) == as.character(wearTimes$startTimeStamp[i]))
      endIdx <- which(as.character(accelerometerData$TimeStamp) == as.character(wearTimes$endTimeStamp[i]))
      if(length(startIdx) == 0 ||
         length(endIdx) == 0) {
        message("Times do not match!!!")
        break()
      }
      wearTimeData.1s <- accelerometerData[startIdx:endIdx, ]
      rm(startIdx, endIdx)
      
      # Used for "axis_proportion" variables
      wearTimes$axis1[i] <- sum(wearTimeData.1m$axis1)
      wearTimes$axis2[i] <- sum(wearTimeData.1m$axis2)
      wearTimes$axis3[i] <- sum(wearTimeData.1m$axis3)
      wearTimes$VM[i] <- sum(wearTimeData.1m$VM)
      
      # identifying bouts withing this wear time
      curr_axis1_bouts <- identify.bouts(wearTimeData.1s$axis1)
      wearTimes$axis1_numberOfBouts[i] <- nrow(curr_axis1_bouts)
      curr_axis2_bouts <- identify.bouts(wearTimeData.1s$axis2)
      wearTimes$axis2_numberOfBouts[i] <- nrow(curr_axis2_bouts)
      curr_axis3_bouts <- identify.bouts(wearTimeData.1s$axis3)
      wearTimes$axis3_numberOfBouts[i] <- nrow(curr_axis3_bouts)
      curr_VM_bouts <- identify.bouts(wearTimeData.1s$VM)
      wearTimes$VM_numberOfBouts[i] <- nrow(curr_VM_bouts)
      
      axis1_bouts <- rbind(axis1_bouts, bout.statistics(curr_axis1_bouts, wearTimeData.1m$axis1))
      activityPeriods <- rbind(activityPeriods, activity.period.features(wearTimeData.1s$axis1))
      
      axis2_bouts <- rbind(axis2_bouts, bout.statistics(curr_axis2_bouts, wearTimeData.1m$axis2))
      axis3_bouts <- rbind(axis3_bouts, bout.statistics(curr_axis3_bouts, wearTimeData.1m$axis3))
      VM_bouts <- rbind(VM_bouts, bout.statistics(curr_VM_bouts, wearTimeData.1m$VM))
      
      axis1_gaps <- rbind(axis1_gaps, gap.statistics(curr_axis1_bouts))
      axis2_gaps <- rbind(axis2_gaps, gap.statistics(curr_axis2_bouts))
      axis3_gaps <- rbind(axis3_gaps, gap.statistics(curr_axis3_bouts))
      VM_gaps <- rbind(VM_gaps, gap.statistics(curr_VM_bouts))
      
      
    }
    rm(i, wearTimeData.1m, wearTimeData.1s, curr_axis1_bouts, curr_axis2_bouts, curr_axis3_bouts, curr_VM_bouts)
    
    output$axis1_proportion <- sum(wearTimes$axis1, na.rm = T) / sum(wearTimes$VM, na.rm = T)
    output$axis2_proportion <- sum(wearTimes$axis2, na.rm = T) / sum(wearTimes$VM, na.rm = T)
    output$axis3_proportion <- sum(wearTimes$axis3, na.rm = T) / sum(wearTimes$VM, na.rm = T)
    
    output$axis1_Activity_count_per_wear_time_avg <- mean(wearTimes$axis1, na.rm = T)
    output$axis1_Activity_count_per_wear_time_std <- sd(wearTimes$axis1, na.rm = T)
    output$axis2_Activity_count_per_wear_time_avg <- mean(wearTimes$axis2, na.rm = T)
    output$axis2_Activity_count_per_wear_time_std <- sd(wearTimes$axis2, na.rm = T)
    output$axis3_Activity_count_per_wear_time_avg <- mean(wearTimes$axis3, na.rm = T)
    output$axis3_Activity_count_per_wear_time_std <- sd(wearTimes$axis3, na.rm = T)
    output$VM_Activity_count_per_wear_time_avg <- mean(wearTimes$VM, na.rm = T)
    output$VM_Activity_count_per_wear_time_std <- sd(wearTimes$VM, na.rm = T)
    
    output$axis1_Total_number_of_bouts <- sum(wearTimes$axis1_numberOfBouts, na.rm = T)
    output$axis2_Total_number_of_bouts <- sum(wearTimes$axis2_numberOfBouts, na.rm = T)
    output$axis3_Total_number_of_bouts <- sum(wearTimes$axis3_numberOfBouts, na.rm = T)
    output$VM_Total_number_of_bouts <- sum(wearTimes$VM_numberOfBouts, na.rm = T)
    
    output$axis1_Activity_counts_per_minute_per_bout_avg <- mean(axis1_bouts$activityCount.per.minute, na.rm = T)
    output$axis1_Activity_counts_per_minute_per_bout_std <- sd(axis1_bouts$activityCount.per.minute, na.rm = T)
    output$axis2_Activity_counts_per_minute_per_bout_avg <- mean(axis2_bouts$activityCount.per.minute, na.rm = T)
    output$axis2_Activity_counts_per_minute_per_bout_std <- sd(axis2_bouts$activityCount.per.minute, na.rm = T)
    output$axis3_Activity_counts_per_minute_per_bout_avg <- mean(axis3_bouts$activityCount.per.minute, na.rm = T)
    output$axis3_Activity_counts_per_minute_per_bout_std <- sd(axis3_bouts$activityCount.per.minute, na.rm = T)
    output$VM_Activity_counts_per_minute_per_bout_avg <- mean(VM_bouts$activityCount.per.minute, na.rm = T)
    output$VM_Activity_counts_per_minute_per_bout_std <- sd(VM_bouts$activityCount.per.minute, na.rm = T)
    
    output$axis1_Bout_length_avg <- mean(axis1_bouts$duration.minute, na.rm = T)
    output$axis1_Bout_length_std <- sd(axis1_bouts$duration.minute, na.rm = T)
    output$axis2_Bout_length_avg <- mean(axis2_bouts$duration.minute, na.rm = T)
    output$axis2_Bout_length_std <- sd(axis2_bouts$duration.minute, na.rm = T)
    output$axis3_Bout_length_avg <- mean(axis3_bouts$duration.minute, na.rm = T)
    output$axis3_Bout_length_std <- sd(axis3_bouts$duration.minute, na.rm = T)
    output$VM_Bout_length_avg <- mean(VM_bouts$duration.minute, na.rm = T)
    output$VM_Bout_length_std <- sd(VM_bouts$duration.minute, na.rm = T)
    
    output$axis1_Gap_between_bouts_length_avg <- mean(axis1_gaps$duration.minute, na.rm = T)
    output$axis1_Gap_between_bouts_length_std <- sd(axis1_gaps$duration.minute, na.rm = T)
    output$axis2_Gap_between_bouts_length_avg <- mean(axis2_gaps$duration.minute, na.rm = T)
    output$axis2_Gap_between_bouts_length_std <- sd(axis2_gaps$duration.minute, na.rm = T)
    output$axis3_Gap_between_bouts_length_avg <- mean(axis3_gaps$duration.minute, na.rm = T)
    output$axis3_Gap_between_bouts_length_std <- sd(axis3_gaps$duration.minute, na.rm = T)
    output$VM_Gap_between_bouts_length_avg <- mean(VM_gaps$duration.minute, na.rm = T)
    output$VM_Gap_between_bouts_length_std <- sd(VM_gaps$duration.minute, na.rm = T)
    
    output$axis1_Number_of_bouts_per_wear_time_avg <- mean(wearTimes$axis1_numberOfBouts, na.rm = T)
    output$axis1_Number_of_bouts_per_wear_time_std <- sd(wearTimes$axis1_numberOfBouts, na.rm = T)
    output$axis2_Number_of_bouts_per_wear_time_avg <- mean(wearTimes$axis2_numberOfBouts, na.rm = T)
    output$axis2_Number_of_bouts_per_wear_time_std <- sd(wearTimes$axis2_numberOfBouts, na.rm = T)
    output$axis3_Number_of_bouts_per_wear_time_avg <- mean(wearTimes$axis3_numberOfBouts, na.rm = T)
    output$axis3_Number_of_bouts_per_wear_time_std <- sd(wearTimes$axis3_numberOfBouts, na.rm = T)
    output$VM_Number_of_bouts_per_wear_time_avg <- mean(wearTimes$VM_numberOfBouts, na.rm = T)
    output$VM_Number_of_bouts_per_wear_time_std <- sd(wearTimes$VM_numberOfBouts, na.rm = T)
    
    axis1_bouts$avgActivityCount <- axis1_bouts$activityCount.per.minute / axis1_bouts$duration.minute
    axis2_bouts$avgActivityCount <- axis2_bouts$activityCount.per.minute / axis2_bouts$duration.minute
    axis3_bouts$avgActivityCount <- axis3_bouts$activityCount.per.minute / axis3_bouts$duration.minute
    VM_bouts$avgActivityCount <- VM_bouts$activityCount.per.minute / VM_bouts$duration.minute
    
    # Note: in some cases, no value can be calculated for a feature. In that case, NA is used.
    # Rows with NA should be dropped because something was not right with the data.
    output$axis1_Counts_per_min_for_half_life <- median(axis1_bouts$avgActivityCount, na.rm = T)
    output$axis1_Bucket_for_half_life <- private.handleNumericZeroValues(ceiling(which.min(abs(axis1_bouts$avgActivityCount - output$axis1_Counts_per_min_for_half_life)) / 60))
    output$axis2_Counts_per_min_for_half_life <- median(axis2_bouts$avgActivityCount, na.rm = T)
    output$axis2_Bucket_for_half_life <- private.handleNumericZeroValues(ceiling(which.min(abs(axis2_bouts$avgActivityCount - output$axis2_Counts_per_min_for_half_life)) / 60))
    output$axis3_Counts_per_min_for_half_life <- median(axis1_bouts$avgActivityCount, na.rm = T)
    output$axis3_Bucket_for_half_life <- private.handleNumericZeroValues(ceiling(which.min(abs(axis3_bouts$avgActivityCount - output$axis3_Counts_per_min_for_half_life)) / 60))
    output$VM_Counts_per_min_for_half_life <- median(VM_bouts$avgActivityCount, na.rm = T)
    output$VM_Bucket_for_half_life <- private.handleNumericZeroValues(ceiling(which.min(abs(VM_bouts$avgActivityCount - output$VM_Counts_per_min_for_half_life)) / 60))
    
    output$number_of_activityPeriods_per_wear_time = nrow(activityPeriods) / output$Number_of_valid_wear_times
    output$activityPeriods_duration_avg = mean(activityPeriods$duration.minute, na.rm = T)
    output$activityPeriods_duration_median = median(activityPeriods$duration.minute, na.rm = T)
    output$activityPeriods_duration_std = sd(activityPeriods$duration.minute, na.rm = T)
    output$activity_count_in_activityPeriods_per_minute_avg = mean(activityPeriods$activityCounts.per.minute, na.rm = T)
    output$activity_count_in_activityPeriods_per_minute_median = median(activityPeriods$activityCounts.per.minute, na.rm = T)
    output$activity_count_in_activityPeriods_per_minute_std = sd(activityPeriods$activityCounts.per.minute, na.rm = T)
  }
  output
}

# Finds the periods which are likely to be walking - activity periods
activity.period.features <- function(axis.1s) {
  MIN_WINDOW_LENGTH <- 60 # The activity period should be at least 1 minute
  EXTEND_STEP <- 30 # Activity period will be extended by half a minute
  SLIDING_STEP <- 30 # Window for examination would be slided by another half a minute
  MIN_NONZERO_THRESHOLD <- 0.7 # 70% of the window should be non-zero activity counts in order to consider it as an "activity period"
  
  activity.durations <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(activity.durations) <- c("duration.minute", "activityCounts.per.minute")
  
  currSecond <- 1
  while(currSecond <= length(axis.1s)) {
    endSecond <- min(currSecond + MIN_WINDOW_LENGTH, length(axis.1s))
    currentChunk <- axis.1s[currSecond:endSecond]
    
    if(nonZeroPercentage(currentChunk) > MIN_NONZERO_THRESHOLD) { # One activity period is found
      # Looking to find the end of activity period by extending the window size by EXTEND STEP
      while(endSecond <= length(axis.1s) && nonZeroPercentage(currentChunk) > MIN_NONZERO_THRESHOLD) {
        currentChunk <- axis.1s[currSecond:endSecond]
        endSecond <- endSecond + EXTEND_STEP
      }
      activity.durations <- rbind(activity.durations, data.frame(duration.minute = (length(currentChunk) / 60),
                                                                 activityCounts.per.minute = (sum(currentChunk) * (60 / length(currentChunk)))))
      currSecond <- currSecond + length(currentChunk)
    } else {
      currSecond <- currSecond + SLIDING_STEP
    }
    
  }
  activity.durations
}

# Calculates what proportion of data points are greater than 0
nonZeroPercentage <- function(dataChunk) {
  totalLength <- length(dataChunk)
  nonZeroLength <- length(which(dataChunk > 0))
  result <- nonZeroLength / totalLength
  result
}

# Checking if all the axes are present
isValid.participantRawFile <- function(AC.1s) {
  if(is.na(AC.1s$axis1[1]))
    return(FALSE)
  if(is.na(AC.1s$axis2[2]))
    return(FALSE)
  if(is.na(AC.1s$axis3[1]))
    return(FALSE)
  TRUE
}

# In some cases, there is no value calculated. Therefore, cannot be assigned to the output's columns. In those cases NA should be used, since there is nothing for that feature.
private.handleNumericZeroValues <- function(value){
  if(length(value) == 0)
    return(NA)
  value
}
