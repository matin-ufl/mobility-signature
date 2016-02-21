
# Simply appends three more features to the existing dataset
aggregate.features <- function(accel.df, additional.df, mapping) {
     output.df <- data.frame(matrix(nrow = 0, ncol = (ncol(accel.df) + 3)))
     colnames(output.df) <- c(colnames(accel.df), "steps_per_day", "minutes_1000", "peak30_cadence")
     for (i in 1:nrow(accel.df)) {
          maskId <- mapping$maskid[mapping$accpid == accel.df$Participant[i]]
          if(length(maskId) > 0) {
               j <- which(additional.df$maskid == maskId)
               if(length(j) > 0) {
                    steps_per_day <- additional.df$steps_totalv2[j]
                    minutes_1000 <- additional.df$minutes_1000[j]
                    peak30_cadence <- additional.df$peak30min_cadence[j]
                    newRow <- data.frame(accel.df[i, ], steps_per_day = steps_per_day, minutes_1000 = minutes_1000, peak30_cadence = peak30_cadence)
                    output.df <- data.frame(rbind(output.df, newRow))
               }
          }
     }
     walkspeed <- output.df$walkspeed
     isTraining <- output.df$isTraining
     output.df$walkspeed <- NULL
     output.df$isTraining <- NULL
     output.df$walkspeed <- walkspeed
     output.df$isTraining <- isTraining
     output.df
}