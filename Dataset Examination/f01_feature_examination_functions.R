# Outlier detection
detection.outliers <- function(participant.df, feature.no = 3, sd.times = 3) {
     counts <- rep(0, nrow(participant.df))
     for(i in 1:nrow(participant.df)) {
          for(j in 2:(ncol(participant.df) - 2)) {
               if(participant.df[i, j] > mean(participant.df[, j]) + sd.times * sd(participant.df[, j])) {
                    counts[i] <- counts[i] + 1
               }
          }
     }
     idx <- which(counts > feature.no)
     idx
}


# Correlation heatmap
correlation.heatmap <- function(participant.df) {
     r <- abs(cor(participant.df[, -c(1, 72:73)]))
     r[lower.tri(r)] <- NA
     plot.df <- melt(r)
     colnames(plot.df) <- c("variable.1", "variable.2", "abs_correlation")
     g <- ggplot(data = plot.df) + geom_tile(aes(x = variable.1, y = variable.2, fill = abs_correlation), colour = "white")
     g <- g + scale_fill_continuous(low = "yellow", high = "red") + theme_gray(base_size = 9) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
     g
}


colinearity.detection <- function(df, threshold = 4) {
     r <- as.data.frame(scale(df))
     v <- vifcor(r, th = threshold)
     result <- data.frame(excluded = v@excluded)
     result
}

correlation.toTarget.barPlot <- function(df, variable.names = colnames(df), color.group = rep("grey", ncol(df))) {
     r <- data.frame(cor(df))
     r <- data.frame(walkspeed = r$walkspeed)
     
     ordered.idx <- order(r$walkspeed, decreasing = F)
     r <- data.frame(correlation = r[ordered.idx, ])
     r$variable <- factor(variable.names[ordered.idx], levels = variable.names[ordered.idx])
     r$color = color.group[ordered.idx]
     r <- r[-nrow(r), ]
     
     g <- ggplot(data = r) + geom_bar(aes(x = variable, y = correlation, fill = variable), stat = "identity")
     g <- g + coord_flip() + scale_fill_manual(values = r$color, guide = F)
     g + labs(y = "Pearson correlation with gait speed", x = "Variable") + theme_bw()
}

performance.check <- function(actual, predicted) {
     temp.fit <- lm(predicted~actual, data = data.frame(actual, predicted))
     summ <- summary(temp.fit)
     RMSE <- round(summ$sigma, digits = 4)
     R.Squared <- round(summ$adj.r.squared, digits = 4)
     tbl <- table(actual < 0.8, predicted < 0.8)
     accuracy <- round(((tbl[1, 1] + tbl[2, 2]) * 100) / sum(tbl), digits = 2)
     sensitivity <- round(tbl[2, 2] / sum(tbl[2, ]), digits = 4)
     specificity <- round(tbl[1, 1] / sum(tbl[1, ]), digits = 4)
     precision <- round(tbl[2, 2] / sum(tbl[, 2]), digits = 4)
     result <- data.frame(RMSE, R.Squared, accuracy, sensitivity, specificity, F.score = round((2 * (precision * sensitivity) / (precision + sensitivity)), digits = 4))
     result
}

linear.performance <- function(trainingSet, testSet) {
     fit <- lm(walkspeed~., data = trainingSet)
     
     fit.testSet <- data.frame(testSet[, -c(ncol(testSet))])
     colnames(fit.testSet) <- colnames(testSet)[-ncol(testSet)]
     
     predicted.values <- predict(fit, fit.testSet)
     performance <- performance.check(actual = testSet$walkspeed, predicted = predicted.values)
     
     performance
}

lasso.ina.performance <- function(trainingSet, testSet, selected.alpha = 1) {
     fit <- cv.glmnet(y = trainingSet$walkspeed, x = as.matrix(trainingSet[, -c(ncol(trainingSet))]), alpha = selected.alpha, nfolds = 5)
     
     predicted.values <- predict(fit, newx = as.matrix(testSet[, -c(ncol(testSet))]), s = "lambda.min")
     
     performance <- performance.check(actual = testSet$walkspeed, predicted = predicted.values)
     performance
}


incremental.feature.inclusion <- function(trainingSet, testSet, sorted.features.idx) {
     result <- data.frame(matrix(nrow = 0, ncol = 6))
     colnames(result) <- c("RMSE", "R2", "Accuracy", "Sensitivity", "Specificity", "F.Measure")
     for(i in 1:length(sorted.features.idx)) {
          curr.result <- linear.performance(trainingSet[, c(sorted.features.idx[1:i], ncol(trainingSet))],
                                            testSet[, c(sorted.features.idx[1:i], ncol(testSet))])
          result <- rbind(result, curr.result)
     }
     result
}


plot.importance.features <- function(importance.results) {
     g1 <- ggplot(data = importance.features) + geom_line(aes(x = feature, y = accuracy, group = "a"))
}

proper.names <- function(feature.names) {
     names <- data.frame(featureName = as.character(feature.names), properName = rep(NA, length(feature.names)))
     names$properName[names$featureName == "meanWalkAC_avg"] <- "A1 AVG AC over activity period"
     names$properName[names$featureName == "axis3_Activity_counts_per_minute_per_bout_std"] <- "A3 STD AC per bout"
     names$properName[names$featureName == "steps_per_day"] <- "Steps per day"
     names$properName[names$featureName == "axis1_Counts_per_min_for_half_life"] <- "A1 AVG AC (half of total activity)"
     names$properName[names$featureName == "axis3_Number_of_bouts_per_wear_time_avg"] <- "A3 AVG NO bouts over wear time"
     names$properName[names$featureName == "axis2_Counts_per_min_for_half_life"] <- "A2 AVG AC (half of total activity)"
     names$properName[names$featureName == "VM_Activity_counts_per_minute_per_bout_avg"] <- "VM AVG AC per bout"
     names$properName[names$featureName == "axis1_Bout_length_avg"] <- "A1 AVG bout length"
     names$properName[names$featureName == "VM_Number_of_bouts_per_wear_time_std"] <- "VM STD NO bouts over wear time"
     names$properName[names$featureName == "peak30_cadence"] <- "Peak 30-min cadence"
     names$properName[names$featureName == "stdWalkAC_max"] <- "A1 STD AC over activity period"
     names$properName[names$featureName == "axis3_proportion"] <- "A3 amplitude proportion"
     names$properName[names$featureName == "axis3_Bout_length_std"] <- "A3 STD bout length"
     names$properName[names$featureName == "axis1_Activity_count_per_wear_time_std"] <- "A1 STD AC over wear time"
     names$properName[names$featureName == "axis1_Activity_counts_per_minute_per_bout_std"] <- "A1 STD AC per bout"
     names$properName[names$featureName == "axis3_Bucket_for_half_life"] <- "A3 AVG bout length (half of total activity)"
     names$properName[names$featureName == "axis1_Gap_between_bouts_length_avg"] <- "A1 AVG activity gap length"
     names$properName[names$featureName == "noWalk_perBout"] <- "NO activity periods per bout"
     names$properName[names$featureName == "axis1_Bucket_for_half_life"] <- "A1 AVG bout length (half of total activity)"
     names$properName[names$featureName == "axis1_Bout_length_std"] <- "A1 STD bout length"
     names$properName[names$featureName == "minutes_1000"] <- "Daily AVG minutes 1000+ AC"
     names$properName[names$featureName == "VM_Gap_between_bouts_length_avg"] <- "VM AVG activity gap length"
     names$properName[names$featureName == "axis2_Bucket_for_half_life"] <- "A2 AVG bout length (half of total activity)"
     names$properName[names$featureName == "axis2_Activity_count_per_wear_time_std"] <- "A2 STD AC over wear time"
     names$properName[names$featureName == "axis3_Activity_count_per_wear_time_std"] <- "A3 STD AC over wear time"
     names$properName[names$featureName == "VM_Bucket_for_half_life"] <- "VM AVG bout length (half of total activity)"
     names$properName[names$featureName == "axis1_Number_of_bouts_per_wear_time_std"] <- "A1 STD NO bouts over wear time"
     names$properName[names$featureName == "VM_Number_of_wear_times"] <- "VM NO wear times"
     names$properName[names$featureName == "axis2_Gap_between_bouts_length_std"] <- "A2 STD activity gap length"
     names$properName[names$featureName == "axis2_Activity_counts_per_minute_per_bout_std"] <- "A2 STD AC per bout"
     names
}


