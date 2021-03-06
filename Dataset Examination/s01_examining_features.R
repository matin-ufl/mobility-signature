# Outline for this script
# 1. Checking features correlation
# 2. VIF and removing correlated features
# 3. Correlation with walking speed Bar Plot

library(ggplot2)
library(reshape2)
library(usdm)
library(glmnet)
library(gridExtra)
setwd("~/Workspaces/R workspace/Mobility Signature Paper/mobility-signature/Dataset Examination/")
source("f01_feature_examination_functions.R")

# Loading the dataset: d01_original_dataset
participant.df <- read.csv(file = file.choose())

# Rejecting outliers ----------------------------------------------
outlier.idx <- detection.outliers(participant.df[, -c(1, 61:63, 72:73)], feature.no = 13, sd.times = 3)
# 10 outliers are detected

# Do not remove outliers, later check their effect on the outcome
#participant.df <- participant.df[-outlier.idx, ]
#rm(outlier.idx)

write.csv(participant.df, file = "~/Dropbox/Work-Research/Current Directory/Mobility Signature Paper/Datasets/V1 - Feb 2015/d01_original.csv")

# Correlation Checking --------------------------------------------
correlation.heatmap(participant.df[, -c(1, 61:63, 72:73)])



# VIF and removing correlated features ----------------------------
data.for.vif <- participant.df[, -c(1, 61:63, 72:73)]
v <- vifstep(data.for.vif, th = 10)
afterVIF.df <- cbind(Participant = participant.df$Participant, data.for.vif[, as.character(v@results$Variables)], participant.df[, 72:73])
outlier.vif.idx <- detection.outliers(afterVIF.df[, -c(1, 33:34)], feature.no = 7, sd.times = 3)
rm(v, data.for.vif)
normal.afterVIF.df <- data.frame(afterVIF.df$Participant, scale(afterVIF.df[, -c(1, 33:34)]), afterVIF.df$walkspeed, afterVIF.df$isTraining)
colnames(normal.afterVIF.df) <- colnames(afterVIF.df)
write.csv(normal.afterVIF.df, file = "~/Dropbox/Work-Research/Current Directory/Mobility Signature Paper/Datasets/V1 - Feb 2015/d02_after_vif_normal.csv", row.names = F)


# Feature Selections ----------------------------------------------
results <- data.frame(matrix(nrow = 0, ncol = 7))
colnames(results) <- c("Method", "RMSE", "R-Squared", "Accuracy", "Sensitivity", "Specificity", "F-Measure")

# Sequential Feature Selection was performed using MATLAB
sfs.selectedFeatures <- c("meanWalkAC_avg", "axis1_Bucket_for_half_life", "axis3_Activity_counts_per_minute_per_bout_std", "noWalk_perBout", "walkspeed")
seq.result <- linear.performance(trainingSet = normal.afterVIF.df[normal.afterVIF.df$isTraining, sfs.selectedFeatures],
                                     testSet = normal.afterVIF.df[!normal.afterVIF.df$isTraining, sfs.selectedFeatures])
results <- rbind(results, data.frame(Method = "Sequential", seq.result))
rm(seq.result)

# LASSO
lasso.result <- lasso.ina.performance(trainingSet = normal.afterVIF.df[normal.afterVIF.df$isTraining, -c(1, ncol(normal.afterVIF.df))],
                                      testSet = normal.afterVIF.df[!normal.afterVIF.df$isTraining, -c(1, ncol(normal.afterVIF.df))],
                                      selected.alpha = 1)

results <- rbind(results, data.frame(Method = "LASSO", lasso.result))
rm(lasso.result)

# Ridge Regression
ridge.result <- lasso.ina.performance(trainingSet = normal.afterVIF.df[normal.afterVIF.df$isTraining, -c(1, ncol(normal.afterVIF.df))],
                                      testSet = normal.afterVIF.df[!normal.afterVIF.df$isTraining, -c(1, ncol(normal.afterVIF.df))],
                                      selected.alpha = 0.00001)

results <- rbind(results, data.frame(Method = "Ridge Regression", ridge.result))
rm(ridge.result)

# Elastic Net
elastic.result <- lasso.ina.performance(trainingSet = normal.afterVIF.df[normal.afterVIF.df$isTraining, -c(1, ncol(normal.afterVIF.df))],
                                      testSet = normal.afterVIF.df[!normal.afterVIF.df$isTraining, -c(1, ncol(normal.afterVIF.df))],
                                      selected.alpha = 0.5)

results <- rbind(results, data.frame(Method = "Elastic Net", elastic.result))
rm(elastic.result)

write.csv(results, file = "~/Dropbox/Work-Research/Current Directory/Mobility Signature Paper/Documents/021916/output01_results.csv", row.names = F)

# Important Features -----------------------------------------------
# At this stage, we would like to see how many features we should include!
# Load the importance feature file (MATLAB produced it)
importance.features <- read.csv(file = file.choose())
sorted.features.idx <- importance.features$index - 1
incremental.result <- incremental.feature.inclusion(trainingSet = normal.afterVIF.df[normal.afterVIF.df$isTraining, -c(1, ncol(normal.afterVIF.df))],
                                                    testSet = normal.afterVIF.df[!normal.afterVIF.df$isTraining, -c(1, ncol(normal.afterVIF.df))],
                                                    sorted.features.idx = sorted.features.idx[1:(length(sorted.features.idx)-2)])

rm(sorted.features.idx)
incremental.result$feature <- factor(importance.features$feature_name[1:nrow(incremental.result)], levels = importance.features$feature_name[1:nrow(incremental.result)])
incremental.result$properName <- factor(proper.names(incremental.result$feature)$properName, levels = proper.names(incremental.result$feature)$properName)
write.csv(incremental.result, file = "~/Dropbox/Work-Research/Current Directory/Mobility Signature Paper/Documents/021916/output02_incremental_results.csv", row.names = F)

# RMSE
g.rmse <- ggplot(data = incremental.result) + geom_line(aes(x = properName, y = RMSE, group = "RMSE"), colour = "red")
g.rmse + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Variable")

# R2
g.rsq <- ggplot(data = incremental.result) + geom_line(aes(x = properName, y = R.Squared, group = "R.Squared"), colour = "red")
g.rsq + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "Variable")

# Sensitivity-Specificity-F.Score
g.sens_spec_fsc <- ggplot(data = incremental.result) + geom_line(aes(x = properName, y = sensitivity, group = "Sensitivity", colour = "Sensitivity"))
g.sens_spec_fsc <- g.sens_spec_fsc + geom_line(aes(x = properName, y = specificity, group = "Specificity", colour = "Specificity"))
g.sens_spec_fsc <- g.sens_spec_fsc + geom_line(aes(x = properName, y = F.score, group = "F-Measure", colour = "F-Measure"))
g.sens_spec_fsc + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y = "", x = "Variable") + scale_colour_manual(values = c("red", "blue", "black"))

# Accuracty
g.acc <- ggplot(data = incremental.result) + geom_line(aes(x = properName, y = accuracy, group = "Accuracy"), colour = "blue")
g.acc + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + scale_colour_manual(values = c("red", "blue", "black")) + labs(x = "Variable")

rm(g.rmse, g.rsq, g.sens_spec_fsc, g.acc)

# Correlation Bar Plot ---------------------------------------------
color.code <- rep("grey", ncol(afterVIF.df) - 2)
color.code[c(26, 16, 28, 14, 4)] <- "black"
correlation.toTarget.barPlot(afterVIF.df[, -c(1, ncol(afterVIF.df))], color.group = color.code, variable.names = proper.names(colnames(afterVIF.df[, -c(1, ncol(afterVIF.df))]))$properName)



# Top 5 Features ---------------------------------------------------
top5.df <- normal.afterVIF.df[, c("Participant", "meanWalkAC_avg", "axis3_Activity_counts_per_minute_per_bout_std", "steps_per_day", "axis2_Counts_per_min_for_half_life", "axis1_Bout_length_avg", "walkspeed", "isTraining")]
top5.result <- linear.performance(trainingSet = top5.df[top5.df$isTraining, -c(1, ncol(top5.df))],
                                 testSet = top5.df[!top5.df$isTraining, -c(1, ncol(top5.df))])
results <- rbind(results, data.frame(Method = "Top 5", top5.result))
rm(top5.result)
write.csv(results, file = "~/Dropbox/Work-Research/Current Directory/Mobility Signature Paper/Documents/022616/output01_results.csv", row.names = F)



# Checking outliers --------------------------------------------------
rm(outlier.idx)
plot.df <- afterVIF.df
plot.df$class <- rep("normal", nrow(plot.df))
plot.df$class[outlier.vif.idx] <- "Outlier"
plot.df$text <- rep("", nrow(plot.df))
plot.df$text[outlier.vif.idx] <- as.character(round(plot.df$walkspeed[outlier.vif.idx], digits = 2))

g1 <- ggplot(data = plot.df) + geom_point(aes(x = steps_per_day, y = peak30_cadence, colour = class, size = class))
g1 <- g1 + scale_colour_manual(values = c("blue", "red")) + scale_size_manual(values = c(3, 7)) + theme_bw() + geom_text(aes(x = steps_per_day, y = peak30_cadence, label = text, hjust = -0.2, just = 0))

g2 <- ggplot(data = plot.df) + geom_point(aes(x = steps_per_day, y = minutes_1000, colour = class, size = class))
g2 <- g2 + scale_colour_manual(values = c("blue", "red")) + scale_size_manual(values = c(3, 7)) + theme_bw() + geom_text(aes(x = steps_per_day, y = minutes_1000, label = text, hjust = -0.2, just = 0))
grid.arrange(g1, g2, ncol = 2)
rm(g1, g2, plot.df)
