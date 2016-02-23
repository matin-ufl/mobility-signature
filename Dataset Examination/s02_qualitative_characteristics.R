# A script for a summarization of Qualitative Characteristic of our participants and accelerometer features

setwd("~/Workspaces/R workspace/Mobility Signature Paper/mobility-signature/")

# Selecting required CSV files -----------------------------------------


# Select qualitative characteristics of participants
# >>>> KeyVariables_112515.csv <<<<
keyVariables.df <- read.csv(file.choose())

# Select participant dataset (outliers excluded)
# >>>> d01_original_noOutlier.csv <<<<
participant.df <- read.csv(file.choose())

keyVariables.df <- chosen.ones(keyVariables.df, participant.df$Participant)

target.idx <- keyVariables.df$walkingspeed < 0.8

# Qualitative Features -------------------------------------------------
# Age
tTestOut <- print(t.test(keyVariables.df$age[target.idx], keyVariables.df$age[!target.idx]))
mean_1 <- round(mean(keyVariables.df$age[target.idx]), digits = 2)
mean_2 <- round(mean(keyVariables.df$age[!target.idx]), digits = 2)
sd_1 <- round(sd(keyVariables.df$age[target.idx]), digits = 2)
sd_2 <- round(sd(keyVariables.df$age[!target.idx]), digits = 2)
p.value <- round(tTestOut$p.value, digits = 4)
print(paste("AGE: ", mean_1, " (", sd_1, ") , ", mean_2, " (", sd_2, "), ", p.value, sep = ""))


# Women
n_1 <- length(which(keyVariables.df$women[target.idx]))
prc_1 <- round(n_1 * 100 / length(which(target.idx)), digits = 1)
n_2 <- length(which(keyVariables.df$women[!target.idx]))
prc_2 <- round(n_2 * 100 / length(which(!target.idx)), digits = 1)
p.value <- round(chisq.test(table(keyVariables.df$women, target.idx))$p.value, digits = 4)
print(paste("WOMEN: ", n_1, " (", prc_1, ") , ", n_2, " (", prc_2, ") , ", p.value, sep = ""))


# Race
race_1 <- keyVariables.df$race[target.idx]
hispanic_1 <- which(race_1 == "H")
cauc_1 <- which(race_1 == "C")
afam_1 <- which(race_1 == "A")
race_2 <- keyVariables.df$race[!target.idx]
hispanic_2 <- which(race_2 == "H")
afam_2 <- which(race_2 == "A")
cauc_2 <- which(race_2 == "C")

p.value <- round(chisq.test(table(keyVariables.df$race == "H", target.idx))$p.value, digits = 4)
print(paste("Hispanic: ", length(hispanic_1), " (", round(length(hispanic_1) * 100 / length(race_1), digits = 1), ") , ",
            length(hispanic_2), " (", round(length(hispanic_2) * 100 / length(race_2), digits = 1), ") , ", p.value, sep = ""))

p.value <- round(chisq.test(table(keyVariables.df$race == "C", target.idx))$p.value, digits = 4)
print(paste("Caucasian: ", length(cauc_1), " (", round(length(cauc_1) * 100 / length(race_1), digits = 1), ") , ",
            length(cauc_2), " (", round(length(cauc_2) * 100 / length(race_2), digits = 1), ") , ", p.value, sep = ""))

p.value <- round(chisq.test(table(keyVariables.df$race == "A", target.idx))$p.value, digits = 4)
print(paste("African American: ", length(afam_1), " (", round(length(afam_1) * 100 / length(race_1), digits = 1), ") , ",
            length(afam_2), " (", round(length(afam_2) * 100 / length(race_2), digits = 1), ") , ", p.value, sep = ""))

rm(race_1, race_2, hispanic_1, hispanic_2, cauc_1, cauc_2, afam_1, afam_2)

# SPPB
mean_1 <- round(mean(keyVariables.df$sppb[target.idx]), digits = 1)
sd_1 <- round(sd(keyVariables.df$sppb[target.idx]), digits = 1)
mean_2 <- round(mean(keyVariables.df$sppb[!target.idx]), digits = 1)
sd_2 <- round(sd(keyVariables.df$sppb[!target.idx]), digits = 1)
p.value <- round(t.test(keyVariables.df$sppb[target.idx], keyVariables.df$sppb[!target.idx])$p.value, digits = 4)
print(paste("SPPB Score: ", mean_1, " (", sd_1, ") , ", mean_2, " (", sd_2, "), ", p.value, sep = ""))

# SPPB < 8
sppb_1 <- which(keyVariables.df$sppbLT8[target.idx])
sppb_2 <- which(keyVariables.df$sppbLT8[!target.idx])

p.value <- round(chisq.test(table(keyVariables.df$sppbLT8, target.idx))$p.value, digits = 4)
print(paste("SPPB Score <8: ", length(sppb_1), " (", round(length(sppb_1) * 100 / length(which(target.idx)), digits = 1), ") , ",
            length(sppb_2), " (", round(length(sppb_2) * 100 / length(which(!target.idx)), digits = 1), ") , ", p.value, sep = ""))
rm(sppb_1, sppb_2)

# walking speed
mean_1 <- round(mean(keyVariables.df$walkingspeed[target.idx]), digits = 2)
sd_1 <- round(sd(keyVariables.df$walkingspeed[target.idx]), digits = 2)
mean_2 <- round(mean(keyVariables.df$walkingspeed[!target.idx]), digits = 2)
sd_2 <- round(sd(keyVariables.df$walkingspeed[!target.idx]), digits = 2)
p.value <- round(t.test(keyVariables.df$walkingspeed[target.idx], keyVariables.df$walkingspeed[!target.idx])$p.value, digits = 2)
print(paste("Walking Speed: ", mean_1, " (", sd_1, ") , ", mean_2, " (", sd_2, "), ", p.value, sep = ""))

# BMI
mean_1 <- round(mean(keyVariables.df$bmi[target.idx]), digits = 1)
sd_1 <- round(sd(keyVariables.df$bmi[target.idx]), digits = 1)
mean_2 <- round(mean(keyVariables.df$bmi[!target.idx]), digits = 1)
sd_2 <- round(sd(keyVariables.df$bmi[!target.idx]), digits = 1)
p.value <- round(t.test(keyVariables.df$bmi[target.idx], keyVariables.df$bmi[!target.idx])$p.value, digits = 2)
print(paste("BMI: ", mean_1, " (", sd_1, ") , ", mean_2, " (", sd_2, "), ", p.value, sep = ""))

# Self-reported minutes per week in walking/weight training activities
mean_1 <- round(mean(keyVariables.df$champs[target.idx]), digits = 1)
sd_1 <- round(sd(keyVariables.df$champs[target.idx]), digits = 1)
mean_2 <- round(mean(keyVariables.df$champs[!target.idx]), digits = 1)
sd_2 <- round(sd(keyVariables.df$champs[!target.idx]), digits = 1)
p.value <- round(t.test(keyVariables.df$champs[target.idx], keyVariables.df$champs[!target.idx])$p.value, digits = 3)
print(paste("Self reported shit: ", mean_1, " (", sd_1, ") , ", mean_2, " (", sd_2, "), ", p.value, sep = ""))

# 3MSE score
mean_1 <- round(mean(keyVariables.df$f_3mse[target.idx]), digits = 1)
sd_1 <- round(sd(keyVariables.df$f_3mse[target.idx]), digits = 1)
mean_2 <- round(mean(keyVariables.df$f_3mse[!target.idx]), digits = 1)
sd_2 <- round(sd(keyVariables.df$f_3mse[!target.idx]), digits = 1)
p.value <- round(t.test(keyVariables.df$f_3mse[target.idx], keyVariables.df$f_3mse[!target.idx])$p.value, digits = 3)
print(paste("3MSE: ", mean_1, " (", sd_1, ") , ", mean_2, " (", sd_2, "), ", p.value, sep = ""))

# Health Status
n_1 <- length(which(keyVariables.df$hypertension[target.idx]))
n_2 <- length(which(keyVariables.df$hypertension[!target.idx]))
p.value <- round(chisq.test(table(keyVariables.df$hypertension, target.idx))$p.value, digits = 3)
print(paste("Hypertension: ", n_1, " (", round(n_1 * 100 / length(keyVariables.df$hypertension[target.idx]), digits = 1), ") , ",
            n_2, " (", round(n_2 * 100 / length(keyVariables.df$hypertension[!target.idx]), digits = 1), ") , ", p.value, sep = ""))


n_1 <- length(which(keyVariables.df$diabetes[target.idx]))
n_2 <- length(which(keyVariables.df$diabetes[!target.idx]))
p.value <- round(chisq.test(table(keyVariables.df$diabetes, target.idx))$p.value, digits = 3)
print(paste("Diabetes: ", n_1, " (", round(n_1 * 100 / length(keyVariables.df$diabetes[target.idx]), digits = 1), ") , ",
            n_2, " (", round(n_2 * 100 / length(keyVariables.df$diabetes[!target.idx]), digits = 1), ") , ", p.value, sep = ""))


n_1 <- length(which(keyVariables.df$heart[target.idx]))
n_2 <- length(which(keyVariables.df$heart[!target.idx]))
p.value <- round(chisq.test(table(keyVariables.df$heart, target.idx))$p.value, digits = 3)
print(paste("Heart attack: ", n_1, " (", round(n_1 * 100 / length(keyVariables.df$heart[target.idx]), digits = 1), ") , ",
            n_2, " (", round(n_2 * 100 / length(keyVariables.df$heart[!target.idx]), digits = 1), ") , ", p.value, sep = ""))


n_1 <- length(which(keyVariables.df$stroke[target.idx]))
n_2 <- length(which(keyVariables.df$stroke[!target.idx]))
p.value <- round(chisq.test(table(keyVariables.df$stroke, target.idx))$p.value, digits = 3)
print(paste("Stroke: ", n_1, " (", round(n_1 * 100 / length(keyVariables.df$stroke[target.idx]), digits = 1), ") , ",
            n_2, " (", round(n_2 * 100 / length(keyVariables.df$stroke[!target.idx]), digits = 1), ") , ", p.value, sep = ""))


n_1 <- length(which(keyVariables.df$cancer[target.idx]))
n_2 <- length(which(keyVariables.df$cancer[!target.idx]))
p.value <- round(chisq.test(table(keyVariables.df$cancer, target.idx))$p.value, digits = 3)
print(paste("Cancer: ", n_1, " (", round(n_1 * 100 / length(keyVariables.df$cancer[target.idx]), digits = 1), ") , ",
            n_2, " (", round(n_2 * 100 / length(keyVariables.df$cancer[!target.idx]), digits = 1), ") , ", p.value, sep = ""))


n_1 <- length(which(keyVariables.df$pulmonary[target.idx]))
n_2 <- length(which(keyVariables.df$pulmonary[!target.idx]))
p.value <- round(chisq.test(table(keyVariables.df$pulmonary, target.idx))$p.value, digits = 3)
print(paste("Chronic Pulmonary disease: ", n_1, " (", round(n_1 * 100 / length(keyVariables.df$pulmonary[target.idx]), digits = 1), ") , ",
            n_2, " (", round(n_2 * 100 / length(keyVariables.df$pulmonary[!target.idx]), digits = 1), ") , ", p.value, sep = ""))

rm(n_1, n_2, p.value, prc_1, prc_2, sppb_1, sppb_2)
# Accelerometer Features --------------------------------------------------
afterVIF.df$valid_days <- participant.df$valid_days
afterVIF.df$minutes_wear <- participant.df$minutes_wear
afterVIF.df$minutes_nonWear <- participant.df$minutes_nonWear
feature_summary.file <- "~/Dropbox/Work-Research/Current Directory/Mobility Signature Paper/Documents/021916/output04_feature_summary.csv"
write(print("Feature,Mean (SD), Range (min - max)"), file = feature_summary.file, append = F)
for (j in c(2:32, 34:36)) {
     a <- print(paste(colnames(afterVIF.df)[j], ", ",
                 round(mean(afterVIF.df[, j], na.rm = T), digits = 2), " (", round(sd(afterVIF.df[, j], na.rm = T), digits = 2), "), ",
                 round(median(afterVIF.df[, j]), digits = 2), " (", round(min(afterVIF.df[, j]), digits = 2), " - ", round(max(afterVIF.df[, j]), digits = 2), ")", sep = ""))
     write(a, file = feature_summary.file, append = T)
}
rm(a, j, feature_summary.file)

