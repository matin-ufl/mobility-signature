# Simply gather all the features (constructed and given) into one single csv for later analysis.

setwd("~/Workspaces/R workspace/Mobility Signature Paper/mobility-signature/Dataset Aggregation/")
source("f01_aggregation_functions.R")

# Select participant features (constructed) : d01_original_dataset_112515.csv
participant.df <- read.csv(file = file.choose())

# Select traditional features (given) : d06_....csv
additional.df <- read.csv(file = file.choose())

# Load mapping between accpid and maskid
load(file.choose())

aggregated.df <- aggregate.features(accel.df = participant.df, additional.df = additional.df, mapping = ACCPID_to_LIFE_maskid)

participant.df <- aggregated.df
write.csv(participant.df, file = "~/Dropbox/Work-Research/Current Directory/Mobility Signature Paper/Datasets/V1 - Feb 2015/d01_original_dataset.csv", row.names = F)
