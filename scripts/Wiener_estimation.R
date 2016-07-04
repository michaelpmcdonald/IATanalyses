# This script performs maximum likelihood estimation of Wiener diffusion
# parameters from IAT data.  The analysis only evaluates parameters for subjects
# whose data were not excluded from the IAT analysis conducted in
# readAndScoreIAT.R .

# This script requires: tbl_iat and tbl_completeSubjects from readAndScoreIAT.R

library(RWiener)
library(bit64)
library(tidyr)
library(ggplot2)
library(data.table)
library(dplyr)
library(dtplyr)
library(IATanalyses)
library(multidplyr)

if(!exists("tbl_iat")){
  tbl_iat <- read.csv("data/tbl_iat.csv")
}

DDMdata <- tbl_iat %>%
  filter(session_id %in% tbl_completeSubjects$session_id) %>%
  select(session_id, pairing, q=trial_latency, resp=trial_error) %>%
  filter(q >= .1 & q < 3)

DDMdata$resp <- ifelse(DDMdata$resp == 0, "upper", "lower")
DDMdata$resp <- factor(DDMdata$resp)

class(DDMdata) <- 'data.frame'

DDMMulti <- partition(DDMdata, session_id)
cluster_library(DDMMulti, "IATanalyses")
cluster_library(DDMMulti, "RWiener")
cluster_library(DDMMulti, "bit64")
cluster_library(DDMMulti, "dplyr")

doMC_start <- proc.time()
results <- DDMMulti %>%
  group_by(session_id, pairing) %>%
  select(q, resp) %>%
  do(DDMestimate(.))
doMC_time <- proc.time()-doMC_start

MCresults <- collect(results)
write.csv(MCresults, "WienerResults.csv", row.names=FALSE)

message("Parameter estimation completed in ", doMC_time[3], " seconds.")
