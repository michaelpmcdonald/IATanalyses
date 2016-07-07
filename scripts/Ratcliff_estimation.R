# This script performs maximum likelihood estimation of Wiener diffusion
# parameters from IAT data.  The analysis only evaluates parameters for subjects
# whose data were not excluded from the IAT analysis conducted in
# readAndScoreIAT.R .

# This script requires: tbl_iat and tbl_completeSubjects from readAndScoreIAT.R

library(RWiener)
library(rtdists)
library(bit64)
library(tidyr)
library(ggplot2)
library(data.table)
library(dplyr)
library(dtplyr)
library(IATanalyses)
library(multidplyr)

if(!exists("tbl_iat")){
  tbl_iat <- fread("extdata/tbl_iat.csv")
}

if(!exists("tbl_completeSubjects")){
  tbl_completeSubjects <- fread("data/completeIATsessionIDsAug2015.csv")
}

# Create simpler subject ID variable that doesn't require bit64 to read
tbl_completeSubjects$subject <- 1:length(tbl_completeSubjects$session_id)

DDMdata <- tbl_iat %>%
  filter(session_id %in% tbl_completeSubjects$session_id) %>%
  left_join(., tbl_completeSubjects) %>%
  select(subject, pairing, q=trial_latency, resp=trial_error) %>%
  filter(q >= .1 & q < 3)

DDMdata$resp <- ifelse(DDMdata$resp == 0, "upper", "lower")
DDMdata$resp <- factor(DDMdata$resp)

class(DDMdata) <- 'data.frame'

set.seed(0)

DDMsmall <- filter(DDMdata, subject %in% sample(unique(DDMdata$subject),5000))

class(DDMsmall) <- 'data.frame'

DDMMulti <- partition(DDMdata, subject)
cluster_library(DDMMulti, "IATanalyses")
cluster_library(DDMMulti, "rtdists")
cluster_library(DDMMulti, "dplyr")

# start <- c(runif(2, 0.5, 3), 0.1, runif(3, 0, 0.5))
# names(start) <- c("a", "v", "t0", "sz", "st0", "sv")

doMC_start <- proc.time()
results <- DDMMulti %>%
  group_by(subject, pairing) %>%
  select(q, resp) %>%
  do(diffusionEstimate(.))
doMC_time <- proc.time()-doMC_start

MCresults <- collect(results)
write.csv(MCresults, "extdata/RatcliffResults.csv", row.names=FALSE)

message("Parameter estimation completed in ", round(doMC_time[3],3), " seconds.")



# do_start <- proc.time()
# cow <- DDMsmall %>%
#   group_by(subject, pairing) %>%
#   select(q, resp) %>%
#   do(diffusionEstimate(.))
# do_time <- do_start-proc.time()


