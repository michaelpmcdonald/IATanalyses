# This script performs maximum likelihood estimation of Wiener diffusion
# parameters from IAT data.  The analysis only evaluates parameters for subjects
# whose data were not excluded from the IAT analysis conducted in
# readAndScoreIAT.R .

# This script requires: tbl_iat and tbl_completeSubjects from readAndScoreIAT.R

library(RWiener)
library(dplyr)
library(data.table)
library(bit64)
library(tidyr)
library(ggplot2)

DDMdata <- tbl_iat %>%
  filter(session_id %in% tbl_completeSubjects$session_id) %>%
  select(session_id, pairing, q=trial_latency, resp=trial_error)

DDMdata$resp <- ifelse(DDMdata$resp == 0, "upper", "lower")
DDMdata$resp <- factor(DDMdata$resp)

set.seed(1)

DDMsmall <- filter(DDMdata, session_id %in% sample(unique(DDMdata$session_id),100))

subjects <- unique(DDMsmall$session_id)

DDMresults <- matrix(rep(NA, length(subjects)*7*6), nrow=length(subjects)*7)
DDMresults[,1] <- as.numeric(rep(subjects, each = 7))

for(i in 1:length(subjects)){
  for(j in 1:7){
    data <- DDMsmall %>% filter(session_id == subjects[i] & pairing == j) %>% select(q, resp)
    fit <- optim(c(1, .001, .001, 1), wiener_deviance, dat=data, method="Nelder-Mead")
    DDMresults[i*7-(7-j),2:6] <- c(j, fit$par[1], fit$par[2], fit$par[3], fit$par[4])
  }
}

DDMresults <- data.frame(DDMresults)
names(DDMresults) <- c("session_id","pairing","alpha","tau","beta","delta")

delta <- summarySE(DDMresults, measurevar = "delta", groupvars=c("pairing"), na.rm=TRUE)

ggplot(delta, aes(x=pairing, y=delta)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=delta-2*se, ymax=delta+2*se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


