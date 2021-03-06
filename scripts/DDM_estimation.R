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
source("R/summarySE.R")

DDMdata <- tbl_iat %>%
  filter(session_id %in% tbl_completeSubjects$session_id) %>%
  select(session_id, pairing, q=trial_latency, resp=trial_error) %>%
  filter(q >= .1 & q < 3)

DDMdata$resp <- ifelse(DDMdata$resp == 0, "upper", "lower")
DDMdata$resp <- factor(DDMdata$resp)

set.seed(0)

<<<<<<< HEAD
DDMsmall <- filter(DDMdata, session_id %in% sample(unique(DDMdata$session_id),10))
=======
DDMsmall <- filter(DDMdata, session_id %in% sample(unique(DDMdata$session_id),1000))
>>>>>>> 35d9fbfe5d1dce31e47d43e365e38cb9dde7b923

subjects <- unique(DDMsmall$session_id)

DDMresults <- matrix(rep(NA, length(subjects)*7*6), nrow=length(subjects)*7)
DDMresults[,1] <- as.numeric(rep(subjects, each = 7))
start <- proc.time()
for(i in 1:length(subjects)){
  for(j in 1:7){
    data <- DDMsmall %>% filter(session_id == subjects[i] & pairing == j) %>%
      select(q, resp)
    # Need to adjust the error trials since they include an extra button press
    # adjustment is mean(incorrect)-mean(correct)
    diff <- mean(data[data$resp=="lower",]$q) - mean(data[data$resp=="upper",]$q)
    if(!is.nan(diff)){
      data[data$resp=="lower",]$q <- data[data$resp=="lower",]$q-diff
    }
    data <- filter(data, q < 3 & q > .1)
    if(wiener_deviance(x=c(1, .1, .1, 1), dat=data)==Inf) {
      message("Wiener deviance for subject ", subjects[i],", pairing ", j, " could not be evaluated.")
      next()
    }
    fit <- optim(c(1, .1, .1, 1), wiener_deviance, dat=data, method="Nelder-Mead")
DDMresults[i*7-(7-j),2:6] <- c(j, fit$par[1], fit$par[2], fit$par[3], fit$par[4])
  }
}
proc.time()-start

DDMresults <- data.frame(DDMresults)
names(DDMresults) <- c("session_id","pairing","alpha","tau","beta","delta")

alpha <- summarySE(DDMresults, measurevar = "alpha", groupvars=c("pairing"), na.rm=TRUE)
tau <- summarySE(DDMresults, measurevar = "tau", groupvars=c("pairing"), na.rm=TRUE)
beta <- summarySE(DDMresults, measurevar = "beta", groupvars=c("pairing"), na.rm=TRUE)
delta <- summarySE(DDMresults, measurevar = "delta", groupvars=c("pairing"), na.rm=TRUE)

ggplot(alpha, aes(x=pairing, y=alpha)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=alpha-2*se, ymax=alpha+2*se),
                width=.2,
                position=position_dodge(.9))

ggplot(tau, aes(x=pairing, y=tau)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=tau-2*se, ymax=tau+2*se),
                width=.2,
                position=position_dodge(.9))

ggplot(beta, aes(x=pairing, y=beta)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=beta-2*se, ymax=beta+2*se),
                width=.2,
                position=position_dodge(.9))

ggplot(delta, aes(x=pairing, y=delta)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=delta-2*se, ymax=delta+2*se),
                width=.2,
                position=position_dodge(.9))

