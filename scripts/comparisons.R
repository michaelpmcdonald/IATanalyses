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

#tbl_iat <- read.csv("data/tbl_iat.csv")

DDMdata <- tbl_iat %>%
  filter(session_id %in% tbl_completeSubjects$session_id) %>%
  select(session_id, pairing, q=trial_latency, resp=trial_error) %>%
  filter(q >= .1 & q < 3)

DDMdata$resp <- ifelse(DDMdata$resp == 0, "upper", "lower")
DDMdata$resp <- factor(DDMdata$resp)

set.seed(0)

DDMsmall <- filter(DDMdata, session_id %in% sample(unique(DDMdata$session_id),5000))

class(DDMsmall) <- 'data.frame'

DDMsmallMulti <- partition(DDMsmall, session_id)
cluster_library(DDMsmallMulti, "IATanalyses")
cluster_library(DDMsmallMulti, "RWiener")
cluster_library(DDMsmallMulti, "bit64")
cluster_library(DDMsmallMulti, "dplyr")

doMC_start <- proc.time()
happy <- DDMsmallMulti %>%
  group_by(session_id, pairing) %>%
  select(q, resp) %>%
  do(DDMestimate(.))
doMC_time <- proc.time()-doMC_start

MCresults <- collect(happy)
write.csv(MCresults, "MCresults5000.csv", row.names=FALSE)
do_start <- proc.time()
happy <- DDMsmall %>%
  group_by(session_id, pairing) %>%
  select(q, resp) %>%
  do(DDMestimate(.))
do_time <- proc.time()-do_start

print(do_time)
print(doMC_time)



# subjects <- unique(DDMsmall$session_id)
#
# DDMresults <- matrix(rep(NA, length(subjects)*7*6), nrow=length(subjects)*7)
# DDMresults[,1] <- as.numeric(rep(subjects, each = 7))
#
# old_start <- proc.time()
# for(i in 1:length(subjects)){
#   for(j in 1:7){
#     data <- DDMsmall %>% filter(session_id == subjects[i] & pairing == j) %>%
#       select(q, resp)
#     # Need to adjust the error trials since they include an extra button press
#     # adjustment is mean(incorrect)-mean(correct)
#     diff <- mean(data[data$resp=="lower",]$q) - mean(data[data$resp=="upper",]$q)
#     if(!is.nan(diff)){
#       data[data$resp=="lower",]$q <- data[data$resp=="lower",]$q-diff
#     }
#     if(wiener_deviance(x=c(1, .1, .1, 1), dat=data)==Inf) {
#       message("Wiener deviance for subject ", subject,", pairing ", j, " could not be evaluated.")
#       next()
#     }
#     fit <- optim(c(1, .001, .001, 1), wiener_deviance, dat=data, method="Nelder-Mead")
#     DDMresults[i*7-(7-j),2:6] <- c(j, fit$par[1], fit$par[2], fit$par[3], fit$par[4])
#   }
# }
# old_time <- proc.time()-old_start
#
# DDMresults <- data.frame(DDMresults)
# names(DDMresults) <- c("session_id","pairing","alpha","tau","beta","delta")
#
# alpha <- summarySE(DDMresults, measurevar = "alpha", groupvars=c("pairing"), na.rm=TRUE)
# tau <- summarySE(DDMresults, measurevar = "tau", groupvars=c("pairing"), na.rm=TRUE)
# beta <- summarySE(DDMresults, measurevar = "beta", groupvars=c("pairing"), na.rm=TRUE)
# delta <- summarySE(DDMresults, measurevar = "delta", groupvars=c("pairing"), na.rm=TRUE)
#
# ggplot(alpha, aes(x=pairing, y=alpha)) +
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=alpha-2*se, ymax=alpha+2*se),
#                 width=.2,
#                 position=position_dodge(.9))
#
# ggplot(tau, aes(x=pairing, y=tau)) +
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=tau-2*se, ymax=tau+2*se),
#                 width=.2,
#                 position=position_dodge(.9))
#
# ggplot(beta, aes(x=pairing, y=beta)) +
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=beta-2*se, ymax=beta+2*se),
#                 width=.2,
#                 position=position_dodge(.9))
#
# ggplot(delta, aes(x=pairing, y=delta)) +
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=delta-2*se, ymax=delta+2*se),
#                 width=.2,
#                 position=position_dodge(.9))
#
