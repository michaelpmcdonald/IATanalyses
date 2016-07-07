# This script provides analysesthe Ratcliff model results
library(dplyr)
library(ggplot2)
library(tidyr)

rats <- fread("extdata/RatcliffResults5000.csv") %>%
  separate(results, c("value", "variable"), "_") %>%
  spread(variable, value)

names(rats) <- c("subject", "pairing", "a", "v", "t0", "sz", "st0", "sv")

smr_a <- summarySE(DDMresults, measurevar = "alpha", groupvars=c("pairing"), na.rm=TRUE)
smr<- summarySE(DDMresults, measurevar = "tau", groupvars=c("pairing"), na.rm=TRUE)
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
