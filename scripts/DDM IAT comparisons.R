# This script provides analysesthe Ratcliff model results
library(dplyr)
library(ggplot2)
library(tidyr)

# Pairing key
# 1 practice good vs bad (20)
# 2 practice white vs black (20)
# 3 practice white/good vs black/bad (20)
# 4 test white/good vs black/bad (40)
# 5 switch practice (40)
# 6 practice white/bad vs black/good (20)
# 7 test white/bad vs black/good (40)

rats <- fread("extdata/RatcliffResults5000.csv") %>%
  separate(results, c("value", "variable"), "_", convert=TRUE) %>%
  spread(variable, value)

names(rats) <- c("subject", "pairing", "a", "v", "t0", "sz", "st0", "sv")

iat <- fread("data/iatVerboseOLPSAug2015.csv")

if(!exists("tbl_completeSubjects")){
  tbl_completeSubjects <- fread("data/completeIATsessionIDsAug2015.csv")
}

if(!exists("explicit")){
  explicit <- fread("extdata/explicitOnlyOLPSAug2015.csv")
}


smr_a <- summarySE(rats, measurevar = "a", groupvars=c("pairing"), na.rm=TRUE)
smr_v<- summarySE(rats, measurevar = "v", groupvars=c("pairing"), na.rm=TRUE)
smr_t0 <- summarySE(rats, measurevar = "t0", groupvars=c("pairing"), na.rm=TRUE)

temp1 <- rats %>%
  select(subject, pairing, a) %>%
  spread(pairing, a)
names(temp1) <- c("subject", "a1", "a2", "a3", "a4", "a5", "a6", "a7")

temp2 <- rats %>%
  select(subject, pairing, v) %>%
  spread(pairing, v)
names(temp2) <- c("subject", "v1", "v2", "v3", "v4", "v5", "v6", "v7")

temp3 <- rats %>%
  select(subject, pairing, t0) %>%
  spread(pairing, t0)
names(temp3) <- c("subject", "t01", "t02", "t03", "t04", "t05", "t06", "t07")

ratsOLPS <- temp1 %>% left_join(temp2) %>% left_join(temp3)

# Remove temporary tables
rm(list=c(paste("temp", 1:3, sep="")))


all <- ratsOLPS %>%
  left_join(tbl_completeSubjects) %>%
  left_join(select(iat, session_id, meanLatencyAll, dPractice, dTest, dAll, order)) %>%
  left_join(filter(explicit, session_id %in% tbl_completeSubjects$session_id))

all$deltaVP <- all$v3-all$v6
all$deltaVT <- all$v4-all$v7
all$deltaV <- 0.5*(all$deltaVP+all$deltaVT)

all$deltaTP <- all$t03-all$t06
all$deltaTT <- all$t04-all$t07
all$deltaT <- 0.5*(all$deltaTP+all$deltaTT)

all$deltaAP <- all$a3-all$a6
all$deltaAT <- all$a4-all$a7
all$deltaA <- 0.5*(all$deltaAP+all$deltaAT)

# interesting <- select(all, deltaVP, deltaVT, deltaV, dPractice, dTest, dAll, meanLatencyAll)


# # #   Plots

ggplot(smr_a, aes(x=pairing, y=a)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=a-2*se, ymax=a+2*se),
                width=.2,
                position=position_dodge(.9))

ggplot(smr_v, aes(x=pairing, y=v)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=v-2*se, ymax=v+2*se),
                width=.2,
                position=position_dodge(.9))

ggplot(smr_t0, aes(x=pairing, y=t0)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=t0-2*se, ymax=t0+2*se),
                width=.2,
                position=position_dodge(.9))
