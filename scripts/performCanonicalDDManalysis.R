# # This script generate DDM parameter estimations based on per-block latency
# information generated by the previous script, "readAndScoreIAT.R"

# requires: "DDMstatisticsMONYEAR.dat"

# Tables generated:
# tbl_iat : raw IAT experimental data + calculated pairing information
# tbl_D_calc_statistics_long : basic blockwise statistics to be used in D calculations
# tbl_D_calc_statistics : blockwise statistics - one line per subject

library(dplyr)
library(data.table)
library(bit64)
library(tidyr)
library(ggplot2)
source("R/DDMfunctions.R")

DDMstats <- fread("data/DDMstatisticsAug2015.dat")

vaterPracCong <- get.vaTer(DDMstats$session_id, DDMstats$acc3, DDMstats$var3, DDMstats$lat3)
names(vaterPracCong) <- c("session_id", "v", "a", "ter")
vaterPracCong$block <- 3
vaterPracCong$practiceOrTest <- "practice"
vaterPracCong$pairingType <- "White/Good"

vaterPracIncong <- get.vaTer(DDMstats$session_id, DDMstats$acc6, DDMstats$var6, DDMstats$lat6)
names(vaterPracIncong) <- c("session_id", "v", "a", "ter")
vaterPracIncong$block <- 6
vaterPracIncong$practiceOrTest <- "practice"
vaterPracIncong$pairingType <- "White/Bad"

vaterTestCong <- get.vaTer(DDMstats$session_id, DDMstats$acc4, DDMstats$var4, DDMstats$lat4)
names(vaterTestCong) <- c("session_id", "v", "a", "ter")
vaterTestCong$block <- 4
vaterTestCong$practiceOrTest <- "test"
vaterTestCong$pairingType <- "White/Good"

vaterTestIncong <- get.vaTer(DDMstats$session_id, DDMstats$acc7, DDMstats$var7, DDMstats$lat7)
names(vaterTestIncong) <- c("session_id", "v", "a", "ter")
vaterTestIncong$block <- 7
vaterTestIncong$practiceOrTest <- "test"
vaterTestIncong$pairingType <- "White/Bad"

DDMstats <- rbindlist(list(vaterPracCong,vaterPracIncong,vaterTestCong,vaterTestIncong)) %>% arrange(., block)

summaries <- DDMstats %>%
  group_by(block) %>%
  summarise(vbar = mean(v, na.rm=TRUE),
            terbar = mean(ter, na.rm = TRUE),
            abar = mean(a, na.rm = TRUE))

DDMv <- summarySE(DDMstats, measurevar="v", groupvars=c("practiceOrTest","pairingType"), na.rm=TRUE)
DDMa <- summarySE(DDMstats, measurevar="a", groupvars=c("practiceOrTest","pairingType"), na.rm=TRUE)
DDMter <- summarySE(DDMstats, measurevar="ter", groupvars=c("practiceOrTest","pairingType"), na.rm=TRUE)

# Save out generated stats
# write.table(DDMstats, "data/DDMOnlyAug2015.dat")







#DDMstats$deltaV <- (.5*DDMstats$v_PCon+.5*DDMstats$v_TCon)- (.5*DDMstats$v_PInc+.5*DDMstats$v_TInc)
#cor.test(DDMstats$deltaV, DDMstats$dAll)



# cor.test(DDMstats$deltaV, DDMstats$tDiff, use="complete")
# cor.test(DDMstats$dAll, DDMstats$tDiff, use="complete")
#
# white <- filter(DDMstats, raceomb == "6")
# black <- filter(DDMstats, raceomb == "5")
#
# hist(white$dAll)
# hist(white$deltaV)
# cor.test(white$dAll, white$deltaV)
#
#
# hist(black$dAll)
# hist(black$deltaV)
# cor.test(black$dAll, black$deltaV)
#
# xtable(DDMv)
#
# ggplot(DDMv, aes(x=pairingType, y=v, fill=practiceOrTest)) +
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=v-2*se, ymax=v+2*se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))
# DDMa
#
# ggplot(DDMa, aes(x=pairingType, y=a, fill=practiceOrTest)) +
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=a-2*se, ymax=a+2*se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))
# DDMter
#
# ggplot(DDMter, aes(x=pairingType, y=ter, fill=practiceOrTest)) +
#   geom_bar(position=position_dodge(), stat="identity") +
#   geom_errorbar(aes(ymin=ter-2*se, ymax=ter+2*se),
#                 width=.2,                    # Width of the error bars
#                 position=position_dodge(.9))
