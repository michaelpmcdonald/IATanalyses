library(dplyr)
library(data.table)
library(bit64)
library(tidyr)
library(ggplot2)

# Read in experimental data
iat <- fread("extdata/August2015/iat.txt")

# For each subject, map blocks to pairing conditions

iat$pairing <- NA

# 1 practice good vs bad (20)
# 2 practice white vs black (20)
# 3 practice white/good vs black/bad (20)
# 4 test white/good vs black/bad (40)
# 5 switch practice (40)
# 6 practice white/bad vs black/good (20)
# 7 test white/bad vs black/good (40)

# Pairing 1 checks
iat$pairing <- ifelse(iat$block_pairing_definition=="Good,Bad"
                      & (iat$block_number==0 | iat$block_number==1), 1, iat$pairing)
iat$pairing <- ifelse(iat$block_pairing_definition=="Bad,Good"
                      & (iat$block_number==0 | iat$block_number==1), 1, iat$pairing)

# Pairing 2 checks
iat$pairing <- ifelse(iat$block_pairing_definition=="European American,African American"
                      & (iat$block_number==0 | iat$block_number==1), 2, iat$pairing)
iat$pairing <- ifelse(iat$block_pairing_definition=="African American,European American"
                      & (iat$block_number==0 | iat$block_number==1), 2, iat$pairing)

# Pairing 3 checks
iat$pairing <- ifelse(iat$block_pairing_definition=="European American/Good,African American/Bad"
                      & (iat$block_number==2 | iat$block_number==5), 3, iat$pairing)
iat$pairing <- ifelse(iat$block_pairing_definition=="African American/Bad,European American/Good"
                      & (iat$block_number==2 | iat$block_number==5), 3, iat$pairing)

# Pairing 4 checks
iat$pairing <- ifelse(iat$block_pairing_definition=="European American/Good,African American/Bad"
                      & (iat$block_number==3 | iat$block_number==6), 4, iat$pairing)
iat$pairing <- ifelse(iat$block_pairing_definition=="African American/Bad,European American/Good"
                      & (iat$block_number==3 | iat$block_number==6), 4, iat$pairing)
# Pairing 5 checks
iat$pairing <- ifelse(iat$block_pairing_definition=="Good,Bad"
                      & iat$block_number==4, 5, iat$pairing)
iat$pairing <- ifelse(iat$block_pairing_definition=="Bad,Good"
                      & iat$block_number==4, 5, iat$pairing)
iat$pairing <- ifelse(iat$block_pairing_definition=="European American,African American"
                      & iat$block_number==4, 5, iat$pairing)
iat$pairing <- ifelse(iat$block_pairing_definition=="African American,European American"
                      & iat$block_number==4, 5, iat$pairing)

# Pairing 6 checks
iat$pairing <- ifelse(iat$block_pairing_definition=="African American/Good,European American/Bad"
                      & (iat$block_number==2 | iat$block_number==5), 6, iat$pairing)
iat$pairing <- ifelse(iat$block_pairing_definition=="European American/Bad,African American/Good"
                      & (iat$block_number==2 | iat$block_number==5), 6, iat$pairing)
# Pairing 7 checks
iat$pairing <- ifelse(iat$block_pairing_definition=="African American/Good,European American/Bad"
                      & (iat$block_number==3 | iat$block_number==6), 7, iat$pairing)
iat$pairing <- ifelse(iat$block_pairing_definition=="European American/Bad,African American/Good"
                      & (iat$block_number==3 | iat$block_number==6), 7, iat$pairing)

# For each subject, determine an order

# order variable:
# white/good > black/good = 1, black/good > white/good = 2
# relative order:
# "compatible" first = 1, "incompatible" first = 2

# Convert milliseconds to seconds
iat$trial_latency <- iat$trial_latency/1000

olps <- iat %>%
  group_by(session_id, pairing) %>%
  summarize(mean_latency = mean(trial_latency),
            sd_latency = sd(trial_latency),
            var_latency = var(trial_latency),
            error_rate = mean(trial_error),
            trials_under_300ms = mean(trial_latency < .300),
            n_trials = n()) %>%
  filter(!is.na(pairing) & !is.na(mean_latency) & !is.na(var_latency)
         & !is.na(sd_latency) & !is.na(error_rate) & !is.na(n_trials))


hold1 <- olps %>% select(session_id, pairing, mean_latency) %>% spread(pairing, mean_latency)
hold2 <- olps %>% select(session_id, pairing, sd_latency) %>% spread(pairing, sd_latency)
hold3 <- olps %>% select(session_id, pairing, var_latency) %>% spread(pairing, var_latency)
hold4 <- olps %>% select(session_id, pairing, error_rate) %>% spread(pairing, error_rate)
hold5 <- olps %>% select(session_id, pairing, n_trials) %>% spread(pairing, n_trials)
hold6 <- olps %>% select(session_id, pairing, trials_under_300ms) %>% spread(pairing, trials_under_300ms)

temp1 <- merge(hold1, hold2, by.x="session_id", by.y="session_id")
temp2 <- merge(hold3, hold4, by.x="session_id", by.y="session_id")
olps <- merge(temp1, temp2, by.x="session_id", by.y="session_id")
olps <- merge(olps, hold5, by.x="session_id", by.y="session_id")
olps <- merge(olps, hold6, by.x="session_id", by.y="session_id")

rm(list=c("hold1", "hold2", "hold3", "hold4", "hold5", "hold6"))

olps$sum <- rowSums(olps)
olps$complete <- !is.na(olps$sum)

names(olps)<-c("session_id","lat1","lat2","lat3","lat4","lat5","lat6","lat7",
               "sd1", "sd2","sd3","sd4","sd5","sd6","sd7",
               "var1", "var2","var3","var4","var5","var6","var7",
               "err1","err2","err3","err4","err5","err6","err7",
               "n1","n2","n3","n4","n5","n6","n7",
               "sum","complete")
olps$meanLatencyAll <- with(olps, (lat3+lat4+lat6+lat7)/4)

# Shift error rates to accuracy rates
olps$acc1 <- 1 - olps$err1
olps$acc2 <- 1 - olps$err2
olps$acc3 <- 1 - olps$err3
olps$acc4 <- 1 - olps$err4
olps$acc5 <- 1 - olps$err5
olps$acc6 <- 1 - olps$err6
olps$acc7 <- 1 - olps$err7


# # # Perform data reduction based on inclusion/drop criteria

# Create variable for total % trials under 300ms in all paired blocks
# olps$flag

# Create flag variable for ANY paired block with > 40% error rate
olps$flagError40 <- olps %>%
  select(err3, err4, err6, err7) %>%
  apply(1, FUN = max) >= .4


# # IAT calculations #  #

olps$dPractice <- with(olps, dScore(lat6, lat3, sd6, sd3, n6, n3))
olps$dTest <- with(olps, dScore(lat7, lat4, sd7, sd4, n7, n4))
olps$dAll <- (.5*olps$dPractice+.5*olps$dTest)

# Save data file containing IAT results along with all blockwise statistics
write.table(olps, "data/iatVerboseOLPSAug2015.dat")
