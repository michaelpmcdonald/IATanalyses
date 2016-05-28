library(dplyr)
library(data.table)
library(bit64)
library(tidyr)
library(ggplot2)

# In addition to completing the IAT, participants complete a variety of self-report
# measures, including demographics, Black/White thermometer ratings (i.e. "How warm do you
# feel toward Black people?"), and a direct favorability rating of Black vs. White.

explicit <- data.table::fread("../extdata/August2015/explicit.txt")

explicit <-  explicit %>%
  filter(attempt == 1) %>%
  # Take lines with "raceomb" in "demographics" or "tBlack" or "tWhite" in "feel" or "feel2"
  filter((questionnaire_name=="demographics" & question_name == "raceomb") |
           ((questionnaire_name=="feel" | questionnaire_name == "feel2") &
              (question_name == "tBlack" | question_name == "tWhite"))) %>%
  filter((question_response != -999) & (question_response != "null")) %>%
  select(session_id, question_name, question_response) %>%
  spread(question_name, question_response)

#all$att <- as.numeric(all$att)
explicit$tBlack <- as.numeric(explicit$tBlack)
explicit$tWhite <- as.numeric(explicit$tWhite)

explicit$tBlack[explicit$tBlack== -9] <- NA
explicit$tWhite[explicit$tWhite== -9] <- NA
explicit$tBlack <- explicit$tBlack-1
explicit$tWhite <- explicit$tWhite-1
explicit$tDiff <- explicit$tWhite - explicit$tBlack

write.table(explicit, "../data/explicitOnlyOLPSAug2015.dat")
