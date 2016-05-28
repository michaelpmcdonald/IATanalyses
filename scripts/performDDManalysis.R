# Perform drift diffusion model calculations

read.delim("../data/")

vaterPracCong <- get.vaTer(olps$session_id, olps$acc3, olps$var3, olps$lat3)
names(vaterPracCong) <- c("session_id", "v_PCon", "a_PCon", "ter_PCon")

vaterPracIncong <- get.vaTer(olps$session_id, olps$acc6, olps$var6, olps$lat6)
names(vaterPracIncong) <- c("session_id", "v_PInc", "a_PInc", "ter_PInc")

vaterTestCong <- get.vaTer(olps$session_id, olps$acc4, olps$var4, olps$lat4)
names(vaterTestCong) <- c("session_id", "v_TCon", "a_TCon", "ter_TCon")

vaterTestIncong <- get.vaTer(olps$session_id, olps$acc7, olps$var7, olps$lat7)
names(vaterTestIncong) <- c("session_id", "v_TInc", "a_TInc", "ter_TInc")

hold1 <- merge(vaterPracCong, vaterPracIncong, by.x = "session_id", by.y = "session_id")
hold2 <- merge(vaterTestCong, vaterTestIncong, by.x = "session_id", by.y = "session_id")
hold3 <- merge(hold1, hold2, by.x = "session_id", by.y = "session_id")
olps <- merge(olps, hold3, by.x = "session_id", by.y = "session_id")

write.table(hold3, "../data/DDMOnlyOLPSAug2015.dat")

rm(c("hold1", "hold2", "hold3"))

olps$deltaV <- (.5*olps$v_PCon+.5*olps$v_TCon)- (.5*olps$v_PInc+.5*olps$v_TInc)
cor.test(olps$deltaV, olps$dAll)

# Merge explicit data with experimental data
olps <- merge(olps, explicit, by.x = "session_id", by.y = "session_id")

# Save out one-line-per-subject file
write.table(olps, "~/tabula/projects/IATdata/olps_Aug2015.dat")

cor.test(olps$deltaV, olps$tDiff, use="complete")
cor.test(olps$dAll, olps$tDiff, use="complete")

white <- filter(olps, raceomb == "6")
black <- filter(olps, raceomb == "5")

hist(white$dAll)
hist(white$deltaV)
cor.test(white$dAll, white$deltaV)


hist(black$dAll)
hist(black$deltaV)
cor.test(black$dAll, black$deltaV)
