source("R/pre_process.R")

head(ctp)

ctp.HM <- table(ctp[, HM])
write.csv(ctp.HM, "report/ctp.HM.csv")

ctp.HMS <- table(ctp[, HMS])
write.csv(ctp.HMS, "report/ctp.HMS.csv")

ctp.StdID <- table(ctp[, StdID])
write.csv(ctp.StdID, "report/ctp.StdID.csv")

png("report/ctp.HM.png", width=1200, height=800)
barplot(ctp.HM, col='darkblue', border=FALSE)
dev.off()

rts.HM <- table(rts[, HM])
write.csv(rts.HM, "report/rts.HM.csv")

rts.HMS <- table(rts[, HMS])
write.csv(rts.HMS, "report/rts.HMS.csv")

rts.StdID <- table(rts[, StdID])
write.csv(rts.StdID, "report/rts.StdID.csv")

png("report/rts.HM.png", width=1200, height=800)
barplot(rts.HM, col='darkblue', border=FALSE)
dev.off()

nrow(ctp)
nrow(rts)
