source("R/pre_process.R")

head(ctp)

ctp.HM <- table(ctp[, HM])
write.csv(ctp.HM, "report/ctp.HM.csv")

ctp.HMS <- table(ctp[, HMS])
write.csv(ctp.HMS, "report/ctp.HMS.csv")

png("report/ctp.HM.png", width=1200, height=800)
barplot(ctp.HM, col='darkblue', border=FALSE)
dev.off()

png("report/ctp.HMS.png", width=1600, height=1200)
barplot(ctp.HMS, col='darkgreen', border=FALSE)
dev.off()

ddlyr(ctp, 

??ddlyr