library(data.table)
library(pipeR)
library(stringr)
library(dplyr)
library(reshape2)

# source(pre_process)

# ctp.StdID <- read.csv("report/ctp.StdID.csv", header=TRUE) %>>%
#   setnames( old=names(.), new=c("n", "StdID", "Nrow"))
# rts.StdID <- read.csv("report/rts.StdID.csv", header=TRUE) %>>%
#   setnames( old=names(.), new=c("n", "StdID", "Nrow"))
# 
# StdID.nrow <- merge(ctp.StdID, rts.StdID, by="StdID", all=TRUE)
# 
# rts.pair <- select(rts, StdID, Time, HMS, Trade.Price, Trade.Volume, OI.Volume,
#                    Quote.AskPrice, Quote.AskSize, Quote.BidPrice, Quote.BidSize) %>>%
#   filter( !is.na(StdID) ) %>>%
#   mutate( MS.ori = 1000*as.numeric( str_sub(Time, start=-4, end=-1) ) ) %>>%
#   mutate( MS = 500*ifelse(MS.ori <= 500, 1, 0) ) %>>%
#   setkey( "StdID", "HMS", "MS")
#   
# # rts.oi <- rts.oi[ rts.oi[, MS.ori == max(MS.ori), by=list(StdID, HMS, MS) ][, V1] ]
# 
# ctp.pair <- select(ctp, StdID, HMS, MS, LastPrice, Volume, OI, 
#                    AskPrc.1, AskVol.1, BidPrc.1, BidVol.1) %>>%
#   filter( HMS >= as.ITime("09:00:00", format="%H:%M:%S") ) %>>%
#   filter( HMS <= as.ITime("15:15:00", format="%H:%M:%S") ) %>>%
#   setkey( "StdID", "HMS", "MS")
# 
# compare <- left_join(rts.pair, ctp.pair, by=c("StdID", "HMS", "MS"), all=TRUE)
# 
# 
# View( head(compare, 500) )
# 
# write.csv(compare, "report/compare.csv", row.names=FALSE)


## OI

rts.oi <- select(rts, StdID, Time, HMS, OI.Volume) %>>%
  filter( !is.na(StdID) ) %>>%
  mutate( MS.ori = 1000*as.numeric( str_sub(Time, start=-4, end=-1) ) ) %>>%
  mutate( MS = 500*ifelse(MS.ori <= 500, 1, 0) ) %>>%
  filter( rts.oi[, MS.ori == max(MS.ori), by=list(StdID, HMS, MS)][, V1] ) %>>%
  setnames( old="HMS", new="HMS.ori") %>>%
  mutate( HMS = ifelse(MS == 0, HMS.ori + 1, HMS.ori ) ) %>>%
  mutate( HMS = as.ITime(HMS, origin=as.ITime("16:00:00") ) ) %>>%
  setkey( "StdID", "HMS", "MS")

ctp.oi <- select(ctp, StdID, HMS, MS, OI) %>>%
  filter( HMS >= as.ITime("09:00:00", format="%H:%M:%S") ) %>>%
  filter( HMS <= as.ITime("15:15:00", format="%H:%M:%S") ) %>>%
  setkey( "StdID", "HMS", "MS")

oi.join <- left_join(ctp.oi, select(rts.oi, StdID, HMS, MS, Time, OI.Volume), 
                     by=c("StdID", "HMS", "MS")) %>>%
  setnames( old=c("OI", "OI.Volume"), new=c("OI.ctp", "OI.rts"))

View(head(oi.join, 500))

write.csv(oi.join, "report/oi.join.csv", row.names=FALSE)


## LastPrice

rts.prc <- select(rts, StdID, Time, HMS, Trade.Price) %>>%
  filter( !is.na(StdID) ) %>>%
  mutate( MS.ori = 1000*as.numeric( str_sub(Time, start=-4, end=-1) ) ) %>>%
  mutate( MS = 500*ifelse(MS.ori <= 500, 1, 0) ) %>>%
  filter( rts.oi[, MS.ori == max(MS.ori), by=list(StdID, HMS, MS)][, V1] ) %>>%
  setnames( old="HMS", new="HMS.ori") %>>%
  mutate( HMS = ifelse(MS == 0, HMS.ori + 1, HMS.ori ) ) %>>%
  mutate( HMS = as.ITime(HMS, origin=as.ITime("16:00:00") ) ) %>>%
  setkey( "StdID", "HMS", "MS")

ctp.prc <- select(ctp, StdID, HMS, MS, LastPrice) %>>%
  filter( HMS >= as.ITime("09:00:00", format="%H:%M:%S") ) %>>%
  filter( HMS <= as.ITime("15:15:00", format="%H:%M:%S") ) %>>%
  setkey( "StdID", "HMS", "MS")

prc.join <- left_join(ctp.prc, select(rts.prc, StdID, HMS, MS, Time, Trade.Price), 
                     by=c("StdID", "HMS", "MS")) %>>%
  setnames( old=c("LastPrice", "Trade.Price"), new=c("Prc.ctp", "Prc.rts"))

View(head(prc.join, 500))

write.csv(prc.join, "report/prc.join.csv", row.names=FALSE)



