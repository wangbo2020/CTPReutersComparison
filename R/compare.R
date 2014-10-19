library(data.table)
library(pipeR)
library(stringr)
library(dplyr)
library(reshape2)

# source(pre_process)

ctp.StdID <- read.csv("report/ctp.StdID.csv", header=TRUE) %>>%
  setnames( old=names(.), new=c("n", "StdID", "Nrow"))
rts.StdID <- read.csv("report/rts.StdID.csv", header=TRUE) %>>%
  setnames( old=names(.), new=c("n", "StdID", "Nrow"))

StdID.nrow <- merge(ctp.StdID, rts.StdID, by="StdID", all=TRUE)

rts.pair <- select(rts, StdID, Time, HMS, Trade.Price, Trade.Volume, OI.Volume,
                   Quote.AskPrice, Quote.AskSize, Quote.BidPrice, Quote.BidSize) %>>%
  filter( !is.na(StdID) ) %>>%
  mutate( MS.ori = 1000*as.numeric( str_sub(Time, start=-4, end=-1) ) ) %>>%
  mutate( MS = 500*ifelse(MS.ori <= 500, 1, 0) ) %>>%
  setkey( "StdID", "HMS", "MS")
  
# rts.oi <- rts.oi[ rts.oi[, MS.ori == max(MS.ori), by=list(StdID, HMS, MS) ][, V1] ]

ctp.pair <- select(ctp, StdID, HMS, MS, LastPrice, Volume, OI, 
                   AskPrc.1, AskVol.1, BidPrc.1, BidVol.1) %>>%
  filter( HMS >= as.ITime("09:00:00", format="%H:%M:%S") ) %>>%
  filter( HMS <= as.ITime("15:15:00", format="%H:%M:%S") ) %>>%
  setkey( "StdID", "HMS", "MS")

compare <- left_join(rts.pair, ctp.pair, by=c("StdID", "HMS", "MS"), all=TRUE)


View( head(compare, 500) )

write.csv(compare, "report/compare.csv", row.names=FALSE)

