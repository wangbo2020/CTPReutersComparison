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

rts.oi <- select(rts, StdID, Time, HMS, OI.Volume) %>>%
  filter( !is.na(StdID) ) %>>%
  mutate( MS.ori = 1000*as.numeric( str_sub(Time, start=-4, end=-1) ) ) %>>%
  mutate( MS = 500*ifelse(MS.ori <= 500, 1, 0) ) %>>%
  setkey( "StdID", "HMS", "MS")
  
# rts.oi <- rts.oi[ rts.oi[, MS.ori == max(MS.ori), by=list(StdID, HMS, MS) ][, V1] ]

ctp.oi <- select(ctp, StdID, HMS, MS, OI) %>>%
  filter( HMS >= as.ITime("09:00:00", format="%H:%M:%S") ) %>>%
  filter( HMS <= as.ITime("15:15:00", format="%H:%M:%S") ) %>>%
  setkey( "StdID", "HMS", "MS")

join.oi <- inner_join(rts.oi, ctp.oi, by=c("StdID", "HMS", "MS"), all=TRUE)

View( head(join.oi, 500) )
