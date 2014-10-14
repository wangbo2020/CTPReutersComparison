library(data.table)
library(dplyr)
library(pipeR)

source("R/id_parse.R")

ctp <- fread("data/CTP/CTP.csv", header=FALSE)
rts <- fread("data/Reuters/Reuters.csv", header=TRUE)

### CTP --------

setnames(ctp, old=names(ctp), 
         new=c("Date", "Time", "MS", "InstrumentID", "LastPrice", 
               "OI", "Turnover", "Volume", 
               "AskPrc.1", "AskPrc.2", "AskPrc.3", "AskPrc.4", "AskPrc.5",
               "AskVol.1", "AskVol.2", "AskVol.3", "AskVol.4", "AskVol.5",
               "BidPrc.1", "BidPrc.2", "BidPrc.3", "BidPrc.4", "BidPrc.5",
               "BidVol.1", "BidVol.2", "BidVol.3", "BidVol.4", "BidVol.5" ))

ctp <- mutate(ctp, HMS = as.ITime(Time, format="%H:%M:%S") ) %>>%
  mutate( HM = as.ITime(Time, format="%H:%M")) %>>% 
  mutate( H = as.ITime(Time, format="%H")) %>>%
  mutate( StdID = id_parse_ctp(InstrumentID) )

# ctp.waste <- filter(ctp.all, HMS < as.ITime("11:30:00", format="%H:%M:%S") |
#                HMS > as.ITime("15:30:00", format="%H:%M:%S") )
# 
# ctp <- filter(ctp.all, HMS >= as.ITime("11:30:00", format="%H:%M:%S"),
#               HMS <= as.ITime("15:30:00", format="%H:%M:%S") )

# nrow(ctp.waste)
# nrow(ctp)

## RTS ----

rts <- mutate(rts, MS = as.numeric(substr(Time, 9, 12)) * 1000 ) %>>%
  mutate( HMS = as.ITime( substr(Time, 1, 8), format="%H:%M:%S") ) %>>%
  mutate( HM = as.ITime(Time, format="%H:%M")) %>>% 
  mutate( H = as.ITime(Time, format="%H"))

get.del.month <- function(RIC){
  RIC <- rts[, RIC]
  RIC.4 <- substr(RIC, 4, 4)
  del.month <- as.numeric( sapply(RIC.4, charToRaw) )
  del.month <- del.month - 69
  return(del.month)
}

rts <- mutate(rts, Exchange = substr(RIC, 1, 1) ) %>>%
  mutate( Product = substr(RIC, 2, 3) ) %>>%
  mutate( DeliveryMonth = get.del.month(RIC) )
  




