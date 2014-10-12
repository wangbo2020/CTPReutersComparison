library(pipeR)
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)

rts.files <- paste0("data/Reuters/", list.files("data/Reuters", "gz", recursive=TRUE) )

# check.names <- sapply(1:file.num, FUN=function(i){
#   data.i <- read.csv( rts.files[i] )
#   return( names(data.i) )
# })
# 
# check.names <- t(check.names) %>>% data.frame()
# View( check.names )

header <- paste("RIC", "Date", "Time", 
                "Trade.Price", "Trade.Volume", "Trade.MarketVWAP",
           "Quote.BidPrice", "Quote.BidSize", "Quote.AskPrice", "Quote.AskSize",
           "OI.Volume", sep=",")
writeLines(header, "data/Reuters/Reuters.csv")

log <- foreach(i=1:length(rts.files), .combine="rbind" ) %do% {
  data.i <- read.csv(rts.files[i]) %>>% data.table()
  setnames(data.i, old=names(data.i)[1:3], new=c("RIC", "Date", "Time"))
  
  data.trade <- filter(data.i, Type == "Trade") %>>%
    select( RIC, Date, Time, Price, Volume, Market.VWAP)
  setnames(data.trade, old=names(data.trade)[4:6], 
           new=c("Trade.Price", "Trade.Volume", "Trade.MarketVWAP"))
  
  data.quote <- filter(data.i, Type == "Quote") %>>%
    select( RIC, Date, Time, Bid.Price, Bid.Size, Ask.Price, Ask.Size)
  setnames(data.quote, old=names(data.quote)[4:7], 
           new=c("Quote.BidPrice", "Quote.BidSize", "Quote.AskPrice", "Quote.AskSize") )
  
  data.oi <- filter(data.i, Type ==  "Open Interest") %>>%
    select( RIC, Date, Time, Volume)
  setnames(data.oi, old="Volume", new="OI.Volume")
  
  # debug
  data.oi <- filter(data.oi, !duplicated(data.oi$Time))
  
  data.merge <- merge(data.trade, data.quote, 
                      by=c("RIC", "Date", "Time"), all=TRUE) %>>%
    merge( data.oi, by=c("RIC", "Date", "Time"), all=TRUE)
    
#   message(rts.files[i])
#   message(paste0("i = ", i, ", ", nrow(data.merge), 
#                  " lines of data has been written out."))
  
  write.table(data.merge, "data/Reuters/Reuters.csv", sep=",",
            row.names=FALSE, col.names=FALSE, append=TRUE)
  
  log.i <- c(rts.files[i], nrow(data.merge), ncol(data.merge))
  return(log.i)
}

log <- data.frame(log)
names(log) <- c("Files", "nrow", "ncol")

write.csv(log, "data/Reuters/log.csv", row.names=FALSE)


## Cannot Append in write.csv
# http://stackoverflow.com/questions/7351049/write-csv-a-list-of-unequally-sized-data-frames
# That's a warning, not an error. You can't change append=FALSE with write.csv.  
# ?write.csv says:
#   
#   Attempts to change ‘append’, ‘col.names’, ‘sep’, ‘dec’ or ‘qmethod’ are ignored, 
# with a warning.
# 
# Use write.table with sep="," instead.

## Bug
# task 93 failed - "Join results in 25221 rows; more than 25217 = max(nrow(x),nrow(i)). Check for duplicate key values in i, each of which join to the same group in x over and over again. If that's ok, try including `j` and dropping `by` (by-without-by) so that j runs for each group to avoid the large allocation. If you are sure you wish to proceed, rerun with allow.cartesian=TRUE. Otherwise, please search for this error message in the FAQ, Wiki, Stack Overflow and datatable-help for advice."

## Debug Note 1
# in file 93, data.oi has two lines with same Time
# data.oi[ duplicated(data.oi$Time), ]
# RIC     Date         Time OI.Volume
# 1: CRSMF5 20141009 10:49:36.494    661720
# data.oi[ duplicated(data.oi$Time, fromLast=TRUE), ]
# RIC     Date         Time OI.Volume
# 1: CRSMF5 20141009 10:49:36.494    661722
# Here I just omitted one line.

## Debug Note 2
# file 342 has same problem, but 
