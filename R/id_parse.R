library(stringr)

## CTP ID Parser --------------------------

id_parse_ctp <- function(id){
  id <- toupper(id)
  del.month <- str_extract(id, "\\d+")
  product <- str_extract(id, "[A-Z]+")
  
  del.month[ str_length(del.month) == 3 ] <- 
    paste0(1, del.month[ str_length(del.month) == 3 ] )
  
  StdID <- paste0(product, del.month)     
  
  return(StdID)
}

## RIC ID Parser ---------------------------------

lookup.table <- read.csv("id_list/RICs.csv", stringsAsFactors=FALSE, header=TRUE) %>>%
  filter( str_length(StdID) != 0 )

id_parse_rts <- function(RIC){
  
  pre.suffix <- str_extract(RIC, "[A-Z]+") %>>%
    str_sub( start=1, end=-2)
  
  product <- sapply(pre.suffix, FUN=function(pre){
    nline <- match(pre, lookup.table$RIC)
    product <- lookup.table[nline, "StdID"]
    return(product)
  })
  
  del.year <- str_sub(RIC, -1) %>>%
    { paste0(1, .) }
  
  del.month <- str_sub(RIC, start=-2, end=-2) %>>%
    sapply( FUN=function(RIC.month.letter){
    rts.letters <- c("F", "G", "H", "J", "K", "M", 
                     "N", "Q", "U", "V", "X", "Z")
    del.month <- match(RIC.month.letter, rts.letters)  
    
    if( !is.na(del.month) && str_length(del.month) == 1 ) 
      del.month <- paste0(0, del.month)
    
    return(del.month)
  })
  
  del.time <- paste0(del.year, del.month)
  
  StdID <- paste0(product, del.time) %>>%
    "[<-"( is.na(product), NA)
  
  return(StdID)
}
