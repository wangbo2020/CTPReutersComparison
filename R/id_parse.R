library(stringr)

id_parse_ctp <- function(id){
  id <- toupper(id)
  del.month <- str_extract(id, "\\d+")
  product <- str_extract(id, "[A-Z]+")
  
  del.month[ str_length(del.month) == 3 ] <- 
    paste0(1, del.month[ str_length(del.month) == 3 ] )
           
  StdID <- paste0(product, del.month)     

  return(StdID)
}

id_parse_rts <- function(RIC){
    
}


# RICs <- unique(rts[, RIC])
# RICs
# 
# ctp_StdID <- id_parse_ctp(ids)
# 
# kinds <- unique(str_extract(ctp_StdID, "[A-Z]+"))
# kinds
# 
# RICs
# 
# str_extract(RICs, kinds)


# http://cn.reuters.com/article/chinaNews/idCNnCN039037220080505
