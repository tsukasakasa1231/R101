library(jsonlite)
library(httr)
options(stringsAsFactors = FALSE)

# 1. Get and convert to an R object ---------------------------------------
Date <- Sys.time()
while(TRUE)
{ 
  # 透過調整if條件來修改頻率
  if(as.numeric(format(Sys.time(), "%M"))!=as.numeric(format(Date, "%M"))&as.numeric(format(Sys.time(), "%M"))%%5==0)
  {
    Date <- Sys.time()
    url <- "http://data.taipei/youbike"
    ubike.list <- fromJSON(content(GET(url),"text"))

    # 2. list -> vector -> matrix -> data.frame -------------------------------
    
    # Select the right node and unlist it --> vector 只選擇"return value"
    ubike.v <- unlist(ubike.list$retVal)
    
    # Fold it by a specified width --> matrix
    ubike.m <- matrix(ubike.v, byrow = T, ncol = 14)
    
    
    # Convert the matrix to dataframe
    ubike.df <- as.data.frame(ubike.m)
    ubike.df["Time"]<-Date
    
    
    write.csv(ubike.df, file=paste(Date, ".csv", sep=""))
  }
  
}

