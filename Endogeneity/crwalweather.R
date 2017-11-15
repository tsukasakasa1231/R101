##import paackage
library(xml2)
library(rvest)

for(i in 0:61){
  ##initial url
  url_initial <- "http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0AC60&stname=%25E4%25B8%2589%25E5%25B3%25BD&datepicker="
  
  ##set the date
  Date <- as.Date("2017-07-01") + i
  
  ##name the url
  url_Date <- paste(url_initial, Date, sep="")
  
  ##get the source code and split
  page_source <- read_html(url_Date, encoding = "UTF-8")
  title <- html_nodes(page_source, ".second_tr th")
  data <- html_nodes(page_source, ".CSSTableGenerator td")
  
  ##translate the source code
  title_content <- html_text(title)
  data_content <- html_text(data)
  
  ##transform title and data into matrix
  data_matrix <- matrix(c(title_content, data_content), 
                        nrow=25, ncol=15, byrow=TRUE)
  
  ##Output
  temp <- gsub("<U+00A0>", "", data_matrix)
  result <- gsub("\\s", "", temp)
  colnames(result)<-result[1,]
  result <- result[-1,]
  time <- unlist(strsplit(as.character(Date), "-"))
  result<-as.data.frame(result)
  result["year"] <- as.numeric(time[1])
  result["month"] <- as.numeric(time[2])
  result["day"] <- as.numeric(time[3])
  for(i in 1:24){
    if(i == result$"觀測時間(LST)ObsTime"[i]){
      result[i,"hour"]=i-1 
    }
  }
  
  write.csv(result, file=paste(Date, ".csv", sep=""))
}
