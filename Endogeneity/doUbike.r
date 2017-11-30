library(jsonlite)
library(httr)
library(gplots)
library(ggplot2)
library(readr)
#
library(xts)

library(ggplot2)
library(plyr)
library(plm)
library(stringr)
library(readxl)

library(foreign)
library(zoo)
library(xts)
library(gtrendsR)
library(reshape2)

library(stargazer)
library(lme4)
library(plm)

library(car)
library(dummies)
library(dplyr)
library(gmapsdistance)

options(stringsAsFactors = FALSE)

# Get the files names匯入天氣資料
files = list.files(pattern="*.csv")
# First apply read.csv, then rbind 合併成一張
#lapply(files, function(x) read.csv(x, fileEncoding = "big5",stringsAsFactors = FALSE))
#for(i in 1:length(files)){
#  read.csv(files[i], fileEncoding = "big5",stringsAsFactors = FALSE)
#}

myfiles = do.call(rbind, lapply(files,function(x){
  read.csv(x,fileEncoding = "big5", stringsAsFactors = FALSE)
}))

ubike.df <- myfiles
#data frame
#--------------------

ubike.df$"X"<-NULL


url <- "http://data.taipei/youbike"
ubike.list <- fromJSON(content(GET(url),"text"))
cname <- names(ubike.list$retVal$`0001`)
names(ubike.df) <- c(cname,"Time")
ubike.df<-ubike.df[!is.na(ubike.df$sbi),]

ubike.df["sat"]<-ubike.df$sbi/ubike.df$tot
ubike.df$"sno"<-as.factor(ubike.df$"sno")
ubike.df$Time<-as.POSIXct(ubike.df$Time)
ubike.df["hour"]<-format(ubike.df$Time,"%H")
ubike.df["date"]<-format(ubike.df$Time,"%d")
ubike.df["min"]<-format(ubike.df$Time,"%M")
Sys.setlocale("LC_TIME", "en_US")
ubike.df["dow"]<-weekdays(ubike.df$Time)
ubike.df$hour<-as.numeric(ubike.df$hour)
ubike.df$date<-as.numeric(ubike.df$date)
ubike.df["position"]<-paste (ubike.df$lat,ubike.df$lng,sep = ",", collapse = NULL)

#--------------------
plotmeans(sat ~ sno, main="Heterogeineity across area", data=ubike.df)
#drop 6,82,139,252
ubike.df<-ubike.df[ubike.df$sno!=6&ubike.df$sno!=82&ubike.df$sno!=139&ubike.df$sno!=252,]
plotmeans(sat ~ sno, main="Heterogeineity across area", data=ubike.df)
#--------------------
setwd("/Users/maxchen/Google Drive/Course/Working_on/內生性報告")
write.csv(ubike.df, file=paste("ubike01", ".csv", sep=""))
#--------------------
ubike.df["myid"]<-0




#wheather
#--------------------
#台北：臺北市中正區公園路64號
taipei<-"http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=466920&stname=%25E8%2587%25BA%25E5%258C%2597&datepicker="
#天母：臺北市士林區天母東路116號(三玉國小內)
#"http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0A9C0&stname=%25E5%25A4%25A9%25E6%25AF%258D&datepicker="
#內湖：臺北市內湖區內湖路一段520號(內湖高工內)
neihu<-"http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0A9F0&stname=%25E5%2585%25A7%25E6%25B9%2596&datepicker="
#信義：臺北市信義區市府路1號(台北市政府頂樓)
xinyi<-"http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0AC70&stname=%25E4%25BF%25A1%25E7%25BE%25A9&datepicker="
#松山：地址：臺北市松山區敦化北路1號(臺北市松山運動中心)
songshan<-"http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0AH70&stname=%25E6%259D%25BE%25E5%25B1%25B1&datepicker="
#公館：臺北市大安區羅斯福路四段1號(臺灣大學大氣科學系觀測坪內)
gongguan<-"http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C1A730&stname=%25E5%2585%25AC%25E9%25A4%25A8&datepicker="
#士林：臺北市士林區延平北路6段308號(社子國小內)
#"http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0A9E0&stname=%25E5%25A3%25AB%25E6%259E%2597&datepicker="
#大直：臺北市中山區北安路420號(大直高中內)
dazhi<-"http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0A9A0&stname=%25E5%25A4%25A7%25E7%259B%25B4&datepicker="
#文山：臺北市文山區木柵路4段159巷14-1號(博嘉國小校園內)
wenshan<-"http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0AC80&stname=%25E6%2596%2587%25E5%25B1%25B1&datepicker="
#石牌
#"http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0A9B0&stname=%25E7%259F%25B3%25E7%2589%258C&datepicker="
#南港：臺北市南港區興南路62號(東新國小內)
nangang<-"http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=C0A9G0&stname=%25E5%258D%2597%25E6%25B8%25AF&datepicker="

##import paackage
library(xml2)
library(rvest)

setwd("/Users/maxchen/Google Drive/Course/Working_on/內生性報告/weather/nangang")
for(i in 0:14){
  ##initial url
  url_initial <- nangang
  
  ##set the date
  Date <- as.Date("2017-11-15") + i
  
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
  result["date"]<-Date
  #要改
  result["station"]<-"nag"
  
  write.csv(result, file=paste(Date, ".csv", sep=""))
}
#--------------------
station<-c("taipei","neihu","xinyi","songshan","gongguan","dazhi","wenshan","nangang")
k=0
for(i in 1:length(station))
{
  wheaWD<-"/Users/maxchen/Google Drive/Course/Working_on/內生性報告/weather/"
  wheaWD<-paste(wheaWD,station[i],sep = "", collapse = NULL)
  setwd(wheaWD)
  files = list.files(pattern="*.csv")
  temp = do.call(rbind, lapply(files,function(x){
    read.csv(x,fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  }))
  if(k==0)
  {
    weather<-temp
    k=1
  }
  else
  {
    weather<-rbind(weather,temp)
  }
 
}

weather$X<-NULL
#--------------------
ubike.df["station"]<-"tpe"
ubike.df$"station"[ubike.df$sareaen=="Xinyi Dist."]<-"xyi"
ubike.df$"station"[ubike.df$sareaen=="Nangang Dist."]<-"nag"
ubike.df$"station"[ubike.df$sareaen=="Zhongzheng Dist."]<-"tpe"
ubike.df$"station"[ubike.df$sareaen=="Wanhua Dist."]<-"tpe"
ubike.df$"station"[ubike.df$sareaen=="Wenshan Dist."]<-"wen"
ubike.df$"station"[ubike.df$sareaen=="Daan Dist."]<-"xyi"



#--------------------
colnames(weather) <-c("Obstime","StnPre","SeaPre","temp_hour","Tddewpoint","humidity","wind_speed", "wind_degree","WSGust","WDGust", "precp","precpHour","sunshine","Globrad","visb","year","month","day","hour","Time","station")

total <- merge(ubike.df,weather, by = c("date","hour","station"),all.x=TRUE )
ubike.df["rain"]<-0
ubike.df$rain[ubike.df$precp>0]<-1
