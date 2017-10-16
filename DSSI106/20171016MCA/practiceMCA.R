## 主題：[ 使用FactoMineR套件進行MCA分析-簡介與練習]
## 講者：中山政治學研究所 劉正山教授
## 日期：2017.10.16 

### 資料描述：TSCS2013 面訪資料
# 台灣社會變遷基本調查計畫2013第六期第四次：國家認同組  
# 調查執行期間：2013.09.22-2013.12.10
# n=1,952 

### 工具套件的安裝
install.packages("FactoMineR")
install.packages("factoextra")

### 變數挑選
library(dplyr)
load("tscs2013.rda")

tscs2013forMCA <- select(tscs2013, 
                         c(# 核心變數 (core vars)
                           v15r, #「祖國」是哪裡
                           v54ar, v54br, v54cr, v54dr,　# 最有承傳價值的歷史事件
                           v57r, #台灣人/既是台灣人也是中國人/其他
                         　v61r, # 統獨立場
                           v75r,　#　國家領土包括中國大陸
                           v76r, # 國號
                           v89ar, v89br, v89cr, v89dr, 
                         　v89er, v89fr, v89gr, v89hr, v89ir, # 民族－國家
                         　gen.1, gen.2, gen.3, gen.4, gen.5
                           ))

### 將無效值剔除（list-wise deletion）。
tscs2013forMCA.nona <- as.data.frame(na.omit(tscs2013forMCA))
nrow(tscs2013forMCA.nona) #1540

### MCA運算
library(FactoMineR)
library(factoextra)

names(tscs2013forMCA.nona)  
res<-MCA(tscs2013forMCA.nona, ncp=10, graph= F) 

#scree plot: first ten 10 dimensions
fviz_screeplot(res, ncp=10) + labs(title="")

### 變數之間（variables）的關聯分析
# 第一和第二維次上的變數分佈

plot(res, axes=c(1, 2), new.plot=TRUE, choix="var", 
     col.var="black", col.quali.sup="darkgreen", 
     label=c("quali.sup", "var"), 
     invisible=c("ind", "quanti.sup", "quali.sup"), 
     autoLab = "yes",
     title="", cex=1,
     xlim=c(-0.2, 0.4), ylim=c(0, 0.6))

# 拉近
plot(res, axes=c(1, 2), new.plot=TRUE, choix="var", 
     col.var="red", col.quali.sup="darkgreen", 
     label=c("quali.sup", "var"), 
     invisible=c("ind", "quanti.sup"), 
     autoLab = "yes",
     cex=0.55,
     xlim=c(0,0.4), ylim=c(0,0.1)
     )

### 變數類別（categories）相關分析
### 各維度的重要變數類別: cos2 指的是變數與維次之間關係係數的平方(the squared correlations between the variables and the dimensions)。這個數值意味著變數對維次構成的重要性。圖所呈現的是那些變數類別對那些維度貢獻最多。

library(factoextra)
# 貢獻第一維度的重要變數類別
fviz_contrib(res, choice ="var", axes = 1)
# 貢獻第二維度的重要變數類別
fviz_contrib(res, choice ="var", axes = 2)

# 構成第一個維度最重要的前六個類別是（1）不同意「作為華夏子孫，我們在國際上應該盡力將中華文化發揚光大」(v89hr_0)、(2)不同意「中華民族本來就包含很多族群，不應該分離」(v89ar_0)、(3)認為自己是台灣人也是中國人(v57r_3)、(4)自己的祖國是中華民國(v15r_2)、(5)不認為「美麗島事件、黨外民主運動」是重要歷史事件(v54br_0)，以及(6)不認為「二二八事件」是重要歷史事件(v54ar_0)。  由此約略可以發現，第一維次軸線所代表的概念是「民族認同」，且民眾對中華民國的認同比較傾向是民族認同的概念。 
  
# 第二個維次則是一個全新的概念，因為這個軸線明顯不是由一般認知的政治認同測量題所構成。構成第二個維度最重要的類別是(1)不認為「推翻滿清，建立中華民國」是重要歷史事件(v54cr_0)、(2)不認為「八年對日抗戰勝利」是重要歷史事件(v54dr_0)。我們由此可以暫以「中華民國史觀的（不）認同」標記這個維次軸線所代表的概念。由於這是新的維次，以下列出其他構成這個維度的變數類別，依序為不認為「二二八事件」是重要歷史事件(v54ar_0)、不認為「美麗島事件、黨外民主運動」是重要歷史事件(v54br_0)、認為「美麗島事件、黨外民主運動」是重要歷史事件(v54br_1)、以及認為「推翻滿清，建立中華民國」是重要歷史事件(v54cr_1)、認為「二二八事件」是重要歷史事件(v54ar_1)。

### 受訪者在兩個維度的分佈
plot(res, axes=c(1, 2), new.plot=TRUE, choix="ind", 
     col.var="red", col.quali.sup="darkgreen",
     col.ind = "black",
     label=c("var"),
     selectMod ="cos2 20", select="cos2 20",
     xlim=c(-1,1),
     invisible=c("quali.sup", "var"),
     cex=0.7, 
     title="")

### 世代分佈的差異
par(mfrow=c(3,3))
plotellipses(res, keepvar = c("gen.1","gen.2","gen.3", 
                                "gen.4","gen.5"))