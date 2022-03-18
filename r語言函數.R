class()--->檢察屬性
str()--->同上，但比較完整
head()--->列出前面n筆
tail()--->列出後面n筆
c()--->把括號裡的物件組合成「一個物件」

ex :
c(1,2,5)
c("text", "married", "widowed")
如果組合中的物件有不同類型，則會將全部物件轉換成最接近，最合適的類型
c(1,2,"text")
==>類型會是「文字」

seq()--->建立數列
seq(from=A, to=B, by=C)
第一項是A，最後一項是B，公差是C
seq(from=5, to=20, by=3)
==>5,8,11,14,17,20
也可以寫成 seq(5,20,3),但不建議

seq如果公差是1
則可以直接用冒號
ex : seq(from=5, to=35, by=1)跟 5:35 是一樣的

rep()--->重複
rep(X=......)--->針對X進行重複
rep(x=1:3, times=4)--->(1)
rep(x=1:3, each=4)--->(2)
(1):123123123123
(2):111122223333

rev()--->把括號裡的物件順序顛倒
rev(1:5)==>54321

paste()--->把括號裡的內容拼再一起
paste("v", 1:100)(不指定的話，預設是逗號之間的物件會有間隔)
==>v 1 v 2 v 3 v 4 v 5..........
paste("v", 1:100, sep="-")
==>v-1 v-2 v-3 v-4 v-5 ...........
若不想要有間隔
paste0(~~~~)或者paste(~~~~, sep"")

**產生一個0, 1~4, 5~9, 10~14........的數列
1. ii<- seq(from=5, to=80, by=5)
2. c(0, "1-4", paste(ii, "-", ii+4, sep=""), "85+")

cbind()--->以上下column的方式將括號裡的東西組合起來
rbind()--->同上，只是變成左右row
ex : cbind(1:3,4:6)

unique()--->找出不一樣的元素
duplicated()--->找出一樣的元素
y<-c(1,4,1:6)
用上面兩個檢驗

t()--->矩陣轉置
nchar()--->檢查括號裡面有多少字(包含字母和數字)

ifelse()--->條件判斷
ifelse(條件, 條件成立時執行這裡, 條件不成立時執行這裡)
假設今天有1到80個分數
60分以上加5分
60分以下扣5分
x<-1:80
ifelse((x>=60, x+5, x-5)

data.frame()
data.frame(x=1:10, y=4:7)
長度不一樣時，長度較小的項目會重複，重複到數量一樣

load(file="")--->載入檔案
setwd()--->變更工作目錄
names()--->列出資料中變項的名稱
nrow()--->列出有多少個row(有多少個案(observation / subject / case)


Census操作：

setwd("C:/Users/user/Downloads/Census 2000-20201007")

load(file="Census.2000.RData")

ls()

str(Census.2000)

tail(Census.2000)

names(Census.2000)

nrow(Census.2000)

Census.2000$ID <- 1:nrow(Census.2000)

sampleIDs <- sample(x=1:nrow(Census.2000), size=600, replace=FALSE)

sampleIDs

asample <- Census.2000[Census.2000$ID %in% sampleIDs, ]

圖形操作：

setwd("C:/Users/user/Downloads/Data 10-14-2020-20201014")

library(readxl)

pop <- read_excel(path="百年人口歷年資料.xlsx", sheet="Sheet1", skip=1)

str(pop)

barplot(formula=人口數 ~ Year, data=pop,na.action="na.pass" )
barplot(formula=人口數 ~ Year, data=pop,na.action="na.pass", col="blueviolet" )
barplot(formula=人口數 ~ Year, data=pop,na.action="na.pass", col=rainbow(10) )
barplot(formula=人口數 ~ Year, data=pop,na.action="na.pass", col="red" )
barplot(formula=人口數 ~ Year, data=pop,na.action="na.pass", border="forestgreen" )
barplot(formula=人口數 ~ Year, data=pop,na.action="na.pass", border="forestgreen", col="lightsalmon")
barplot(formula=人口數 ~ Year, data=pop,na.action="na.pass", border="forestgreen", col="lightsalmon", space=0)
barplot(formula=人口數 ~ Year, data=pop,na.action="na.pass", border="forestgreen", col="lightsalmon", space=0, main="1905-2019年台灣人口成長")
barplot(formula=人口數 ~ Year, data=pop,na.action="na.pass", border="forestgreen", col="lightsalmon", space=0, main="1905-2019年台灣人口成長", cex.main=2)
barplot(formula=人口數 ~ Year, data=pop,na.action="na.pass", border="forestgreen", col="lightsalmon", space=0, main="1905-2019年台灣人口成長", cex.main=2, col.main="steelblue" )

plot(x=pop$Year, y=pop$人口數, type="b", col="blue", pch=3, cex=2)
plot(x=pop$Year, y=pop$出生, type="b", col="blue")
plot(x=pop$Year, y=pop$出生, type="b", col="blue", ylim=c(0,450000))
lines(x=pop$Year, y=pop$死亡, type="b", col="red")
points(x=pop$Year, y=pop$死亡, type="b", col="red", pch=0)

census作業:用系統化隨機抽樣，抽1200人

人口作業：將每年的性比例算出來，製作成圖表，並進行社會學推論

系統性隨機抽樣：
1. 清冊==>流水號
2. 設定抽樣比=n / 清冊規模
3. 單一亂數起始點
4. 抽樣間距 interval = 清冊規模 / n
大於清冊要減掉清冊規模，小於就什麼也不用做
最後要把id對應到資料
用head列出幾筆資料

人口作業：
1. 建立性比例(sex ratio)變項
2. 畫圖
3. 解釋



