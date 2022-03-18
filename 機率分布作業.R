1. student's t distribution

1. 常態
2. 第一個：nu=10
3. 第二個：nu=120
三圖重疊

不只要做成紙本，還要做成pdf檔上傳moodle
**考試也是要用pdf檔--->打在word然後轉成pdf

2. census.2000
以A060為變項，建立一個次數分布表(data.frame)(次數+百分比+累積次數+累積次數百分比)，然後用適當的圖形呈現出來

3. 極限村落
1/3以上的人都是老年人口(不分男女)，就叫做極限村落
老年人口：65歲以上

#常態分布

mu <- 0
sigma <- 1
x <- seq(from=-3, to=3, by=.1)
x
y <- 1/(sigma*sqrt(2*pi)) * exp(-1/2 * ((x - mu)/sigma)^2)
y
plot(x=x, y=y, col="red", lwd=2) #lwd是設定線的寬度

plot(x=x, y=y, type="l", lwd=2, col="blue")
abline(h=0, col="brown")
#lines(x= , y= , .....)
#以後輸出圖表不要用截圖，用輸出的方式

#Census次數分布

setwd("C:/Users/user/Downloads/Census 2000-20201007")
load(file="Census.2000.RData")
str(Census.2000)

hist(x=Census.2000$A020, breaks=0:120, right=FALSE) #次數分布直方圖
#right是設定???????

?hist

table(Census.2000$A010)
#改變結果名稱
#1. ans$x <- c("","".....)
#2. data$v1 <- factor(x=c(1:4),label)

#讀csv檔案
setwd("C:/Users/user/Downloads/Data 10-21-2020-20201021")
library(data.table)
pop <- fread(input="村里戶數人口數單一年齡人口數10909M030.csv", skip=1, encoding="UTF-8", colClasses="character")
Townpop <- fread(input="2019年鄉鎮市區單一年齡人口數.csv", colClasses="character", header=TRUE, sep=";")
#windows預設編碼是big-5,所以如果不是big-5, 要特別設定encoding
#sep則是設定csv檔案的分隔符號(如果不是逗號的話要特別設定)

library(readr)
#也可以用載入這個函式庫，用read_delim載入

#接上續，分析「極限村落」
library(stringr)
pop
# 1. 把變項中的減字號刪
names(pop)
str_replace_all(string=names(pop), pattern=c("-"=""))
names(pop) <- str_replace_all(string=names(pop), pattern=c("-"=""))
str_subset(string=names(pop), pattern="^[0-9]")
library(tidyr)
pop.byage <- subset(x=pop, select=-c(統計年月,戶數,人口數......))



aggregate(formula=Population~區域別代碼+區域別+村里, data=POP, FUN="sum")


