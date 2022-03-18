第一部分

mu <- 0
sigma <- 1
nu1 <- 10
x <- seq(from=-3, to=3, by=.1)
a <- 1/(sigma*sqrt(2*pi)) * exp(-1/2 * ((x - mu)/sigma)^2)
plot(x=x, y=a, type="l", lwd=2, col="blue")
b <- gamma((nu1+1)/2)/(sqrt(pi*nu1)*gamma(nu1/2))*(1+(x^2/nu1))^(-(nu1+1)/2)
lines(x=x, y=b, col="red", type="l", lwd=2)
nu2 <- 120
c <- gamma((nu2+1)/2)/(sqrt(pi*nu2)*gamma(nu2/2))*(1+(x^2/nu2))^(-(nu2+1)/2)
lines(x=x, y=c, col="green", type="l", lwd=2)


第二部分

setwd("C:/Users/user/Downloads/Census 2000-20201007")
load(file="Census.2000.RData")
str(Census.2000)

marriagenumber <- c(1,2,3,4) 

marriage1 <- Census.2000[Census.2000$A060 %in% 1, ]
f1 <- nrow(marriage1)
marriage2 <- Census.2000[Census.2000$A060 %in% 2, ]
f2 <- nrow(marriage2)
marriage3 <- Census.2000[Census.2000$A060 %in% 3, ]
f3 <- nrow(marriage3)
marriage4 <- Census.2000[Census.2000$A060 %in% 4, ]
f4 <- nrow(marriage4)
frequency <- c(f1, f2, f3, f4)

n <- nrow(Census.2000)
f1+f2+f3+f4 == n
p1 <- f1/n * 100
p2 <- f2/n * 100
p3 <- f3/n * 100
p4 <- f4/n * 100
p1+p2+p3+p4==100
percentage <- c(p1,p2,p3,p4)

cf1 <- f1
cf2 <- f1+f2
cf3 <- f1+f2+f3
cf4 <- f1+f2+f3+f4
cumfrequency <- c(cf1,cf2,cf3,cf4)

cp1 <- p1
cp2 <- p1+p2
cp3 <- p1+p2+p3
cp4 <- p1+p2+p3+p4
cumpercentage <- c(cp1,cp2,cp3,cp4)

status <- c("未婚","有配偶同居","離婚分居","配偶死亡")
marriagename <- factor(x=marriagenumber, labels=status)
Marriage <- data.frame(Var=marriagename, freq=frequency, percentage=percentage, cumfreq=cumfrequency, cumper=cumpercentage)
Marriage

第三部分

setwd("C:/Users/user/Downloads/Data 10-21-2020-20201021")
library(data.table)
pop <- fread(input="村里戶數人口數單一年齡人口數10909M030.csv", skip=1, encoding="UTF-8", colClasses="character")
#windows預設編碼是big-5,所以如果不是big-5, 要特別設定encoding
#sep則是設定csv檔案的分隔符號(如果不是逗號的話要特別設定)

library(stringr)
names(pop)
names(pop) <- str_replace_all(string = names(pop), pattern = c("-"=""))
str_subset(string = names(pop), pattern = "^[0~9]")
library(tidyr)
pop.byage <- subset(x=pop, select=-c(統計年月, 戶數, 人口數, 人口數男, 人口數女))
POP <- pivot_longer(data=pop.byage, 
                    cols=str_subset(string = names(pop), pattern = "^[0-9]"),
                    names_to = "Group", values_to = "Population")
POP$性別 <- str_extract(string = POP$Group, pattern = "[男女]$")
POP$年齡 <- as.numeric(str_extract(string = POP$Group, pattern = "(^[0-9]{1,3})"))
str(POP)
POP$Population <- as.numeric(POP$Population)
Total <- aggregate(formula=Population~區域別代碼+區域別+村里,data=POP, FUN="sum")
Total
OLD <- subset(x=POP, subset=(年齡>=65))
old <- aggregate(formula=Population~區域別代碼+區域別+村里,data=OLD,FUN="sum")
old
Total$老年人口 <- old$Population
Total$老年人口比例 <- Total$老年人口/Total$Population*100
極限村落 <- Total[Total$老年人口比例>=33.3333, ]
library(dplyr)
極限村落 <- 極限村落 %>% arrange(老年人口比例)
str(極限村落)


