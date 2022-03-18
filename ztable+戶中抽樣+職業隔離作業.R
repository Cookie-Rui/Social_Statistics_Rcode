用r把書上的ztable跟moodle的ztable用出來(可以用excel格式輸出)

pnorm(1) #從無線小到正1的面積，0.8413447
pnorm(1.96) #0.9750021
qnorm(0.025) #-1.959964

x <- seq(from=-3,to=3,by=0.01)
plot(x=x,, y=dnorm(x), col="blue", type="l")

用台灣職業資料，計算每個年度的兩性職業隔離指標(歧異指標)，畫成折線圖

以家戶，用系統性抽樣抽1500個家裡住有20歲以上(大於等於20)成人的家戶
其次，再從各個抽樣家戶中，簡單隨機抽樣戶內抽1個人

setwd("C:/Users/user/Downloads/Census 2000-20201007")
load(file="Census.2000.RData")
str(Census.2000)
IDs <- unique(Census.2000$Household_ID)
str(IDs)


職業作業：
setwd("C:/Users/user/Downloads/taiwan occupation")

library(readxl)
ALL <- read_excel(path="taiwan occupation.xlsx", sheet="兩性合計")
BOY <- read_excel(path="taiwan occupation.xlsx", sheet="男性")
GIRL <- read_excel(path="taiwan occupation.xlsx", sheet="女性")

library(tidyr)
boy <- pivot_longer(data=BOY,
                    cols = subset(x=names(BOY), subset=((names(BOY)==c("年","總計")) %in% FALSE)),
                    names_to="occupation", values_to="population")

girl <- pivot_longer(data=GIRL,
                    cols = subset(x=names(GIRL), subset=((names(GIRL)==c("年","總計")) %in% FALSE)),
                    names_to="occupation", values_to="population")

boy$rate <- boy$population / boy$總計
girl$rate <- girl$population / girl$總計
p1p2 <- abs(boy$rate - girl$rate)

dataframe <- data.frame(YEAR=boy$年,職業=boy$occupation,比例差絕對值=p1p2)
dataframe1 <- aggregate(formula=比例差絕對值~YEAR, data=dataframe, FUN="sum")
dataframe1$比例差絕對值 <- dataframe1$比例差絕對值/2
plot(x=dataframe1$比例差絕對值, type="l", col="black", lwd=3, main="台灣2001~2019性別隔離指數折線圖",cex.main=2, xlab="年", ylab="指數")

ztable作業：

x <- seq(from=0.01,to=4.1,by=0.01)
y <- round(1-pnorm(x), 5)
ztable1 <- matrix(data=y, ncol=10, byrow = T)
ztable1
colnames(ztable1) <- c(seq(from=0.01,to=0.10,by=0.01))
rownames(ztable1) <- c(seq(from=0.0,to=4.0,by=0.1))
ztable1

write.csv(ztable1, "ztable1.csv")

z <- seq(from=0,to=4,by=0.01)
a <- round(pnorm(z),4)
b <- round(1-pnorm(z),4)
c <- round(pnorm(z)-0.5,4)
ztable2 <- cbind(z=z,body_proportion=a,tail_proportion=b,proportion_between_mean_and_z=c)

write.csv(ztable2,"ztable2.csv")

戶中抽樣作業：

setwd("C:/Users/user/Downloads/Census 2000-20201007")
load(file="Census.2000.RData")
str(Census.2000)
pop <- Census.2000[Census.2000$A020 >=20, ]

POP <- nrow(pop)
HOUSE <- unique(pop$Household_ID)
M <- length(unique(pop$Household_ID))
n <- 1500
interval <- ceiling(M/n)
startpoint <- sample(x=1:M, size=1, replace=FALSE)
lastnumber <- startpoint + interval * 1499
paste(M,n,startpoint,interval,lastnumber) 
samplenumber <- c(seq(from=startpoint, to=lastnumber, by=interval))
head(samplenumber)

for(i in c(1:1500)){ 
  boolean <- samplenumber[i] > M 
  samplenumber[i] <- samplenumber[i] - boolean * M
}

samplenumber1 <- sort(samplenumber)
samplehouseid <- HOUSE[c(samplenumber1)]
samplepeople <- Census.2000[Census.2000$Household_ID %in% samplehouseid, ]
samplepeople
