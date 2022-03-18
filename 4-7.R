setwd("C:/Users/user/Downloads")
Duncan <- readxl::read_excel(path="DuncanSEI.xlsx", sheet="Duncan")
summary(Duncan$Income)
summary(Duncan$Prestige)
with(data=Duncan, plot(x=Income, y=Prestige, 
                       xlim=c(0,85), ylim=c(0,100), las=1, 
                       pch=21, col="blue", bg="orange", cex=2, 
                       xlab="Income", ylab="Prestige"))
with(data=Duncan, text(x=Income, y=Prestige, labels=CaseID, 
                       pos=3, col="forestgreen", cex=1))

with(data=Duncan[4, ], points(x=Income, y=Prestige, pch=22, 
                              col="DarkGoldenrod"))

with(data=Duncan[4, ], text(x=Income, y=Prestige, labels=paste0("(", Income, ", ", Prestige,")"), 
                            col="navyblue", cex=1, pos=4))
#E1 RULE
xbar <- mean(Duncan$Income)
ybar <- mean(Duncan$Prestige)
abline(h=ybar, lwd=1.5, lty=2, col="maroon")
with(data=Duncan[4, ], points(x=Income, y=ybar, pch=20,
                              col="darkgoldenrod", cex=2))
with(data=Duncan[4, ], text(x=Income, y=ybar, pch=20, col="navyblue", 
                            labels=paste0("(", Income, ",", round(ybar,2),")"),
                            cex=1,pos=4))
with(data=Duncan[4, ], segments(x0=Income, y0=Prestige, x1=Income, y1=ybar, lty=2, lwd=2,
                                col="darkgreen"))
a <- 90
b <- 0.15
abline(a=a,b=b, lwd=2, col="OliveDrab")
with(data=Duncan[4, ], points(x=Income, y=a+b*Income, pch=20, col="darkgoldenrod", cex=2))
with(data=Duncan[4, ], text(x=Income, y=a+b*Income, pch=20, col="navyblue", 
                            labels=paste0("(", Income, ",", round(a+b*Income,2),")"),
                            cex=1,pos=4))
lm(data=Duncan, formula=Prestige~Income)
summary(lm(data=Duncan, formula=Prestige~Income))
results <- lm(data=Duncan, formula=Prestige~Income)
with(data=Duncan, plot(x=Income, y=Prestige, 
                       xlim=c(0,85), ylim=c(0,100), las=1, 
                       pch=21, col="blue", bg="orange", cex=2, 
                       xlab="Income", ylab="Prestige"))
abline(coef(results), lty=1, lwd=2.5, col="skyblue") 
text(x=20,y=30, labels=paste0("Prestige = ", round(coef(results)[1],2)," + ",
                              round(coef(results)[2], 2)," * Income"),
     pos=4, cex=1.5, col="darkred")
abline(v=xbar, lwd=1.5, lty=2, col="maroon")
abline(h=ybar, lwd=1.5, lty=2, col="maroon") #迴歸直線會通過(x平均,y平均)

#Duncan資料
N <- nrow(Duncan)
# independent & dependent variables
setDT(Duncan)
y <- as.matrix(Duncan[ , .(Prestige) ])
x <- as.matrix(Duncan[ , .(EDU,Income) ])
y
x
one <- matrix(data=1, ncol = 1, nrow=N)
one
k <- ncol(x)
t(one) %*% y %*% (N^(-1))
lm(data=Duncan, formula=Prestige~Income+EDU)
X <- cbind(x,one)
colnames(X)[k+1] <- "Constant"
X
bs <- solve(t(X) %*% X) %*% t(X) %*% y
bs


#作業
#前置作業
setwd("C:/Users/user/Downloads")
library(data.table)
library(dplyr)
DATA1 <- readxl::read_excel(path="台灣中油加油站服務資訊.xlsx")
DATA2 <- readxl::read_excel(path="台塑石油.xlsx")
DATA3 <- readxl::read_excel(path="2020年醫療機構之基本資料.xlsx")
DATA4 <- readxl::read_excel(path="2020鄉鎮市區各項資料彙整.xlsx")
setDT(DATA1)
setDT(DATA2)
setDT(DATA3)
setDT(DATA4)

#計算各鄉鎮市區的加油站和西醫診所
Western <- c("西醫專科診所", "西醫診所")
A <- DATA1[ , .N, keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
B <- DATA2[ , .N, keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
C <- DATA3[型態別 %in% Western , .N, keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
D <- DATA4[ ,.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
FIRST <- merge(D,A,by=intersect(names(D), names(A)), all.x=TRUE)
SECOND <- merge(FIRST,B,keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY), all.x=TRUE)
ALL <- merge(SECOND,C,keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY), all.x=TRUE)
ALL <- (ALL[, ":="(N.x=ifelse(is.na(N.x), 0,N.x ),
                  N.y=ifelse(is.na(N.y), 0,N.y ),
                  N=ifelse(is.na(N), 0,N )), ]
        [ , 加油站總數:=N.x+N.y])
names(ALL)[5:7] <- c("中油加油站數量","台塑加油站數量","西醫診所數量")

#計算各鄉鎮市區的人口數(以千為單位)以及加油站跟診所的密度，並進行迴歸計算與畫圖
#人口數的部分使用第一次作業的計算結果
ALL <- ALL[ , 每千人口數:=DATA4$總人口數/1000]
ALL <- ALL[ , ":="(加油站密度=加油站總數/每千人口數,
                    西醫診所密度=西醫診所數量/每千人口數)]

lm(data=ALL, formula=西醫診所密度~加油站密度)
summary(lm(data=ALL, formula=西醫診所密度~加油站密度))
result <- lm(data=ALL, formula=西醫診所密度~加油站密度)
with(data=ALL, plot(x=加油站密度, y=西醫診所密度, las=1, 
                       pch=21, col="blue", bg="orange", cex=1.2, 
                       xlab="加油站密度", ylab="西醫診所密度"))
abline(coef(result), lty=1, lwd=4, col="forestgreen") 