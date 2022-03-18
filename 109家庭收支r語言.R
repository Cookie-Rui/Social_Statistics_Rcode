library(data.table)
library(stringr)
setwd("C:/Users/user/Downloads/2018年家庭收支調查")
load(file="TFIES2018.RData")
str(TFIES2018)
summary(TFIES2018$itm400)#所得收入總計
summary(TFIES2018$itm600)#非消費支出，有NA就改為0
setDT(TFIES2018)
TFIES2018 <- (TFIES2018
              [ , itm600:=ifelse(is.na(itm600), 0, itm600)]
              [ , income:=itm400-itm600]
              [])

#每戶平均
group_mean <- TFIES2018[ , sum(income*a20)/sum(a20)]
weighted.mean(TFIES2018$income, TFIES2018$a20)


#每人平均
person_mean <- TFIES2018[ , sum(income * a20)/sum(a20*a8)]

#每戶中位
group_median <- (TFIES2018
           [ , .(income, a20, a8)]
           [order(income), ]
           [cumsum(a20)>=sum(a20)/2, ]
           [ , min(income)]
           [])

#每人中位
person_median <- (TFIES2018
                  [ , .(income, a20, a8)]
                  [order(income/a8), ]
                  [cumsum(a20*a8)>=sum(a20*a8)/2, ]
                  [ , min(income)]
                  [])

#勞倫茲曲線
TFIES2018 <- (TFIES2018
              [order(income),]
              [ , 戶數累積百分比:=cumsum(a20)/sum(a20)*100]
              [ , 所得累積百分比:=cumsum(income*a20)/sum(income*a20)*100]
              [])
with(data=TFIES2018,plot(x=戶數累積百分比, y=所得累積百分比, type="l",
                         col="red", lwd=2.5,
                         main="勞倫茲曲線", cex.main=2))
segments(x0=0,y0=0, x1=100,y1=100, lwd=2.5)
