setwd("C:/Users/user/Downloads/Data 12-30-2020-20201230")
load(file="MarriageSample.RData")
ls()
str(MarriageSample)
library(dplyr)
library(lubridate)
Marriage <- mutate(.data=MarriageSample, 
                   H.婚齡=time_length(x=interval(start = H.出生年月日,end=結婚年月日), unit = "year"),
                   W.婚齡=time_length(x=interval(start = W.出生年月日,end=結婚年月日), unit = "year"),
                   HW.年齡差距=time_length(x=interval(start=H.出生年月日,end=W.出生年月日),unit="year"))

head(Marriage)

set.seed(5678)
N <- 300
asample <- sample_n(tbl=Marriage, size=N, replace = FALSE)
mean <- mean(asample$H.婚齡)
sd <- sd(asample$H.婚齡)
alpha <- 0.05
zvalue <- qnorm(alpha/2, lower.tail = FALSE)
margin.error <- zvalue * sd / sqrt(N)
upper <- mean + margin.error
lower <- mean - margin.error
CI <- c(lower,upper)
CI
boxplot(x=asample$H.婚齡)

t.test(x=asample$H.婚齡)
tvalue <- qt(alpha/2, lower.tail = FALSE, df=N-1) #查t表的方法
margin.error.t <- tvalue * sd / sqrt(N)
upper.t <- mean + margin.error.t
lower.t <- mean - margin.error.t
CI2 <- c(lower.t,upper.t)
CI2

#假設檢定
alpha = 0.05
#H0:mu=30
t.test(x=asample$H.婚齡, mu=30, alternative="two.sided", conf.level=1-alpha) #雙尾檢定
#H1:mu>30
t.test(x=asample$H.婚齡, mu=30, alternative="greater", conf.level=1-alpha) #右尾檢定
#H1:mu<30
t.test(x=asample$H.婚齡, mu=30, alternative="less", conf.level=1-alpha) #左尾檢定
#t:樣本平均數mean在H0樣本分布中的t值 / df:自由度 / 
(mean-30)/(sd/sqrt(N))


#女性婚齡假設檢定，我們認為女性的平均婚齡不會大於35歲，單尾檢定
#H0:MU=35
#H1:MU<35
alpha <- 0.01
t.test(x=asample$W.婚齡, mu=35, alternative="less", conf.level=1-alpha)
-8.9069 < qt(alpha/2, lower.tail = TRUE, df=N-1)
#樣本平均值落入拒絕區
#拒絕H0
#女性的平均婚齡小於35

count(x=asample,H.婚前婚姻狀況 )
asample <- mutate(.data=asample, 再婚=ifelse((H.婚前婚姻狀況=="未婚"),0,1))
p <- mean(asample$再婚)
alpha <- 0.05
ci <- p+c(-1,1)*qnorm(alpha/2, lower.tail = FALSE)*sqrt(p*(1-p)/N)
ci

prop.test(x=sum(asample$再婚==1), n=length(asample$再婚),
          alternative="two.sided", conf.level=1-alpha) #比例的檢定公式

library(DescTools)
BinomCI(x=sum(asample$再婚==1), n=length(asample$再婚),
        conf.level=1-alpha, method="wald")

#助教課

Marriage <- mutate(.data=MarriageSample, 
                   H.婚齡=time_length(x=interval(start = H.出生年月日,end=結婚年月日), unit = "year"),
                   W.婚齡=time_length(x=interval(start = W.出生年月日,end=結婚年月日), unit = "year"),
                   HW.年齡差距=time_length(x=interval(start=H.出生年月日,end=W.出生年月日),unit="year"))

t.test(x=asample$HW.年齡差距, mu=0, alternative="greater", conf.level=0.95)
