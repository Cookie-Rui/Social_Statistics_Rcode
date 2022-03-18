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

set.seed(2021)
N <- 1000
bsample <- sample_n(tbl=Marriage, size=N, replace = FALSE)
summary(bsample$H.婚齡)
var(bsample$H.婚齡)
sd(bsample$H.婚齡)

library(psych)
help(package=psych)
describe(x=bsample$H.婚齡) #trimmed：刪除極端值後的平均數 / mad：以絕對值方式算出的標準差
count(x=bsample, W.國籍)

bsample <- mutate(.data=bsample,外配=ifelse((W.國籍)=="本國籍", 0, 1))
describe(x=bsample$H.婚齡)
count(bsample,外配)
describeBy(x=bsample$H.婚齡, group=bsample$外配)

options(scipen = 999)
var.test(formula=H.婚齡~外配,data=bsample) #變異數檢定，~前面為檢驗對象，後面為分組的標準
#要注意這裡並沒有把比較大的變異數放在分子，小的放分母，為求方便，研究假設設為「不等於0」

t.test(formula=H.婚齡~外配, data=bsample, var.equal=FALSE)
