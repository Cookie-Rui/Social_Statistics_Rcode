1. 以普查資料，畫出四種婚姻狀況在不同年齡間的直方圖 & 盒形圖
2. 計算男性、女性、不同年齡(15~70)的勞參率
3. 同2. ，只是變成失業率

library(haven)
setwd("C:/Users/user/Downloads")
MP_2019 <- read_spss(file="lb108.sav")
str(MP_2019)
unique((MP_2019$WorkStatus)

library(dplyr)
SEX <- count(x=MP_2019, Sex, wt=weight/12)
SEXRATE <- SEX[1,2] / SEX[2,2]
SEXRATE
MP <- filter(.data=MP_2019, WorkStatus !="Non-Civilian")
status <- count(x=MP, WorkStatus, wt=weight/12)
#employed是就業者，unemployed是失業者，OLF是非勞動力
#LFP是勞參率
LFP <- summarize(.data=status, 
                 lfpr=sum((WorkStatus %in% c("Employed", "Unemployed"))*n)/sum(n)*100)

LFP


setwd("C:/Users/user/Downloads/midterm exam/Census 2000-20201007")
load(file="Census.2000.RData")

boxplot(x=Census.2000$A020)
str(Census.2000)
boxplot(formula = A020 ~ A060, data =Census.2000, col ="gray",names=c("未婚","有配偶同居","離婚分居","配偶死亡"))    
Census.2000$A060 <- as.numeric(Census.2000$A060)
marriage1 <- Census.2000[Census.2000$A060 %in% 1, ]
plot(x=marriage1$A020, y=marriage1$A060)

library(dplyr)
library(haven)
setwd("C:/Users/user/Downloads")
MP_2019 <- read_spss(file="lb108.sav")
str(MP_2019)
unique(MP_2019$WorkStatus)
MP <- filter(.data=MP_2019, WorkStatus !="Non-Civilian")
A <- group_by(MP, Sex)
str(A)
sexgroup <- summarise(A, count=n(), labor= count(x=A, WorkStatus, wt=weight))
B <- count(MP, Sex, wt=weight)
C <- count(MP[MP$Sex %in% 1,], WorkStatus,wt=weight)
AA <- group_by(group_by(MP,Sex), Age)
str(AA)
max(MP$)
----------------------------------------------

setwd("C:/Users/user/Downloads/midterm exam/Census 2000-20201007")
load(file="Census.2000.RData")
boxplot(formula = A020 ~ A060, data =Census.2000, col ="gray",
        names=c("未婚","有配偶同居","離婚分居","配偶死亡"), xlab="婚姻狀況", ylab="年齡")    

str(Census.2000)
hist(x=Census.2000$A020, freq=TRUE)
Census.2000$A060 <- as.numeric(Census.2000$A060)
marriage1 <- Census.2000[Census.2000$A060 %in% 1, ]
plot(x=marriage1$A020, y=marriage1$A060)
?hist
count(Census.2000, A060)
?count
library(dplyr)
library(haven)

setwd("C:/Users/user/Downloads")
MP_2019 <- read_spss(file="lb108.sav")
str(MP_2019)
unique(MP_2019$WorkStatus)
MP <- filter(.data=MP_2019, WorkStatus !="Non-Civilian")
MPboy <- MP[MP$Sex %in% 1, ]
MPboydata <- as.data.frame(MPboy[ ,c("Age", "WorkStatus", "weight")])
MPboydata
MPboydata$WorkStatus <- as.numeric(MPboydata$WorkStatus %in% c("Employed","Unemployed"))
MPboydata1 <- group_by(MPboydata, Age)
MPboycount <- count(MPboydata1, WorkStatus, wt=weight)
BOYall <- aggregate(formula=n~Age, data=MPboycount, FUN="sum")
BOYlf <- MPboycount[MPboycount$WorkStatus %in% 1, ]


MPboydata1
str(MPboy)







A <- group_by(MP, Sex)
str(A)
sexgroup <- summarise(A, count=n(), labor= count(x=A, WorkStatus, wt=weight))
B <- count(MP, Sex, wt=weight)
count(MP[MP$Sex %in% 1,], WorkStatus,wt=weight)
AA <- group_by(group_by(MP,Sex), Age)
str(AA)
max(MP$)




