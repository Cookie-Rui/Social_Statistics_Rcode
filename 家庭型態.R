library(data.table)
library(stringr)
library(dplyr)
setwd("C:/Users/user/Downloads/r folder (second semester)/2018年家庭收支調查")
load(file="TFIES2018.RData")
setDT(TFIES2018)
str(TFIES2018)
table(TFIES2018$a7)#不考慮加權
count(x=TFIES2018,a7, wt=a20)#考慮加權
TFIES2018 <- (copy(TFIES2018)
              [ , Sex:=factor(x=a7,levels = 1:2,labels = c("男","女"))]
              [])
TFIES2018[ , .N, by=.(Sex)]
TFIES2018[ , .(Freq=sum(a20)), by=.(Sex)]
FreqTable <- (TFIES2018
              [ , .(Freq=sum(a20)), by=.(Sex)]
              [ , Percent:=Freq/sum(Freq)*100]
              [ , ":="(CumFreq=cumsum(Freq), CumPercent=cumsum(Percent))]
              [])
FreqTable

#家庭型態
l.family <- c("單人家庭","夫婦家庭","單親家庭","核心家庭","祖孫家庭","三代同堂","其他")
TFIES2018[ , .N, keyby=.(a18)]
TFIES2018 <- (copy(TFIES2018)
              [ , Family:=floor(a18/100)]
              [ , Family:=factor(x=Family, levels = 1:7, labels = l.family)]
              [])
TFIES2018[ , .(Freq=sum(a20)), keyby=.(Family)]
TFIES2018[ , (Freq=sum(a20)), keyby=.(Family,Sex)]
FreqTable <- (TFIES2018[ , .(Freq=sum(a20)), keyby=.(Family,Sex)]
              [ , .(Family, Freq, Percent=Freq/sum(Freq)*100), keyby=.(Sex)]
              [])
FreqTable
SexTable <- dcast(FreqTable, Family~Sex,sum)

library(ggplot2)
p <- ggplot()+
  geom_bar(data=TFIES2018, aes(x=a7, weight=a20))
p
p <- ggplot()+
  geom_bar(data=TFIES2018, aes(x=a7, weight=a20), 
           color="red", fill="green") + theme_classic()
p
TFIES2018 <- TFIES2018[ , Type:=floor(a18/100)][]
p <- ggplot()+
  geom_bar(data=TFIES2018, aes(x=Family, weight=a20), 
           color="red", fill="green") + theme_classic()
p
p <- ggplot()+
  geom_bar(data=TFIES2018, aes(x=Family, weight=a20, fill=Family), 
           color="red") + theme_classic() + coord_polar()
p
p <- ggplot()+
  geom_bar(data=TFIES2018, aes(x=Family, weight=a20, fill=Family), 
           color="red",size=0.01) + 
  theme_classic() + 
  coord_polar() + 
  facet_wrap(~Sex) + 
  scale_color_viridis_d(option="D", direction=-1)
p

#作業
library(data.table)
library(stringr)
library(dplyr)
setwd("C:/Users/user/Downloads/r folder (second semester)/2018年家庭收支調查")
load(file="TFIES2018.RData")
setDT(TFIES2018)
TFIES2018 <- (copy(TFIES2018)
              [ , Sex:=factor(x=a7,levels = 1:2,labels = c("男","女"))]
              [])
l.family <- c("單人家庭","夫婦家庭","單親家庭","核心家庭","祖孫家庭","三代同堂","其他")
TFIES2018[ , .N, keyby=.(a18)]
TFIES2018 <- (copy(TFIES2018)
              [ , Family:=floor(a18/100)]
              [ , Family:=factor(x=Family, levels = 1:7, labels = l.family)]
              [])
TFIES2018 <- (TFIES2018
              [ , itm600:=ifelse(is.na(itm600), 0, itm600)]
              [ , income:=itm400-itm600]
              [])

POOR <- TFIES2018[ , .(Sex, Family, a20, income)]
incometable <- (POOR[ , .(income_mean=sum(income*a20)/sum(a20)), keyby=.(Family,Sex)]
                [ , .(Family, income_mean), keyby=.(Sex)]
                [])
INCOMEtable <- dcast(incometable,Family~Sex)

poverty_point <- (POOR[order(income), ][cumsum(a20)>=sum(a20)/5, ][ , min(income)][])
A <- POOR[income<=poverty_point, .(under_point=sum(a20)),keyby=.(Family,Sex)]
B <- POOR[,.(all=sum(a20)),keyby=.(Family,Sex)]
povertytable <- A[, .(Family,Sex,POVERTY=under_point/B$all)]
POVERTYtable <- dcast(povertytable, Family~Sex)

POOR[ , Family_code:=as.numeric(Family)][ , Sex_code:=as.numeric(Sex)]
str(POOR)
Median <- c()

for (i in 1:2) {
  for (j in 1:7) {
    Median[j+(i-1)*7] <- POOR[Sex_code %in% i & Family_code %in% j, ][order(income), ][cumsum(a20)>=sum(a20)/2, ][ , min(income)]
  }
}
mediantable <- incometable[ ,.(Sex,Family, median=Median) ]
MEDIANtable <- dcast(Mediantable, Family~Sex)
INCOMEtable
POVERTYtable
MEDIANtable


