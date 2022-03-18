setwd("C:/Users/user/Downloads/2019年家庭收支調查")
library(haven)
library(data.table)
library(stringr)
TFIES2019 <- setDT(read_spss(file="inc108.sav"))
str(TFIE2019)
TFIES2019 <- (TFIES2019
              [ , itm600:=ifelse(is.na(itm600), 0, itm600)]
              [ , income:=itm400-itm600]
              [])

#每戶平均
group_mean <- TFIES2019[ , sum(income*a20)/sum(a20)]
weighted.mean(TFIES2019$income, TFIES2019$a20)


#每人平均
person_mean <- TFIES2019[ , sum(income * a20)/sum(a20*a8)]

#每戶中位
group_median <- (TFIES2019
                 [ , .(income, a20, a8)]
                 [order(income), ]
                 [cumsum(a20)>=sum(a20)/2, ]
                 [ , min(income)]
                 [])

#每人中位
person_median <- (TFIES2019
                  [ , .(income, a20, a8)]
                  [, income:=income/a8 ]
                  [order(income)]
                  [cumsum(a20*a8)>=sum(a20*a8)/2, ]
                  [ , min(income)]
                  [])
#等值規模：不同年齡時的消費金額的不同比例
#以30歲為1，等值規模=1.5代表該年齡的消費金額是30歲的消費金額的1.5倍

#一個個案是一個家戶，a8變項告訴我們該家戶中有多少人
#bx_y：x代表變項，y代表該家戶中的第y個人
#b1_1表示第一個人的第一個變項，b2_3表示第三個人的第二個變項，以此類推

TFIES2019[ , sum(a8)]
bs <- str_subset(string = names(TFIES2019), pattern = "^b")
b <- str_subset(string = bs, pattern = "^b[0-9]{1,2}(?=_1$)")
b <- str_extract(string = b, pattern = "^b[0-9]{1,2}")
Person <- (copy(TFIES2019)
           [ , Household_ID:=.I] #新增household_ID變項，數值n為第n行
           [rep(x=seq_len(.N), times=a8),] #將每個戶的資料重複n遍，n=a8，即戶內人數
           [ , 戶內序號:=rowid(Household_ID)] #新增新的戶內序號變項，用rowid函數，依照household_ID新增序號(每換一個新的household_ID就重新從1新增)
           [order(Household_ID, 戶內序號),]
           [ , (b):=.SD[ , paste0(b,"_",戶內序號),with=FALSE],
             by=.(戶內序號)]
           [ ,(bs):=NULL]
           [ , .SD, keyby=.(Household_ID, 戶內序號)]
           [])

str(Person, list.len=1000)
Person[ ,.N, by=.(b3)]
Person[ ,.N, keyby=.(b4)]
hist(x=Person$b4, breaks=100, right=F)
library(ggplot2)

items <- str_subset(string=names(Person), pattern="^itm")
Person <- (Person
           [ , (items):=lapply(X=.SD, function(x) ifelse(is.na(x), 0,x)),
             .SDcols=items]
           [])
Person <- (Person
           [ , ":="(Sex=factor(x=b3, levels = 1:2, labels = c("男","女")),
                    Age=ifelse((b4>=95), 95, b4), Weight=a20)]
           [ , FamilyType:=factor(x=floor(a18/100), levels = 1:7,
                                  labels = c("單人家庭","夫婦二人家庭","單親家庭",
                                             "核心家庭","祖孫二代","三代同堂","其他"))]
           [])
AgeSex <- (copy(Person)
           [ , .(人口數=sum(Weight)), by=.(Sex,Age,FamilyType)]
           [])
age <- (copy(AgeSex)[ , 人口數:=ifelse((Sex=="男"),-人口數,人口數)][])
age
p3 <- ggplot()+
  geom_histogram(data=age, aes(x=Age, weight=人口數, group=Sex))
p3
p4 <- ggplot()+
  geom_histogram(data=age, aes(x=Age, weight=人口數, group=Sex),
                 binwidth=1)+coord_flip()
p4
p5 <- ggplot()+
  geom_histogram(data=age, aes(x=Age, weight=人口數, group=Sex, color=Sex, fill=Sex),
                 binwidth=1, alpha=0.7)+
  scale_color_viridis_d(option="D", direction = -1)+
  scale_fill_brewer(palette = "Paired")+
  coord_flip()+theme_void()
p5
#alpha參數：調整顏色透明度 / coord_flip：逆時針翻轉90度\
p6 <- ggplot()+
  geom_histogram(data=age, aes(x=Age, weight=人口數, group=Sex, color=Sex, fill=Sex),
                 binwidth=1, alpha=0.7)+
  scale_color_viridis_d(option="D", direction = -1)+
  scale_fill_brewer(palette = "Paired")+
  coord_flip()+theme_void()+facet_wrap(~FamilyType)
p6
p7 <- ggplot()+
  geom_histogram(data=age, aes(x=Age, weight=人口數, group=Sex, color=Sex, fill=Sex),
                 binwidth=1, alpha=0.7)+
  scale_color_viridis_d(option="D", direction = -1)+
  scale_fill_brewer(palette = "Paired")+
  coord_flip()+theme_void()+facet_wrap(~FamilyType, scales = "free_x")
p7
#facet_wrap：將以上的圖再依據某變項分組表示 / scales參數：設定為"free_x"，代表不使用絕對尺度

#作業-----------------------------------------
setwd("C:/Users/user/Downloads/2019年家庭收支調查")
library(haven)
library(data.table)
library(stringr)
TFIES2019 <- setDT(read_spss(file="inc108.sav"))
str(TFIE2019)
TFIES2019 <- (TFIES2019
              [ , itm600:=ifelse(is.na(itm600), 0, itm600)]
              [ , income:=itm400-itm600]
              [])
TFIES2019 <- (TFIES2019
              [order(income),]
              [ , 戶數累積百分比:=cumsum(a20)/sum(a20)*100]
              [ , 所得累積百分比:=cumsum(income*a20)/sum(income*a20)*100]
              [])
interval <- c(0,20,40,60,80,100)
TFIES2019[ , 所得五等分:=NA]
for (i in 1:5){
  TFIES2019[ , 所得五等分:=
               ifelse(所得累積百分比>interval[i] & 所得累積百分比<=interval[i+1],i,所得五等分)]
}
TFIES2019[ , 所得五等分:=ifelse(is.na(所得五等分),1, 所得五等分)]

bs <- str_subset(string = names(TFIES2019), pattern = "^b")
b <- str_subset(string = bs, pattern = "^b[0-9]{1,2}(?=_1$)")
b <- str_extract(string = b, pattern = "^b[0-9]{1,2}")
Person <- (copy(TFIES2019)
           [ , Household_ID:=.I] #新增household_ID變項，數值n為第n行
           [rep(x=seq_len(.N), times=a8),] #將每個戶的資料重複n遍，n=a8，即戶內人數
           [ , 戶內序號:=rowid(Household_ID)] #新增新的戶內序號變項，用rowid函數，依照household_ID新增序號(每換一個新的household_ID就重新從1新增)
           [order(Household_ID, 戶內序號),]
           [ , (b):=.SD[ , paste0(b,"_",戶內序號),with=FALSE],
             by=.(戶內序號)]
           [ ,(bs):=NULL]
           [ , .SD, keyby=.(Household_ID, 戶內序號)]
           [])
items <- str_subset(string=names(Person), pattern="^itm")
Person <- (Person
           [ , (items):=lapply(X=.SD, function(x) ifelse(is.na(x), 0,x)),
             .SDcols=items]
           [])
Person <- (Person
           [ , ":="(Sex=factor(x=b3, levels = 1:2, labels = c("男","女")),
                    Age=ifelse((b4>95), 95, b4), Weight=a20)]
           [ , 所得五等分:=factor(x=所得五等分, levels = 1:5,
                                  labels = c("第一等分","第二等分","第三等份",
                                             "第四等分","第五等分"))]
           [])
AgeSex <- (copy(Person)
           [ , .(人口數=sum(Weight)), by=.(Sex,Age,所得五等分)]
           [])
age <- (copy(AgeSex)[ , 人口數:=ifelse((Sex=="男"),-人口數,人口數)][])
age[order(所得五等分,Sex,Age)]

p0 <- ggplot()+
  geom_histogram(data=age, aes(x=Age, weight=人口數, group=Sex, color=Sex, fill=Sex),
                 binwidth=1, alpha=0.7)+
  scale_color_viridis_d(option="D", direction = -1)+
  scale_fill_brewer(palette = "Paired")+
  coord_flip()+theme_void()+facet_wrap(~所得五等分, scales = "free_x")
p0
