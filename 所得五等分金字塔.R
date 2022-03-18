#作業-----------------------------------------
setwd("C:/Users/user/Downloads/2019年家庭收支調查")
library(haven)
library(data.table)
library(stringr)
library(ggplot2)
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
                             labels = c("0~20","20~40","40~60",
                                        "60~80","80~100"))]
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
  coord_flip()+theme_void()+facet_wrap(~所得五等分,scales="free_x")
p0

