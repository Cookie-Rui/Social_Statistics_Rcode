#ggplot-------------------------------------------------------
library(dplyr)
library(data.table)
library(sf)
library(ggplot2)
library(readxl)

setwd("C:/Users/user/Downloads/R folder (first semester)/Data 9-23-2020-20200923")
load(file="County.RData")
load(file="Town.RData")

p <- ggplot() + 
  geom_sf(data=Town, fill=NA, color="brown") + 
  geom_sf(data=County, fill=NA, color="forestgreen")
p

Town #(地方創生區不包含外島，所以要把外島的鄉鎮市區去掉)
County
Town <- filter(.data = Town,!(COUNTY %in% c("連江縣","金門縣","澎湖縣")))
County <- filter(.data = County,!(COUNTY %in% c("連江縣","金門縣","澎湖縣")))
p <- ggplot() + 
  geom_sf(data=Town, fill=NA, color="brown") + 
  geom_sf(data=County, fill=NA, color="forestgreen")
p

setwd("C:/Users/user/Downloads/Data-3-10-2021-20210310")
Type <- setDT(read_excel(path="地方創生優先推動地區.xlsx", sheet="地方創生優先推動地區"))
Type <- (Type[ , 類型:=factor(x=類型, levels=c("農山漁村", "中介城鎮","原鄉"), labels=c("農山漁村", "中介城鎮","原鄉"))])
Taiwan <- left_join(x=Town, y=Type, by=intersect(names(Town), names(Type)))
#left_join：把x跟y資料集合起來，以by=......的方式
Taiwan

p0 <- ggplot()+
  geom_sf(data=Taiwan, aes(fill=類型), size=0.01, color="tan")+
  geom_sf(data=County, fill=NA, color="sandybrown", size=0.03)+
  scale_fill_manual(values = terrain.colors(5), na.value=NA)+
  coord_sf(ndiscr = 0) + theme_void()+
  theme(legend.position = c(0.25,0.85), legend.title = element_blank(), 
        legend.text = element_text(size=18))
p0

#ggplot()：空白圖層，在這裡做的設定會套用到所有圖層上，通常不做動作
#geom_sf(Taiwan)：以Taiwan資料，透過aes變數來設定依據「類型」變項來填顏色，最後設定Town邊界的大小和顏色
#geom_sf(County)
#scale_fill_manual：手動調整剛剛填進去的顏色，values變數可以放各種顏色，也可以放如terrain.color的預設色盤，na.value則控制遇到NA時要填什麼顏色

#第二週3/3的data.table-----------------------------------------------

library(data.table)
library(stringr)
setwd("C:/Users/user/Downloads/r folder (second semester)")
POP <- fread(input="2020年底村里戶數人口數單一年齡人口數.csv", skip=1,
             encoding="UTF-8",colClasses="character")
names(POP) #我們讀進來時設定的變項型態為character，但有一些變項是數字，所以要做轉換
names(POP) <- enc2native(names(POP)) #把編碼改為現在這台電腦的編碼，windows：BIG-5，mac：UTF-8

number <- str_subset(string = names(POP), pattern="[數歲男女]")
POP <- (copy(POP)
        [ , (number):=lapply(.SD, as.numeric), .SDcols=number]
        [])

str(POP)
names(POP)
POP<-(copy(POP)
      [,COUNTY:=str_sub(string = 區域別,start=1,end=3)]
      [,TOWN:=str_sub(string = 區域別,start=4,end=-1)]
      [,VILLAGE:=村里]
      [,":="(COUNTY_ID=str_sub(string =區域別代碼,start=1 , end=5),
             TOWN_ID=str_sub(string =區域別代碼,start=1 , end=8),
             V_ID=paste0(str_sub(string =區域別代碼,start=1 , end=8),"-",
                         str_sub(string =區域別代碼,start=9 , end=11)))]
      [])

總戶數<-(copy(POP)
      [,.(總戶數=sum(戶數),總人口數=sum(人口數)),
        by=.(COUNTY_ID,TOWN_ID,COUNTY,TOWN)]
      [,戶量:=總人口數/總戶數]
      [])

population<-melt(data=POP,
                 id.vars = c("COUNTY_ID","TOWN_ID","V_ID","COUNTY","TOWN","VILLAGE"),
                 measure.vars = str_subset(string = names(POP),pattern = "^[0-9]{0,3}歲"),
                 variable.name="AgeGroup",variable.factor=F,value.name="Population")
str(population)

Ages <- (copy(population)
         [ ,":="(性別=str_extract(string=AgeGroup, pattern = "[男女]$"),
                   年齡=as.numeric(str_extract(string = AgeGroup,pattern = "^[0-9]{1,3}")),
                   人口數=as.numeric(Population))]
         [ ,.(COUNTY_ID, TOWN_ID, V_ID, COUNTY, TOWN, VILLAGE, 性別, 年齡, 人口數) ]
         [])

Ages.W <- dcast(data=Ages, 
                formula=COUNTY_ID+TOWN_ID+V_ID+COUNTY+TOWN+VILLAGE+性別~年齡, 
                value.var="人口數")
head(Ages.W)
#鄉鎮市區平均年齡
Town.Age <- (copy(Ages)
             [ , .(MeanAge=sum((年齡+0.5)*人口數)/sum(人口數)),
               by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
             [])
Town.Age

#鄉鎮市區老年人口百分比
Town.Old <- (copy(Ages)
             [ , .(Old=sum((年齡>=65)*人口數)/sum(人口數)*100),
               by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
             [])
Town.Old
profile1 <- merge(x=總戶數, y=Town.Age, 
                  by=intersect(names(總戶數), names(Town.Age)))
profile1

#第三週3/10的data.table----------------------------------------------

#鄉鎮市區平均年齡&老化指數
Town.Age <- (copy(Ages)
             [ , .(MeanAge=sum((年齡+0.5)*人口數)/sum(人口數)),
               by=.(TOWN_ID)]
             [])
Town.Aging <- (copy(Ages)
               [ , .(老化指數=sum((年齡>=65)*人口數)/sum((年齡<=14)*人口數)*100),
                 by=.(TOWN_ID)]
               [])
Town.Age
Town.Aging
profile2 <- merge(x=Town.Age, y=Town.Aging, 
                  by=intersect(names(Town.Age), names(Town.Aging)))
profile2

#將地方創生地點與平均年齡、老化指數合起來
setwd("C:/Users/user/Downloads/Data-3-10-2021-20210310")
Type <- setDT(read_excel(path="地方創生優先推動地區.xlsx", sheet="地方創生優先推動地區"))
Type <- Type[ , 地方創生:=ifelse(is.na(類型), "一般", "優先")][]
profile3 <- merge(x=Type, y=profile2, by="TOWN_ID")
profile3

#以「地方創生」變項作區分(一般&優先)，進行兩母體之平均年齡的變異數分析和t檢定
psych::describeBy(x=profile3$MeanAge, group=profile3$地方創生)
var.test(formula=MeanAge~地方創生, data=profile3)
t.test(formula=MeanAge~地方創生, data=profile3, var.equal=TRUE)
t.test(formula=MeanAge~地方創生, data=profile3, var.equal=FALSE)

#不同類別地區的年齡之ANOVA分析
profile3 <- profile3[ , 地區:=ifelse(is.na(類型), "一般", 類型)][]
profile3[ ,.N, , by=(地區)]
psych::describeBy(x=profile3$MeanAge, group=profile3$地區)
results <- aov(formula=MeanAge~地區, data=profile3)
summary(results)
model.tables(x=results, "mean")

#比較組間差異
TukeyHSD(x=results)
DescTools::ScheffeTest(x=results)

#線性回歸分析
reg.results <- lm(data=profile3, formula=老化指數~地區)
anova(reg.results)
summary(reg.results)

#作業----------------------------------------------------------------

#前置作業，以及高等教育人數&比例
setwd("C:/Users/user/Downloads/Data-3-10-2021-20210310/2020年村里15歲以上現住人口按性別、年齡、婚姻狀況及教育程度分")
pop <- fread(input="2020年村里15歲以上現住人口按性別、年齡、婚姻狀況及教育程度分.csv", skip=1,
             encoding="UTF-8",colClasses="character")
pop
names(pop) <- enc2native(names(pop))
pop[ , 人口數:=as.numeric(人口數)]
pop[ , ":="(COUNTY=str_sub(string = 區域別,start=1,end=3),
            TOWN=str_sub(string = 區域別,start=4,end=-1),
            VILLAGE=村里)]
pop[ , ":="(COUNTY_ID=str_sub(string =區域別代碼,start=1 , end=5),
            TOWN_ID=str_sub(string =區域別代碼,start=1 , end=8),
            V_ID=paste0(str_sub(string =區域別代碼,start=1 , end=8),"-",
                        str_sub(string =區域別代碼,start=9 , end=11)))]
pop[ , Age_lwr:=as.numeric(str_extract(string = 年齡, pattern = "^[0-9]{2,3}"))]
names(pop)[8] <- "edu"
names(pop)[9] <- "population"
pop[Age_lwr>=30 & Age_lwr<65,
    .(高等教育比例=sum((edu %in% c("博畢","碩畢","大畢"))* population) / 
                        sum(population)), by=.(TOWN_ID)]
pop[ , .(高等教育人數=sum(edu %in% c("博畢","碩畢","大畢"))* population), 
                         by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]

