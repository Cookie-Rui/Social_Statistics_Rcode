#Duncan--------------------
setwd("C:/Users/user/Downloads")
library(readxl)
Duncan <- read_excel(path="DuncanSEI.xlsx")
Duncan
plot(x=Duncan$Income, y=Duncan$Prestige)
plot(x=Duncan$Income, y=Duncan$Prestige, pch=21,col="brown", bg="green")
text(x=Duncan$Income, y=Duncan$Prestige, labels = Duncan$CaseID,
     pos=1, col="RoyalBlue", cex=1)
library(car)
scatterplot(Prestige~Income,data=Duncan, smooth=FALSE, regLine=FALSE)
scatterplot(Prestige~EDU,data=Duncan, smooth=FALSE, regLine=FALSE)
scatterplot(Income~EDU,data=Duncan, smooth=FALSE, regLine=FALSE)
# 兩個變項可以形成一個一個散佈圖，四個變項兩兩一對可以形成16個散佈圖，合起來形成「散佈圖矩陣」
pairs(formula=~Prestige+Rating+Income+EDU, data=Duncan, col="brown")
pairs(formula=~Prestige+Rating+Income+EDU, data=Duncan, col="brown", upper.panel=panel.smooth)
#1. 左上到右下的對角線是自己跟自己做交叉，是對角線直線，因此不畫出來
#2. 對角線以下的圖跟對角線以上的圖是互為x軸/y軸互調
scatterplotMatrix(formula=~Prestige+Rating+Income+EDU, data=Duncan,
                  smooth=FALSE, regLine=FALSE, diagonal=list(method="histogram", breaks=20))
library(ggplot2)
library(GGally)
library(data.table)
setDT(Duncan)
p0 <- ggplot(data=Duncan, aes(x=Income, y=Prestige))+
  geom_point(shape=11, col="brown", fill="forestgreen", size=2)+
  geom_smooth(method="lm")+theme_classic()
p0
p2 <- ggpairs(data=Duncan[ , .(Prestige,Rating, Income, EDU)],
              diag = list(continuous=wrap("barDiag", binwidth=1, fill="tomato")))
p2

#旅館------------------------
library(sf)
library(dplyr)
library(ggplot2)
library(data.table)
library(readxl)
library(car)
setwd("C:/Users/user/Downloads")
RawData <- setDT(read_excel(path="旅館民宿 - 觀光資訊資料庫.xlsx"))
DATA <- read_excel(path="2020鄉鎮市區各項資料彙整.xlsx")
MONEY <- read_excel(path="107年綜合所得稅所得總額申報統計.xlsx")
RawData <- RawData[ , ":="(COUNTY=Region, TOWN=Town)][]
ID <- setDT(read_excel(path="TOWN_ID.xlsx", sheet="ID"))
Hotels <- merge(x=RawData, y=ID, by=c("COUNTY", "TOWN"), all.x = TRUE)
Hotels <- Hotels[ , Class:=factor(x=Class, levels=1:4, 
                                  labels=c("國際觀光旅館","一般觀光旅館","一般旅館","民宿"))]
hotel <- (copy(Hotels)
          [ , .N, keyby=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN, Class)]
          [])
hotel <- dcast(data=hotel, formula=COUNTY_ID+TOWN_ID+COUNTY+TOWN~Class, value.var = "N")
hotel

hotel1 <- merge(x=hotel, y=DATA, keyby=c("COUNTY", "TOWN"))
hotel1 <- hotel1[ , 民宿密度:=(民宿/土地面積)]
hotel2 <- merge(x=hotel1, y=MONEY, keyby=c("COUNTY", "TOWN"))
scatterplotMatrix(formula=~民宿密度+人口密度+中位數, data=hotel2,
                  smooth=FALSE, regLine=FALSE, diagonal=list(method="histogram", breaks=20))



#畫地圖---------------------
load(file="Town.RData")
load(file="County.RData")
Town <- left_join(x=Town, y=hotel, by=intersect(names(Town), names(hotel)))
p1 <- ggplot()+
  geom_sf(data=Town, aes(fill=民宿),color="grey",size=0.02)+
  geom_sf(data=County, fill=NA, color="forestgreen", size=0.05)+
  scale_fill_viridis_c(option="A", na.value=NA, direction=-1)+
  theme_void()+theme(legend.position = c(0.2,0.2))

p1
