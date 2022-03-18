setwd("C:/Users/user/Downloads/r folder (second semester)/Data-5-26-2021")
load(file="factory.RData")
load(file="TaipeiCounty.RData")
load(file="TaipeiTown.RData")
library(sf)
library(dplyr)
library(ggplot2)
library(data.table)
plot(TaipeiCounty$geometry)
plot(TaipeiTown$geometry)
plot(factory$geometry)

p0 <- ggplot()+
  geom_sf(data=TaipeiTown)
p0 <- ggplot()+
  geom_sf(data=TaipeiTown, fill=NA, color="blue")
p0 <- ggplot()+
  geom_sf(data=TaipeiTown, fill=NA, color="grey")+
  geom_sf(data=TaipeiCounty, fill=NA, color="sandybrown")+
  geom_sf(data=factory, size=1, color="forestgreen")
p0 <- ggplot()+
  geom_sf(data=TaipeiTown, fill=NA, color="grey")+
  geom_sf(data=TaipeiCounty, fill=NA, color="sandybrown")+
  geom_sf(data=factory, size=1, color="forestgreen")+
  geom_sf_text(data=TaipeiTown, aes(label=TOWN), size=3, color="blue")
p0 <- ggplot()+
  geom_sf(data=TaipeiTown, fill=NA, color="grey")+
  geom_sf(data=TaipeiCounty, fill=NA, color="sandybrown")+
  geom_sf(data=factory, size=1, color="forestgreen")+
  geom_sf_text(data=TaipeiTown, aes(label=TOWN), size=3, color="blue")+
  coord_sf(ndiscr = 0) + theme_void()
#因為台灣位在北半球，在地圖投影上會呈現南大北小(經線之間的距離愈往北邊愈窄)
#coord_sf(ndiscr = 0)就是改變投影方式，取消上述的影響

Taipei <- st_cast(st_union(x=summarise(group_by(.data=TaipeiCounty))))
#把雙北的區域全部合併在一塊
plot(Taipei)

grids <- st_make_grid(x=Taipei, cellsize=1000, what="polygons")
#以Taipei資料為準，畫出每方格邊長為1000公尺(即一方格= 1平方公里)的方格圖
plot(grids)

grids <- st_as_sf(data.table(ID=1:length(grids), geometry=grids))
#將每個方格賦予編號
grids

Taipei.grids <- st_intersection(x=grids, y=Taipei)
#將雙北地圖 & 方格圖合併在一起
plot(Taipei.grids$geometry)

units <- aggregate(x=factory, by=Taipei.grids, FUN ="length")
#將工廠資料跟方格地圖合併，計算每個方格內的工廠數量(使用"length")
units

Taipei.grids$units <- aggregate(x=factory, by=Taipei.grids, FUN ="length")$REGI_ID
#把REGI_id(即工廠數量)單獨提取出來
Taipei.grids

Taipei.grids <- filter(.data=Taipei.grids, !is.na(units))
#將不是NA的地區(即沒有工廠的地區)提取出來
Taipei.grids
plot(Taipei.grids$geometry)
plot(Taipei.grids)

p1 <- ggplot()+
  geom_sf(data=Taipei.grids, aes(fill=units), color=NA)+
  geom_sf(data=TaipeiTown, fill=NA, color="grey90", size=0.01)+
  geom_sf(data=TaipeiCounty, fill=NA, color="orchid", size=0.5)+
  coord_sf(ndiscr = 0) + theme_void()

hist(Taipei.grids$units, breaks=50)
#透過直方圖我們可以看到次數分布是非常傾斜的，故進行對數轉換

p2 <- ggplot()+
  geom_sf(data=Taipei.grids, aes(fill=log(units)), color=NA)+
  geom_sf(data=TaipeiTown, fill=NA, color="grey90", size=0.01)+
  geom_sf(data=TaipeiCounty, fill=NA, color="orchid", size=0.5)+
  geom_sf_text(data=TaipeiTown, aes(label=TOWN), size=2.5, color="forestgreen")+
  scale_fill_viridis_c(option = "B", direction = -1, name="工廠家數",breaks=0:6,
                       labels=floor(exp(0:6)))+
  coord_sf(ndiscr = 0) + theme_void()+
  theme(legend.position = c(0.8,0.2))

#作業1：運用上學期9/23的「全國全聯分布」，模仿今天的流程，呈現雙北地區的全聯熱區
#作業2：運用本次的「臺北捷運每日分時各站OD流量統計資料_202104」(等同於上學期的資料)
#分析4月份台北捷運的使用有什麼趨勢或現象

#作業一
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(sf)
setwd("C:/Users/user/Downloads/r folder (second semester)/Data-5-26-2021")
load(file="PxMart.RData")
load(file="TaipeiCounty.RData")
load(file="TaipeiTown.RData")
TaipeiMart <- filter(PxMart,COUNTY %in% c("臺北市","新北市"))

p0 <- ggplot()+
  geom_sf(data=TaipeiTown, fill=NA, color="grey")+
  geom_sf(data=TaipeiCounty, fill=NA, color="sandybrown")+
  geom_sf(data=TaipeiMart, size=1, color="forestgreen")+
  geom_sf_text(data=TaipeiTown, aes(label=TOWN), size=3, color="blue")+
  coord_sf(ndiscr = 0) + theme_void()

Taipei <- st_cast(st_union(x=summarise(group_by(.data=TaipeiCounty))))
grids <- st_make_grid(x=Taipei, cellsize=1000, what="polygons")
grids <- st_as_sf(data.table(ID=1:length(grids), geometry=grids))
Taipei.grids <- st_intersection(x=grids, y=Taipei)
plot(Taipei.grids$geometry)
units <- aggregate(x=TaipeiMart, by=Taipei.grids, FUN ="length")
units
Taipei.grids$units <- aggregate(x=TaipeiMart, by=Taipei.grids, FUN ="length")$TOWN
Taipei.grids
Taipei.grids <- filter(.data=Taipei.grids, !is.na(units))
Taipei.grids
plot(Taipei.grids$geometry)
plot(Taipei.grids)

p1 <- ggplot()+
  geom_sf(data=Taipei.grids, aes(fill=units), color=NA)+
  geom_sf(data=TaipeiTown, fill=NA, color="grey90", size=0.01)+
  geom_sf(data=TaipeiCounty, fill=NA, color="orchid", size=0.5)+
  geom_sf_text(data=TaipeiTown, aes(label=TOWN), size=2.5, color="forestgreen")+
  scale_fill_viridis_c(option = "B", direction = -1, name="全聯家數",breaks=0:6,
                       labels=0:6)+
  coord_sf(ndiscr = 0) + theme_void()+
  theme(legend.position = c(0.8,0.2))

#作業二
OD <- fread("臺北捷運每日分時各站OD流量統計資料_202104.csv")

OD[時段>=7 & 時段<=9, 尖峰離峰時段:="上班"]
OD[時段>=16 & 時段<=18,尖峰離峰時段:="下班"]
OD[is.na(尖峰離峰時段),尖峰離峰時段:="離峰"]
DATA <- OD[ , .(人次=sum(人次)), by=.(尖峰離峰時段)]
DATA[尖峰離峰時段 %in% "離峰", 平均人次:=人次/18]
DATA[尖峰離峰時段 %in% c("上班","下班"), 平均人次:=人次/3]
DATA

weekend <- str_sub(string = unique(OD$日期), start=-2, end=-1)
weekend <- weekend[c(2,3,4,5,10,11,17,18,24,25)]
weekend
OD[ ,日:=str_sub(string = 日期 ,start=-2 , end=-1)]
OD[ , 平日假日:=ifelse(日 %in% weekend, "假日", "平日")]
DATA1 <- OD[ , .(人次=sum(人次)), by=.(平日假日)]
DATA1[平日假日 %in% "平日", 平均人次:=人次/20]
DATA1[平日假日 %in% "假日", 平均人次:=人次/10]
DATA1

A <- head(OD[ , .(總進站人次=sum(人次)),by=.(進站)][order(總進站人次, decreasing=TRUE)],10)
names(A)[1] <- "站名"
B <- head(OD[ , .(總出站人次=sum(人次)),by=.(出站)][order(總出站人次, decreasing=TRUE)],10)
names(B)[1] <- "站名"
C <- merge(x=A, y=B, by="站名")
C[ , 進站減出站:=總進站人次-總出站人次]
C[ , 進站減出站_每日:=round(進站減出站/30)]
