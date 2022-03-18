setwd("C:/Users/user/Downloads/Data 9-23-2020-20200923")
load(file="County.RData")
load(file="PxMart.RData")
load(file="SimpleMart.RData")
load(file="Town.RData")
library(ggplot2)
library(sf)
Town #數值地圖
County #數值地圖
PxMart #1009家全聯位置
ggplot() + geom_sf(data = Town) #ggplot：建立空白圖紙 / geom_sf：建立Town的數值地圖
ggplot() + geom_sf(data = County)
ggplot() + geom_sf(data = Town) + geom_sf(data = County) #縣市的圖蓋過了鄉鎮的圖

p1 <- ggplot() + geom_sf(data=Town, fill=NA, color="tan") + 
      geom_sf(data=County, fill=NA, color="forestgreen") #fill=NA：變成透明
p1

ggplot() + geom_sf(data=PxMart)

p2 <- ggplot() + geom_sf(data=Town, fill=NA, color="tan") + 
      geom_sf(data=County, fill=NA, color="forestgreen") + 
      geom_sf(data=PxMart, color="navyblue") + 
      theme_void() # theme_void：把背景的座標格線刪除
p2

setwd("C:/Users/user/Downloads/Data 12-16-2020-20201216")
library(data.table)
library(dplyr)
IN <- fread(input="107年綜合所得稅所得總額申報統計_鄉鎮市區.csv",skip=1,
            colClasses=list(character=1:2))


