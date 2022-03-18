setwd("C:/Users/user/Downloads/r folder (second semester)/Data-6-2-2021")
load(file="MRT.Routes.RData")
load(file="MRT.Stations.RData")
library(sf)
library(ggplot2)
MRT.Routes
MRT.Stations
plot(st_geometry(MRT.Routes), col="blue", lwd=1.5)
table(MRT.Routes$MRTCODE)
#左下角的三鶯線尚未通車，我們把它的線路移除
MRT.Routes <- dplyr::filter(.data=MRT.Routes, MRTCODE!="三鶯線")
plot(st_geometry(MRT.Routes), col="blue", lwd=1.5)
plot(st_geometry(MRT.Stations),pch=22, col="tomato", bg="forestgreen", add=TRUE)
#add參數：設定是否要「清除」前一張圖再放上現在的圖，設定為TRUE代表保留前一張，將現在這張蓋在上面

#作業1
setwd("C:/Users/user/Downloads/r folder (second semester)/Data-6-2-2021")
load(file="MRT.Routes.RData")
load(file="MRT.Stations.RData")
library(sf)
library(ggplot2)
MRT.Routes <- dplyr::filter(.data=MRT.Routes, MRTCODE!="三鶯線")
p3 <- ggplot()+
  geom_sf(data=MRT.Routes, col="SlateBlue", size=0.6)+
  geom_sf(data=MRT.Stations, shape=22, size=3,
          color="olivedrab", fill="goldenrod")+
  theme_void()+coord_sf(ndiscr = 0)
p3
#加上站名
p4 <- ggplot()+
  geom_sf(data=MRT.Routes, col="SlateBlue", size=0.6)+
  geom_sf(data=MRT.Stations, shape=22, size=3,
          color="olivedrab", fill="goldenrod")+
  geom_sf_text(data=MRT.Stations, aes(label=站名), size=3, color="forestgreen")+
  theme_void()+coord_sf(ndiscr = 0)
p4


#作業2
library(data.table)
library(stringr)
library(lubridate)
MRT <- fread(input="臺北捷運每日分時各站OD流量統計資料_202104.csv", header=TRUE, 
             colClasses=list(character=1:4))

MRT <- (MRT[ , 日期:=as.Date(fast_strptime(日期, "%Y-%m-%d"))]
        [ , ":="(進站=str_replace_all(string = 進站,
                                    pattern=c("大橋頭站"="大橋頭","台"="臺")),
                   出站=str_replace_all(string = 出站,
                                      pattern=c("大橋頭站"="大橋頭","台"="臺")))]
        [])

NB <- setDT(readxl::read_excel(path="站間起迄站數.xlsx", sheet="起迄站數"))
NB
setnames(x=NB, old="起迄", new="進站")
NB <- melt(data=NB, id.vars = "進站",measure.var=(names(NB)[-1]),
           variable.name="出站",
           value.name="站數")
Stops <- merge(x=MRT, y=NB[ , 出站:=as.character(出站)],
               by=intersect(names(MRT), names(NB)))
names(Stops) <- enc2native(names(Stops))
Stops[,":="(時段=as.numeric(時段),人次=as.numeric(人次),日期=as.Date(日期))] 

IN <- (Stops[時段 %in% c(05,06,07)]
       [ , .(IN.平均站數=sum(人次*站數)/sum(人次)), by=.(進站)]
       [ , .(站名=進站, IN.平均站數)])

OUT <- (Stops[時段 %in% c(23,00,01)]
        [ , .(OUT.平均站數=sum(人次*站數)/sum(人次)), by=.(出站)]
        [ , .(站名=出站, OUT.平均站數)])

OTHER <- (Stops[!時段 %in% c(23,00,01,05,06,07)]
          [ , .(OTHER.平均站數=sum(人次*站數)/sum(人次)), by=.(進站)]
          [ , .(站名=進站, OTHER.平均站數)])
early_late <- merge(x=merge(x=IN, y=OUT),y=OTHER)
early_late[ , 早出晚歸平均減其他時段:=(IN.平均站數+OUT.平均站數)/2 - OTHER.平均站數]
early_late[ , z分數:=
              (早出晚歸平均減其他時段-mean(早出晚歸平均減其他時段))/sd(早出晚歸平均減其他時段)]
head(early_late[order(z分數, decreasing=TRUE)],10)
head(early_late[order(z分數)],10)


#作業3
郊區 <- c("淡水","紅樹林","象山","台北101/世貿","松山","南京三民","新店區公所","新店","頂埔","永寧","南港","南港展覽館","動物園","木柵","南港展覽館","南港軟體園區","景安","南勢角","蘆洲","三民高中","迴龍","丹鳳","新北產業園區","幸福","大坪林","十四張")
Stops[進站 %in% 郊區, .(平均站數=sum(人次*站數)/sum(人次))]
Stops[!進站 %in% 郊區, .(平均站數=sum(人次*站數)/sum(人次))]