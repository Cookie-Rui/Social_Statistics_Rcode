library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
setwd("C:/Users/user/Downloads/r folder (second semester)/Data-5-26-2021")
MRT <- fread(input="臺北捷運每日分時各站OD流量統計資料_202104.csv",
             header=TRUE,
             colClasses=list(character=1:4))
MRT[ , 時段:=as.numeric(時段)]
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
Stops <- merge(x=MRT, y=NB[ , 出站 :=as.character(出站 )],
               by=intersect(names(MRT), names(NB)))
names(Stops) <- enc2native(names(Stops))
Stops[,":="(時段 =as.numeric(時段 ),人次 =as.numeric(人次 ),日期 =as.Date(日期 ))]
IN <- (Stops[時段 %in% c(05,06,07)]
       [ , .(IN.平均站數 =sum(人次 *站數 )/sum(人次 )), by=.(進站 )]
       [ , .(站名 =進站 , IN.平均站數 )])
OUT <- (Stops[時段 %in% c(23,00,01)]
        [ , .(OUT.平均站數 =sum(人次 *站數 )/sum(人次 )), by=.(出站 )]
        [ , .(站名 =出站 , OUT.平均站數 )])
OTHER <- (Stops[!時段 %in% c(23,00,01,05,06,07)]
          [ , .(OTHER.平均站數 =sum(人次 *站數 )/sum(人次 )), by=.(進站 )]
          [ , .(站名 =進站 , OTHER.平均站數 )])
merge(x=merge(x=IN, y=OUT),y=OTHER)

early_late <- merge(x=merge(x=IN, y=OUT),y=OTHER)
early_late[ , 早出晚歸平均減其他時段 :=(IN.平均站數 +OUT.平均站數 )/2 - OTHER.平均站數 ]
early_late[ , z分數 :=
              (早出晚歸平均減其他時段-mean(早出晚歸平均減其他時段))/sd(早出晚歸平均減其他時段)]
head(early_late[order(z分數, decreasing=TRUE)],10)
head(early_late[order(z分數)],10)

郊區<- c("淡水","紅樹林","象山","臺北101/世貿","松山","南京三民","新店區公所","新店","頂埔","永寧","南港","南港展覽館","動物園","木柵","南港展覽館","南港軟體園區","景安","南勢角","蘆洲","三民高中","迴龍","丹鳳","新北產業園區","幸福","大坪林","十四張")
Stops[進站 %in% 郊區 , .(平均站數 =sum(人次 *站數 )/sum(人次 ))]
Stops[!進站 %in% 郊區 , .(平均站數 =sum(人次 *站數 )/sum(人次 ))]


MRT[時段 >=7 & 時段 <=9, 尖峰離峰時段:="上班"]
MRT[時段 >=16 & 時段 <=18, 尖峰離峰時段:="下班"]
MRT[is.na(尖峰離峰時段),尖峰離峰時段:="離峰"]
DATA1_1 <- MRT[ , .(人次 =sum(人次)), by=.(尖峰離峰時段, 進站)]
DATA1_1[尖峰離峰時段 %in% "離峰", 平均人次 :=人次 /15]
DATA1_1[尖峰離峰時段 %in% c("上班","下班"), 平均人次 :=人次 /3]
DATA1_1 <- dcast(DATA1_1[ , 人次:=NULL], 進站~尖峰離峰時段)
DATA1_1[ , 上班減離峰:=上班-離峰]
DATA1_1[ , 下班減離峰:=下班-離峰]
DATA1_1[ , 上班減下班:=上班-下班]
尖峰離峰平均進站 <- (DATA1_1[ , 上vs離z分數:=(上班減離峰-mean(上班減離峰))/sd(上班減離峰)]
  [ , 下vs離z分數:=(下班減離峰-mean(下班減離峰))/sd(下班減離峰)]
  [ , 上vs下z分數:=(上班減下班-mean(上班減下班))/sd(上班減下班)])


DATA1_2 <- MRT[ , .(人次 =sum(人次)), by=.(尖峰離峰時段, 出站)]
DATA1_2[尖峰離峰時段 %in% "離峰", 平均人次 :=人次 /15]
DATA1_2[尖峰離峰時段 %in% c("上班","下班"), 平均人次 :=人次 /3]
DATA1_2 <- dcast(DATA1_2[ , 人次:=NULL], 出站~尖峰離峰時段)
DATA1_2[ , 上班減離峰:=上班-離峰]
DATA1_2[ , 下班減離峰:=下班-離峰]
DATA1_2[ , 上班減下班:=上班-下班]
尖峰離峰平均出站 <- (DATA1_2[ , 上vs離z分數:=(上班減離峰-mean(上班減離峰))/sd(上班減離峰)]
             [ , 下vs離z分數:=(下班減離峰-mean(下班減離峰))/sd(下班減離峰)]
             [ , 上vs下z分數:=(上班減下班-mean(上班減下班))/sd(上班減下班)])



weekend <- str_sub(string = unique(MRT$日期 ), start=-2, end=-1)
weekend <- weekend[c(2,3,4,5,10,11,17,18,24,25)]
weekend

MRT[ ,日 :=str_sub(string = 日期 ,start=-2 , end=-1)]
MRT[ , 平日假日 :=ifelse(日 %in% weekend, "假日", "平日")]
DATA1 <- MRT[ , .(進站人次 =sum(人次 )), by=.(平日假日,進站)]
DATA2 <- MRT[ , .(出站人次 =sum(人次 )), by=.(平日假日,出站)]
DATA1[平日假日 %in% "平日", 進站平均人次 :=進站人次 /20]
DATA1[平日假日 %in% "假日", 進站平均人次 :=進站人次 /10]
DATA1[ , 進站人次:=NULL]
DATA2[平日假日 %in% "平日", 出站平均人次 :=出站人次 /20]
DATA2[平日假日 %in% "假日", 出站平均人次 :=出站人次 /10]
DATA2[ , 出站人次:=NULL]
DATA1 <- dcast(DATA1, 進站~平日假日)
DATA2 <- dcast(DATA2, 出站~平日假日)
DATA1[ , 平日減假日:=平日-假日 ]
平日假日平均進站 <- (DATA1[ , z分數 :=(平日減假日-mean(平日減假日))/sd(平日減假日)]
             [order(z分數,decreasing=TRUE)])
DATA2[ , 平日減假日:=平日-假日 ]
平日假日平均出站 <- (DATA2[ , z分數 :=(平日減假日-mean(平日減假日))/sd(平日減假日)]
             [order(z分數,decreasing=TRUE)])

進站人次 <- (MRT[ , .(總人次 =sum(人次 )),by=.(進站 )][,平均每日人次:=總人次/30])
出站人次 <- (MRT[ , .(總人次 =sum(人次 )),by=.(出站 )][,平均每日人次:=總人次/30])
進站人次[order(平均每日人次)]
names(進站人次)[1] <- "站名"
names(出站人次)[1] <- "站名"
names(平日假日平均進站)[1] <- "站名"
names(平日假日平均出站)[1] <- "站名"
names(尖峰離峰平均進站)[1] <- "站名"
names(尖峰離峰平均出站)[1] <- "站名"
merge(x=merge(x=進站人次,y=出站人次), y=merge(x=平日假日平均進站,y=平日假日平均出站))
A <- left_join(x=進站人次,y=出站人次, by="站名")
B <- left_join(x=平日假日平均進站,y=平日假日平均出站,by="站名")
C <- left_join(x=尖峰離峰平均進站,y=尖峰離峰平均出站,by="站名")
大資料 <- merge(merge(A,B),C)

library(rio)
export(大資料,"大資料.xlsx")

bigdata <- setDT(readxl::read_excel(path="大資料.xlsx"))
ncol(bigdata)
rank <- bigdata[ , c(1,32:37,39:40)]
rank <- rank[order(A)]
names(rank)[8:9] <- c("G","H")
export(rank,"rank.xlsx")

export(rank[A<30 & C<30][order(A)], "aaa.xlsx")
export(rank[A<30 & C>90][order(A)], "bbb.xlsx")
export(rank[A>80 & C<40][order(A)], "ccc.xlsx")
export(rank[A>90 & C>90][order(A)], "ddd.xlsx")
rank[E<30][order(E)]
rank[F<30][order(F)]
rank[G<30][order(G)]

rank[A<30 & E<30][order(A)]
rank[A<30 & G<30][order(A)]
rank[A<30 & F<30][order(A)]
rank[H<30][order(A)]
summary(lm(rank,formula=A~B))
plot(x=rank$A, y=rank$B, xlab="變項A", ylab="變項B", pch=16, cex=1, cex.lab=2)
p <- ggplot()+
  geom_point(data=rank, aes(x=A, y=B))+
  theme_classic()+
  scale_x_continuous(name="變項A")+
  scale_y_continuous(name="變項B")+
  theme(text = element_text(size=35))

p1 <- ggplot()+
  geom_point(data=rank, aes(x=C, y=D))+
  theme_classic()+
  scale_x_continuous(name="變項C")+
  scale_y_continuous(name="變項D")+
  theme(text = element_text(size=35))
