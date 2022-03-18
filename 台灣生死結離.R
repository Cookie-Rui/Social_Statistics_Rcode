setwd("C:/Users/user/Downloads/r folder (second semester)/Data-6-9-2021")
library(data.table)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
rawdata <- setDT(read_excel(path="臺灣地區歷月生死結離數.xlsx", sheet="生死結離"))
names(rawdata) <- enc2native(names(rawdata))
rawdata <- (copy(rawdata)
            [ , 日期:=as.Date(x=paste0(月,"-15-",年), format="%m-%d-%Y")]
            [])
p1 <- ggplot()+
  geom_point(data=rawdata, aes(x=日期,y=出生數), color="forestgreen", size=1)
p1

p2 <- ggplot()+
  geom_step(data=rawdata, aes(x=日期,y=出生數), color="forestgreen", 
            size=1, direction="mid")+
  scale_x_date(breaks=(seq.Date(from = as.Date("1910-1-1"),
                                to=as.Date("2020-1-1"), by="5 year")),
               date_label="%Y")+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour = "tan", size = 0.05))
p2

AnnualBirths <- (rawdata[年<2021,][ , .(出生數=sum(出生數)), by=.(年)])
AnnualDeath <- (rawdata[年<2021,][ , .(死亡數=sum(死亡數)), by=.(年)])
AnnualMarried <- (rawdata[年<2021,][ , .(結婚對數=sum(結婚對數)), by=.(年)])
AnnualDivorce <- (rawdata[年<2021,][ , .(離婚對數=sum(離婚對數)), by=.(年)])
AnnualPOP <- (rawdata[年<2021,][ , .(人口數=sum(人口數)), by=.(年)])
p3 <- ggplot(data=AnnualBirths,aes(x=年, y=出生數))+
  geom_vline(data=data.table(v=seq(from=1910, to=2020,by=5)),
             aes(xintercept=v), linetype=2, size=0.5, color="grey50")+
  geom_hline(data=data.table(h=seq(from=100000, to=420000,by=10000)),
             aes(yintercept=h), linetype=2, size=0.5, color="grey80")+
  geom_line(color="royalblue", size=1)+
  geom_point(shape=22, color="orange", fill="forestgreen", size=6)+
  scale_x_continuous(breaks = seq(from=1910,to=2020,by=5))+
  scale_y_continuous(breaks = seq(from=150000, to=400000, by=50000))+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour = "tan", size = 0.05),
        text = element_text(size=25))

p4 <- ggplot(data=AnnualDeath,aes(x=年, y=死亡數))+
  geom_vline(data=data.table(v=seq(from=1910, to=2020,by=5)),
             aes(xintercept=v), linetype=2, size=0.5, color="grey50")+
  geom_hline(data=data.table(h=seq(from=100000, to=180000,by=10000)),
             aes(yintercept=h), linetype=2, size=0.5, color="grey80")+
  geom_line(color="royalblue", size=1)+
  geom_point(shape=22, color="orange", fill="forestgreen", size=6)+
  scale_x_continuous(breaks = seq(from=1910,to=2020,by=5))+
  scale_y_continuous(breaks = seq(from=100000, to=180000, by=20000))+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour = "tan", size = 0.05),
        text = element_text(size=25))

p5 <- ggplot(data=AnnualMarried,aes(x=年, y=結婚對數))+
  geom_vline(data=data.table(v=seq(from=1910, to=2020,by=5)),
             aes(xintercept=v), linetype=2, size=0.5, color="grey50")+
  geom_hline(data=data.table(h=seq(from=100000, to=190000,by=10000)),
             aes(yintercept=h), linetype=2, size=0.5, color="grey80")+
  geom_line(color="royalblue", size=1)+
  geom_point(shape=22, color="orange", fill="forestgreen", size=6)+
  scale_x_continuous(breaks = seq(from=1910,to=2020,by=5))+
  scale_y_continuous(breaks = seq(from=100000, to=200000, by=20000))+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour = "tan", size = 0.05),
        text = element_text(size=25))

p6 <- ggplot(data=AnnualDivorce,aes(x=年, y=離婚對數))+
  geom_vline(data=data.table(v=seq(from=1910, to=2020,by=5)),
             aes(xintercept=v), linetype=2, size=0.5, color="grey50")+
  geom_hline(data=data.table(h=seq(from=10000, to=70000,by=10000)),
             aes(yintercept=h), linetype=2, size=0.5, color="grey80")+
  geom_line(color="royalblue", size=1)+
  geom_point(shape=22, color="orange", fill="forestgreen", size=6)+
  scale_x_continuous(breaks = seq(from=1910,to=2020,by=5))+
  scale_y_continuous(breaks = seq(from=10000, to=70000, by=10000))+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour = "tan", size = 0.05),
        text = element_text(size=25))

p7 <- ggplot(data=AnnualPOP,aes(x=年, y=人口數))+
  geom_vline(data=data.table(v=seq(from=1910, to=2020,by=5)),
             aes(xintercept=v), linetype=2, size=0.5, color="grey50")+
  geom_hline(data=data.table(h=seq(from=150000000, to=300000000,by=20000000)),
             aes(yintercept=h), linetype=2, size=0.5, color="grey80")+
  geom_line(color="royalblue", size=1)+
  geom_point(shape=22, color="orange", fill="forestgreen", size=4)+
  scale_x_continuous(breaks = seq(from=1910,to=2020,by=5))+
  scale_y_continuous(breaks = seq(from=150000000, to=300000000, by=20000000))+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour = "tan", size = 0.05))

p7

p1
p2
p3
p4
p5
p6
p7


birth_death <- merge(AnnualBirths,AnnualDeath)
BIRTH_DEATH <- melt(data=birth_death, id.vars = "年",measure.var=c("出生數","死亡數"),
     variable.name="生or死",
     value.name="人數")

p8 <- ggplot(data=BIRTH_DEATH,aes(x=年, y=人數, group=生or死))+
  geom_vline(data=data.table(v=seq(from=1910, to=2020,by=5)),
             aes(xintercept=v), linetype=2, size=0.5, color="grey50")+
  geom_hline(data=data.table(h=seq(from=100000, to=420000,by=10000)),
             aes(yintercept=h), linetype=2, size=0.5, color="grey80")+
  geom_line(color="royalblue", size=1)+
  geom_point(shape=22, color="orange", fill="forestgreen", size=2)+
  scale_x_continuous(breaks = seq(from=1910,to=2020,by=5))+
  scale_y_continuous(breaks = seq(from=150000, to=400000, by=50000))+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour = "tan", size = 0.05),
        text = element_text(size=25))
p8
