setwd("C:/Users/user/Downloads")
load(file="WorkStatus_1978_2019.RData")
library(ggplot2)
library(data.table)
Labor <- WorkStatus_1978_2019
str(Labor)
Sex.code <- attr(Labor$Sex, which="labels")
EDU.code <- attr(Labor$EDU, which="labels")
Marital.code <- attr(Labor$MaritalStatus, which="labels")
Work.code <- attr(Labor$WorkStatus, which="labels")
head(Labor)
unique(Labor$EDU)

'(Labor[ , Sex:=factor(x=Sex, levels = Sex.code,labels = names(Sex.code))]
  [ , EDU:=factor(x=EDU, levels = EDU.code,labels = names(EDU.code))]
  [ , MaritalStatus:=factor(x=MaritalStatus, levels = Marital.code,
                            labels = names(Marital.code))]
  [ , WorkStatus:=factor(x=WorkStatus, levels = Work.code,labels = names(Work.code))])
'
Age.25 <- (copy(Labor)
           [Age>=25,]
           [ , Weight:=ifelse((Year==2020), COUNT/11, COUNT/12)]
           [ , Sex:=factor(x=Sex, levels = Sex.code,labels = names(Sex.code))]
           [ , 高教:=ifelse((EDU %in% c(7,8,9,10)),1,0)]
           [ , .(高教百分比=sum((高教==1)*Weight)/sum(Weight)*100), by=.(Year,Sex)]
           [])
Age.25

p0 <- ggplot()+
  geom_line(data=Age.25, aes(x=Year,y=高教百分比, group=Sex, color=Sex))+
  geom_point(data=Age.25, aes(x=Year,y=高教百分比, group=Sex, 
                              color=Sex, shape=Sex, fill=Sex), size=5)+
  scale_shape_manual(values=21:22)+
  scale_color_brewer(palette = "Paired")+
  scale_fill_manual(values = c("forestgreen", "sandybrown"))+
  scale_x_continuous(name="年份", breaks=seq(from=1980,to=2020, by=5))+
  scale_y_continuous(breaks = seq(from=0, to=50, by=5))+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour="tan", size=0.05), 
        legend.position = c(0.8,0.2),
        text = element_text(size=40))
p0

Age5 <- (copy(Labor)
         [ , Weight:=ifelse((Year==2020), COUNT/11, COUNT/12)]
         [ , Age:=ifelse((Age>=75), 75, Age)]
         [ , Age:=Age-(Age %% 5)]
         [ , Sex:=factor(x=Sex, levels = Sex.code,labels = names(Sex.code))]
         [ , 高教:=ifelse((EDU %in% c(7,8,9,10)),1,0)]
         [ , .(高教百分比=sum((高教==1)*Weight)/sum(Weight)*100), by=.(Year,Sex, Age)]
         [ , 年齡:=ifelse((Age<75), paste0(Age,"-",Age+4), "75+")]
         [])
ages <- (copy(Age5)[Age %in% c(25,30,35,40,45),][])
str(ages)

p1 <- ggplot()+
  geom_line(data=ages, aes(x=Year,y=高教百分比, group=年齡, color=年齡))+
  geom_point(data=ages, aes(x=Year,y=高教百分比, group=年齡, 
                            color=年齡, shape=年齡, fill=年齡), size=5)+
  scale_shape_manual(values=21:25)+
  scale_color_brewer(palette = "Paired")+
  scale_fill_viridis_d(option="B")+
  scale_x_continuous(name="年份", breaks=seq(from=1980,to=2020, by=5))+
  scale_y_continuous(breaks = seq(from=0, to=85, by=5))+
  facet_wrap(~Sex, ncol=2)+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour="tan", size=0.05), 
        legend.position = c(0.1,0.8), 
        legend.background = element_blank(), legend.key = element_blank(),
        strip.background = element_blank(), strip.text = element_text(size=30),
        legend.text = element_text(size=30), legend.title = element_text(size=25),
        text = element_text(size=25))
p1

cohorts <- (copy(Labor)
            [ , Weight:=ifelse((Year==2020), COUNT/11, COUNT/12)]
            [ , BirthCohort:=Year-Age]
            [ , Age:=ifelse((Age>=65), 65, Age)]
            [ , BirthCohort:=BirthCohort - (BirthCohort %% 5)]
            [ , Sex:=factor(x=Sex, levels = Sex.code,labels = names(Sex.code))]
            [ , 高教:=ifelse((EDU %in% c(7,8,9,10)),1,0)]
            [ , .(高教百分比=sum((高教==1)*Weight)/sum(Weight)*100), 
              by=.(BirthCohort,Sex, Age)]
            [1950<=BirthCohort & BirthCohort<=1990, ]
            [ , 出生年次:=paste0(BirthCohort,"-",BirthCohort+4)]
            [])
cohorts

p2 <- ggplot()+
  geom_line(data=cohorts, aes(x=Age,y=高教百分比, group=出生年次, color=出生年次),size=3)+
  scale_color_brewer(palette = "Set1", direction = -1)+
  scale_x_continuous(name="年齡", breaks=seq(from=25,to=65, by=5),
                     limits = c(25,65))+
  scale_y_continuous(breaks = seq(from=0, to=85, by=5))+
  facet_wrap(~Sex, ncol=2)+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour="tan", size=0.05), 
        legend.position = c(0.9,0.75), 
        legend.background = element_blank(), legend.key = element_blank(),
        legend.title = element_text(size=35, color="forestgreen"),
        legend.text = element_text(size=30, color="sandybrown"),
        strip.background = element_blank(), strip.text = element_text(size=35),
        text = element_text(size=35))
p2

library(patchwork)
p1+p2



Labor

Age.25_不分性別 <- (copy(Labor)
           [Age>=25,]
           [ , Weight:=ifelse((Year==2020), COUNT/11, COUNT/12)]
           [ , Sex:=factor(x=Sex, levels = Sex.code,labels = names(Sex.code))]
           [ , 高教:=ifelse((EDU %in% c(7,8,9,10)),1,0)]
           [ , .(高教百分比=sum((高教==1)*Weight)/sum(Weight)*100), by=.(Year)]
           [])

Age.25_不分性別

p0_1 <- ggplot()+
  geom_line(data=Age.25_不分性別, aes(x=Year,y=高教百分比))+
  geom_point(data=Age.25_不分性別, aes(x=Year,y=高教百分比), size=5)+
  scale_shape_manual(values=21:22)+
  scale_color_brewer(palette = "Paired")+
  scale_fill_manual(values = c("forestgreen", "sandybrown"))+
  scale_x_continuous(name="年份", breaks=seq(from=1980,to=2020, by=5))+
  scale_y_continuous(breaks = seq(from=0, to=50, by=5))+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour="tan", size=0.05), 
        legend.position = c(0.8,0.2),
        text = element_text(size=40))
p0_1

p0

p1

p2

Marital_EDU <- (copy(Labor)
                [Age>=25,]
                [ , Weight:=ifelse((Year==2020), COUNT/11, COUNT/12)]
                [ , Sex:=factor(x=Sex, levels = Sex.code,labels = names(Sex.code))]
                [ , 高教:=ifelse((EDU %in% c(7,8,9,10)),1,0)]
                [ , .(高教百分比=sum((高教==1)*Weight)/sum(Weight)*100), 
                  by=.(Age,MaritalStatus,Sex)]
                [])

Marital_EDU[ ,MaritalStatus:=as.character(MaritalStatus)]
p3 <- ggplot()+
  geom_line(data=Marital_EDU, aes(x=Year,y=高教百分比, group=MaritalStatus, 
                           color=MaritalStatus))+
  geom_point(data=Marital_EDU, aes(x=Year,y=高教百分比, group=MaritalStatus, 
                            color=MaritalStatus, shape=MaritalStatus, 
                            fill=MaritalStatus), size=5)+
  scale_shape_manual(values=21:24)+
  scale_color_brewer(palette = "Paired")+
  scale_fill_viridis_d(option="B")+
  scale_x_continuous(name="年份", breaks=seq(from=1978,to=2020, by=5))+
  scale_y_continuous(breaks = seq(from=0, to=85, by=5))+
  facet_wrap(~Sex, ncol=2)+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour="tan", size=0.05), 
        legend.position = c(0.1,0.8), 
        legend.background = element_blank(), legend.key = element_blank(),
        strip.background = element_blank(), strip.text = element_text(size=35),
        legend.title = element_text(size=35), legend.text = element_text(size=35),
        text = element_text(size=30))
p3

p3_1 <- ggplot()+
  geom_line(data=Marital_EDU, aes(x=Age,y=高教百分比, group=MaritalStatus, 
                                  color=MaritalStatus),size=3)+
  scale_color_brewer(palette = "Set1", direction = -1)+
  scale_x_continuous(name="年齡", breaks=seq(from=25,to=65, by=5),
                     limits = c(25,65))+
  scale_y_continuous(breaks = seq(from=0, to=85, by=5))+
  facet_wrap(~Sex, ncol=2)+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour="tan", size=0.05), 
        legend.position = c(0.9,0.75), 
        legend.background = element_blank(), legend.key = element_blank(),
        legend.title = element_text(size=35, color="forestgreen"),
        legend.text = element_text(size=30, color="sandybrown"),
        strip.background = element_blank(), strip.text = element_text(size=35),
        text = element_text(size=35))
p3_1
str(Marital_EDU)

WorkStatus_EDU <- (copy(Labor)
                [Age>=25,]
                [ , Weight:=ifelse((Year==2020), COUNT/11, COUNT/12)]
                [ , Sex:=factor(x=Sex, levels = Sex.code,labels = names(Sex.code))]
                [ , 高教:=ifelse((EDU %in% c(7,8,9,10)),1,0)]
                [ , .(高教百分比=sum((高教==1)*Weight)/sum(Weight)*100), 
                  by=.(Age,WorkStatus,Sex)]
                [])

WorkStatus_EDU[ ,WorkStatus:=as.character(WorkStatus)]
p4 <- ggplot()+
  geom_line(data=WorkStatus_EDU, aes(x=Year,y=高教百分比, group=WorkStatus, 
                                  color=WorkStatus))+
  geom_point(data=WorkStatus_EDU, aes(x=Year,y=高教百分比, group=WorkStatus, 
                                   color=WorkStatus, shape=WorkStatus, 
                                   fill=WorkStatus), size=5)+
  scale_shape_manual(values=21:24)+
  scale_color_brewer(palette = "Paired")+
  scale_fill_viridis_d(option="B")+
  scale_x_continuous(name="年份", breaks=seq(from=1978,to=2020, by=5))+
  scale_y_continuous(breaks = seq(from=0, to=85, by=5))+
  facet_wrap(~Sex, ncol=2)+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour="tan", size=0.05), 
        legend.position = c(0.6,0.8), 
        legend.background = element_blank(), legend.key = element_blank(),
        strip.background = element_blank(), strip.text = element_text(size=35),
        legend.text = element_text(size=30), legend.title = element_text(size=30),
        text = element_text(size=30))
p4
str(WorkStatus_EDU)

p4_1 <- ggplot()+
  geom_line(data=WorkStatus_EDU, aes(x=Age,y=高教百分比, group=WorkStatus, 
                                  color=WorkStatus),size=3)+
  scale_color_brewer(palette = "Set1", direction = -1)+
  scale_x_continuous(name="年齡", breaks=seq(from=25,to=65, by=5),
                     limits = c(25,65))+
  scale_y_continuous(breaks = seq(from=0, to=85, by=5))+
  facet_wrap(~Sex, ncol=2)+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour="tan", size=0.05), 
        legend.position = c(0.9,0.75), 
        legend.background = element_blank(), legend.key = element_blank(),
        legend.title = element_text(size=35, color="forestgreen"),
        legend.text = element_text(size=30, color="sandybrown"),
        strip.background = element_blank(), strip.text = element_text(size=35),
        text = element_text(size=35))
p4_1


cohorts

p2 <- ggplot()+
  geom_line(data=cohorts, aes(x=Age,y=高教百分比, group=出生年次, color=出生年次),size=3)+
  scale_color_brewer(palette = "Set1", direction = -1)+
  scale_x_continuous(name="年齡", breaks=seq(from=25,to=65, by=5),
                     limits = c(25,65))+
  scale_y_continuous(breaks = seq(from=0, to=85, by=5))+
  facet_wrap(~Sex, ncol=2)+
  theme_classic()+
  theme(panel.background = element_rect(fill=NA, colour="tan", size=0.05), 
        legend.position = c(0.9,0.75), 
        legend.background = element_blank(), legend.key = element_blank(),
        legend.title = element_text(size=35, color="forestgreen"),
        legend.text = element_text(size=30, color="sandybrown"),
        strip.background = element_blank(), strip.text = element_text(size=35),
        text = element_text(size=35))
p2
