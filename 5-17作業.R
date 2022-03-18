#七類家庭終生消費支出差異-------------------------------------
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
str(Person)
Person[ , .N, by=.(b3)]
Person[ , .N, by=.(b13)]
items <- str_subset(string = names(Person), pattern = "^itm[0-9]{2,4}")
items
Person <- (Person
           [ , (items):=lapply(X=.SD,
                               function(x){ifelse(is.na(x),0,x)}),
             .SDcols=items]
           [])
Person[ , mean(itm1000/a8)]
Person <- (Person
           [ , ":="(Sex=factor(x=b3, levels = 1:2, labels = c("男","女")),
                    Age=ifelse((b4>=95), 95, b4), Weight=a20)]
           [ , FamilyType:=factor(x=floor(a18/100), levels = 1:7,
                                  labels = c("單人家庭","夫婦二人家庭","單親家庭",
                                             "核心家庭","祖孫二代","三代同堂","其他"))]
           [ , 年齡:=Age]
           [])
scale <- setDT(readxl::read_excel(path="EquivalenceScale.xlsx"))
scale <- scale[ , .(年齡=Age, Scale)]
scale[年齡==75, ]
scale[年齡==75, .(年齡=76:95,Scale)]
scale <- rbindlist(l=list(scale,scale[年齡==75, .(年齡=76:95,Scale)]), use.names = TRUE, fill=TRUE)
scale
ITM1000 <- merge(x=Person, y=scale,by="年齡", all.x=TRUE)
ITM1000
ITM1000 <- (copy(ITM1000)
            [ , 消費性支出:=itm1000*Scale/sum(Scale), by=.(Household_ID)])

LifeTime <- (copy(ITM1000)
             [ , .(支出=sum(消費性支出*a20)/sum(a20)),
               keyby=.(Age,FamilyType)])

pp <- ggplot()+
  geom_line(data=LifeTime, aes(x=Age, y=支出, group=FamilyType, color=FamilyType))+
  geom_point(data=LifeTime, aes(x=Age, y=支出, group=FamilyType, color=FamilyType,
                                shape=FamilyType, fill=FamilyType), size=4)+
  scale_shape_manual(values = 19:25)+
  scale_color_brewer(palette = "Paired")+
  scale_fill_viridis_d(option = "A")+
  scale_x_continuous(name="年齡", breaks=seq(from=0,to=95, by=5),
                     expand = expansion(mult = c(0.02,0.02)))+
  scale_y_continuous(name="消費性支出", breaks=seq(from=100000, to=360000, by=50000),
                     limits=c(100000, 360000))+
  theme(axis.text = element_text(size=13), axis.title = element_text(size=18),
        panel.background = element_rect(colour = "tan", fill=NA), 
        panel.grid = element_blank(), plot.background = element_blank(),)

pp

data <- setDT(readxl::read_excel(path = "income_cost_data.xlsx"))
DATA <- data[ , .(...1,單人比例=單人/sum(單人)*100,
                夫婦比例=夫婦/sum(夫婦)*100,
              單親比例=單親/sum(單親)*100,
              核心比例=核心/sum(核心)*100,
              祖孫比例=祖孫/sum(祖孫)*100,
              三代比例=三代/sum(三代)*100,
              其他比例=其他/sum(其他)*100)]

#迴歸分析---------------------------------------------------
setwd("C:/Users/user/Downloads/r folder (second semester)")
Duncan <- setDT(readxl::read_excel(path="DuncanSEI.xlsx"))

A <- summary(lm.beta::lm.beta(lm(data=Duncan, formula=Prestige~Income+EDU)))
B <- summary(lm.beta::lm.beta(lm(data=Duncan, formula=Prestige~Income)))
C <- summary(lm.beta::lm.beta(lm(data=Duncan, formula=EDU~Income)))
Dtable <- data.frame(cbind(B$residuals, C$residuals))
D <- summary(lm.beta::lm.beta(lm(data=Dtable, formula=X1~X2)))
coef(A)
coef(B)
coef(C)
coef(D)

A <- summary(lm.beta::lm.beta(lm(data=Duncan, formula=Prestige~Income+EDU)))
B <- summary(lm.beta::lm.beta(lm(data=Duncan, formula=Prestige~EDU)))
C <- summary(lm.beta::lm.beta(lm(data=Duncan, formula=Income~EDU)))
Dtable <- data.frame(cbind(B$residuals, C$residuals))
D <- summary(lm.beta::lm.beta(lm(data=Dtable, formula=X1~X2)))
coef(A)
coef(B)
coef(C)
coef(D)