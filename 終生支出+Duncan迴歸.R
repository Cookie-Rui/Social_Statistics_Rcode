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
ALL_scale <- ITM1000[ ,.(ALL_scale=sum(Scale)) ,keyby=Household_ID]
PERSON <- merge(x=ITM1000, y=ALL_scale, by="Household_ID")
PERSON[,.(Household_ID, itm1000, Scale, ALL_scale)]
PERSON[ , 消費性支出:=(itm1000*(Scale/ALL_scale))]
PERSON[ , sum(消費性支出*Weight)/sum(Weight)]
PERSON[, .(支出=sum(消費性支出*Weight)/sum(Weight)), keyby=.(Age)]

LifeTime <- (copy(PERSON)
             [ , .(支出=sum(消費性支出*a20)/sum(a20)), keyby=.(Age)]
             [])
LifeTime[ , .(終生支出=sum(支出))]
LifeTime <- (copy(PERSON)
             [ , .(支出=sum(消費性支出*a20)/sum(a20)), keyby=.(Sex,Age)]
             [])
LifeTime[ , .(終生支出=sum(支出)), by=.(Sex)]

p0 <- ggplot()+
  geom_boxplot(data=PERSON, aes(x=Age, y=消費性支出, group=Age, weight=Weight))
p0
p1 <- ggplot()+
  geom_boxplot(data=PERSON, aes(x=Age, y=消費性支出, group=Age, weight=Weight), 
               outlier.shape=NA, color="sandybrown", fill="powderblue")+
  scale_x_continuous(name="年齡", breaks=seq(from=0, to=95, by=5), 
                     expand=expansion(mult = c(0.02,0.02)))+
  scale_y_continuous(name = "消費性支出",
                     breaks=seq(from=50000, to=650000, by=50000),
                     limits=c(50000, 650000))+
  theme(axis.text = element_text(size=13), axis.title = element_text(size=18), 
        panel.background = element_rect(colour = "tan", fill = NA),
        panel.grid = element_blank(), plot.background = element_blank(),)
p1


#迴歸分析---------------------------------------------------
setwd("C:/Users/user/Downloads/r folder (second semester)")
Duncan <- setDT(readxl::read_excel(path="DuncanSEI.xlsx"))

#correlation matrix-----------------------------------------
results <- cor(Duncan[ , .(Prestige, Rating, EDU, Income, SEI)])
results
options(scipen = 999)

#計算相關係數的信賴區間-------------------------------------
results <- psych::corr.test(Duncan[ , .(Prestige, Rating, EDU, Income, SEI)])
str(results)
results$r
results$t
results$p
results$se
results$ci

results <- lm(formula=Prestige~Income, data=Duncan)
str(results)
coef(results)
summary(results)
anova(results)

#我們可以將x變項變成標準化數據，y變項也標準化，然後再進行迴歸分析，此時也可以得出標準化的迴歸係數
results.beta <- lm.beta::lm.beta(results)
anova(results.beta)
summary(results.beta)
#標準化迴歸係數中，截距必為0，因為模型必經過x跟y的平均，此時是(0,0)
#斜率的絕對值不會大於1，且等於相關係數，因為原本斜率m = r * sd(y)/sd(x)，現在sd(y)跟sd(x)都是1，所以m=r
results$residuals
results$fitted.values


summary(lm(data=Duncan, formula=Prestige~Income+EDU))
summary(lm(data=Duncan, formula=Prestige~Income))
summary(lm(data=Duncan, formula=Income~EDU))
anova((lm(data=Duncan, formula=Prestige~Income+EDU)))
