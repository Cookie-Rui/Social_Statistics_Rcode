以鄉鎮市區為單位
x2. 小學學齡兒童所佔的百分比(大於等於6歲，小於12歲)
x1. 老人所佔的百分比(大於等於65歲)
2020年9月村里資料
最後名稱：學號 姓名 

setwd("C:/Users/user/Downloads/Data 10-21-2020-20201021")
library(data.table) 
pop <- fread(input="村里戶數人口數單一年齡人口數10909M030.csv",
             skip=1, encoding="UTF-8", colClasses="character")
library(stringr) 
names(pop)
names(pop) <- str_replace_all(string = names(pop), pattern = c("-"=""))
names(pop)

library(tidyr) 
pop.byage <- subset(x=pop, select=-c(統計年月, 戶數, 人口數, 人口數男, 人口數女)) 
pop.byage
POP <- pivot_longer(data=pop.byage,
                    cols=str_subset(string = names(pop), pattern = "^[0-9]"),
                    names_to = "Group", values_to = "Population") 

POP$年齡 <- as.numeric(str_extract(string = POP$Group, pattern = "(^[0-9]{1,3})")) 
POP$Population <- as.numeric(POP$Population)
str(POP)

Total <- aggregate(formula=Population~區域別,data=POP, FUN="sum") 
head(Total) 
YOUNG <- subset(x=POP, subset=(年齡>=6 & 年齡<12))
young <- aggregate(formula=Population~區域別,data=YOUNG,FUN="sum") 
head(young)
OLD <- subset(x=POP, subset=(年齡>=65))
old <- aggregate(formula=Population~區域別, data=OLD, FUN="sum")
head(old)

Total$youngpopulation <- young$Population
Total$oldpopulation <- old$Population
Total$學齡人口占比 <- Total$youngpopulation / Total$Population * 100
Total$老年人口占比 <- Total$oldpopulation / Total$Population * 100
nrow(Total)==368

youngseq <- Total[order(Total$學齡人口占比, decreasing = TRUE), ] 
youngdata <- as.data.frame(youngseq[ ,c("區域別","學齡人口占比") ])
head(youngdata)

oldseq <- Total[order(Total$老年人口占比, decreasing = TRUE), ] 
olddata <- as.data.frame(oldseq[ ,c("區域別","老年人口占比") ])
head(olddata)
