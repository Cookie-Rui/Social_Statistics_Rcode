library(data.table)
library(stringr)
setwd("C:/Users/user/Downloads")
POP <- fread(input="2020年底村里戶數人口數單一年齡人口數.csv", skip=1,
             encoding="UTF-8",colClasses="character")
names(POP) #我們讀進來時設定的變項型態為character，但有一些變項是數字，所以要做轉換
names(POP) <- enc2native(names(POP)) #把編碼改為現在這台電腦的編碼，windows：BIG-5，mac：UTF-8

number <- str_subset(string = names(POP), pattern="[數歲男女]")
POP <- (copy(POP)
        [ , (number):=lapply(.SD, as.numeric), .SDcols=number]
        [])

str(POP)

asample <- (copy(POP)
            [sample(.N, size = 20, replace = FALSE), ]
            [])
# .N：是data.table內建的變項，指一個資料中的個案數
# .I：是data.table內建的變項，指一筆個案在資料中的序號

POP <- (copy(POP)
        [ , COUNTY:=str_sub(string = 區域別, start=1, end=3)]
        [ , TOWN:=str_sub(string = 區域別, start=4, end=-1)]
        [ , VILLAGE:=村里]
        [ , ":="(COUNTY_ID=str_sub(string = 區域別代碼, start=1, end=5), 
                 TOWN_ID=str_sub(string = 區域別代碼, start=1, end=8), 
                 ])


總戶數 <- (copy(POP)
        [ , .(總戶數=sum(戶數),總人口數=sum(人口數)),
          by=.()])
# .()：是list()在data.table的縮寫，指建立一個list

dt[ i , j , by ]
i：選取符合條件的row資料(index)
j：1. 選colum資料 2.做計算(job)
by：分組(group)

dt[ , 運算式]==>向量形式
dt[ , .(運算式)]==>data table形式


