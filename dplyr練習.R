用捷運進站資料
計算9月份每一站平日上班時段(7:00~9:00)平均進站人數計算出來
假日也是一樣的時段計算人數
*9/26(六)是平日，要注意

setwd("C:/Users/user/Downloads/Data 10-21-2020-20201021")
library(data.table)
pop <- fread(input="村里戶數人口數單一年齡人口數10909M030.csv", skip=1, encoding="UTF-8", colClasses="character")
#windows預設編碼是big-5,所以如果不是big-5, 要特別設定encoding
#sep則是設定csv檔案的分隔符號(如果不是逗號的話要特別設定)

library(stringr)
names(pop)
names(pop) <- str_replace_all(string = names(pop), pattern = c("-"=""))
str_subset(string = names(pop), pattern = "^[0~9]")
library(tidyr)
pop.byage <- subset(x=pop, select=-c(統計年月, 戶數, 人口數, 人口數男, 人口數女))
POP <- pivot_longer(data=pop.byage, 
                    cols=str_subset(string = names(pop), pattern = "^[0-9]"),
                    names_to = "Group", values_to = "Population")
POP$性別 <- str_extract(string = POP$Group, pattern = "[男女]$")
POP$年齡 <- as.numeric(str_extract(string = POP$Group, pattern = "(^[0-9]{1,3})"))
str(POP)
POP$Population <- as.numeric(POP$Population)
library(dplyr)
results <- summarize(.data=group_by(.data=POP, 區域別),
                     OLD=sum((年齡>=65)*Population)/sum(Population)*100,
                     KID=sum((6<=年齡 & 年齡<12)*Population)/sum(Population)*100)

results
results <- arrange(.data=results, KID)
results

library(data.table)
setwd("C:/Users/user/Downloads/Data 10-28-2020-20201028")
OUT <- fread(input="各站分時出站量統計.2020.09.csv", encoding="UTF-8", colClasses=list(character=1:2))
OUT <- mutate(.data=OUT, 日期=as.Date(x=日期), 時段=as.numeric(時段))
September <- unique(OUT$日期)
September
weekdays(September)





setwd("C:/Users/user/Downloads/Data 10-28-2020-20201028")
library(data.table)
library(dplyr)
IN <- fread(input="各站分時進站量統計.2020.09.csv", colClasses=list(character=1:2))
IN <- mutate(.data=IN, 日期=as.Date(x=日期), 時段=as.numeric(時段))
names(IN)
September <- unique(IN$日期)
September
weekdays(September)
weekday <- September[weekdays(September) %in% c("星期一","星期二","星期三","星期四","星期五")]
weekend <- September[weekdays(September) %in% c("星期六","星期日")]
weekend <- weekend[-7]
weekday <- sort(c(weekday,as.Date("2020-09-26")))
weekday

head(IN)

