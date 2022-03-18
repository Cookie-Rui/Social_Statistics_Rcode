setwd("C:/Users/user/Downloads/Data 10-28-2020-20201028")
library(data.table)
library(dplyr)
IN <- fread(input="各站分時進站量統計.2020.09.csv", colClasses=list(character=1:2))
str(IN)
IN$日期 <- as.Date(x=IN$日期)
IN$時段 <- as.numeric(IN$時段)
str(IN)

September <- unique(IN$日期)
September
weekday <- September[weekdays(September) %in% c("星期一","星期二","星期三","星期四","星期五")]
weekend <- September[weekdays(September) %in% c("星期六","星期日")]
weekend <- weekend[-7]
weekday <- sort(c(weekday,"2020-09-26"))
weekday
weekend

weekdaydata <- IN[IN$日期 %in% weekday & IN$時段 %in% c(7,8), ]
unique(weekdaydata$日期)
unique(weekdaydata$時段)
weekenddata <- IN[IN$日期 %in% weekend & IN$時段 %in% c(7,8), ]
unique(weekenddata$日期)
unique(weekenddata$時段)

WEEKDAYDATA <- pivot_longer(data=weekdaydata,
                            cols = subset(x=names(IN), subset=((names(IN)==c("日期","時段")) %in% FALSE)),
                            names_to="station", values_to="population")

WEEKENDDATA <- pivot_longer(data=weekenddata,
                            cols = subset(x=names(IN), subset=((names(IN)==c("日期","時段")) %in% FALSE)),
                            names_to="station", values_to="population")

str(WEEKDAYDATA)
str(WEEKENDDATA)

WEEKDAYSUM <- summarize(.data=group_by(.data=WEEKDAYDATA, station),
                        POP=sum(population)/23)
WEEKENDSUM <- summarize(.data=group_by(.data=WEEKENDDATA, station),
                        POP=sum(population)/7)

result <- bind_cols(站名=WEEKDAYSUM$station,平日=WEEKDAYSUM$POP, 假日=WEEKENDSUM$POP)
result
