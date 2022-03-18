setwd("C:/Users/user/Downloads")
library(readxl)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
DATA1 <- fread(input="109年12月行政區工商家數_鄉鎮市區.csv",colClasses="character",skip=1)
DATA2 <- fread(input="2021年02月鄉鎮市區單齡人口數按性別分.csv",colClasses="character")
DATA3 <- read_excel(path="鄉鎮市區土地面積.xlsx", skip=1)
DATA4 <- read_excel(path="四大超商門市.xlsx", skip=1)
setDT(DATA1)
setDT(DATA2)
setDT(DATA3)
setDT(DATA4)
names(DATA1)[1:4] <- c("COUNTY_ID", "COUNTY", "TOWN_ID", "TOWN")
names(DATA4)[6:9] <- c("TOWN_ID", "TOWN", "COUNTY_ID", "COUNTY")


location <- DATA1[ , .(TOWN_ID,TOWN,COUNTY_ID,COUNTY)]
convenient <- DATA4[ , .N, keyby=.(TOWN_ID,TOWN,COUNTY_ID,COUNTY)]
names(convenient)[5] <- "便利商店數量"
pop <- setDT(pivot_longer(data=DATA2,
                          cols = str_subset(string=names(DATA2), pattern ="^[男女]"),
                          names_to="group", values_to="population"))
pop$population <- as.numeric(pop$population)
POP <- pop[ ,sum(population), keyby=.(TOWN_ID,TOWN,COUNTY_ID,COUNTY)]
work <- DATA1[ , 1:5]
MAIN <- merge(location,convenient,by=intersect(names(location), names(convenient)), all.x=TRUE)
MAIN <- merge(MAIN,POP,keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY), all.x=TRUE)
MAIN <- left_join(MAIN, DATA3)
MAIN <- merge(MAIN,work,keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY), all.x=TRUE)
MAIN[ , 區域別:=NULL]
MAIN <- MAIN[, 便利商店數量:=ifelse(is.na(便利商店數量), 0,便利商店數量 )]
names(MAIN)[6] <- "人口數"


MAIN <- MAIN[ , 每千人口數:=MAIN$人口數/1000]
MAIN <- MAIN[ , 每千便利商店數:=便利商店數量/每千人口數]
MAIN <- MAIN[ , 每平方公里工商家數:=as.numeric(工商業總家數)/土地面積]
with(data=MAIN, plot(x=每千便利商店數, y=每平方公里工商家數, ylim=c(0,1000), xlim=c(0,0.5)))
result <- lm(data=MAIN,formula=每平方公里工商家數~每千便利商店數 )
abline(coef(result), lwd=3)