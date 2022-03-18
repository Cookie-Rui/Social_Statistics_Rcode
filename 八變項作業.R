setwd("C:/Users/user/Downloads/Data-4-28-2021-20210428")
library(readxl)
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)

#總人口數&老年人口比例---------------------------------------------
pop <- setDT(read_excel(path="2020年鄉鎮市區單一年齡人口數.xlsx"))
POP <- melt(data=pop, id.vars = c("COUNTY","TOWN", "COUNTY_ID", "TOWN_ID","性別"),
            measure.vars=paste(0:100),
            variable.name="Age", value.name="人口數",
            variable.factor=FALSE, value.factor=FALSE)
population <- (POP[ , Age:=as.numeric(Age)]
               [ , .(總人口數=sum(人口數),
                         老年人口百分比=sum((Age>=65)*人口數)/sum(人口數)*100),
                 by=.(COUNTY,TOWN, COUNTY_ID, TOWN_ID)]
               [])

#超商密度------------------------------------
sheets <- excel_sheets(path="四大超商.xlsx")
getOne <- function(i){ 
  store <- read_excel(path="四大超商.xlsx", sheet=i) 
  setDT(store)
  store <- store[ , 品牌:=i][]
  return(store) 
}
Stores <- lapply(X=sheets, FUN=function(x){getOne(i=x)})
str(Stores)
Stores <- rbindlist(l=lapply(X=sheets, FUN=function(x){getOne(i=x)}),
                    use.names=TRUE, fill=TRUE)
STORES <- Stores[ , .(超商家數=.N), by=.(COUNTY,TOWN, COUNTY_ID, TOWN_ID)]
profile <- merge(population,STORES,keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY), all.x=TRUE)
profile <- profile[, 超商家數:=ifelse(is.na(超商家數), 0,超商家數 )]
profile[ , 超商每千人密度:=超商家數/(總人口數/1000)]

#工商繁榮數-----------------------------------------
industry <- fread(input="109年12月行政區工商家數_鄉鎮市區.csv",colClasses="character",skip=1)
names(industry)[1:4] <- c("COUNTY_ID", "COUNTY", "TOWN_ID", "TOWN")
INDUSTRY <- industry[ ,1:5]
area <- setDT(read_excel(path="鄉鎮市區土地面積.xlsx"))
area <- area[(as.numeric(TOWN_ID)-100000)>0]
profile <- merge(profile,INDUSTRY,keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY), all.x=TRUE)
profile <- merge(profile,area,by=.(TOWN_ID), all.x=TRUE)
profile <- left_join(profile,area)
profile[ , 工商繁榮度:=as.numeric(工商業總家數)/土地面積]

#單獨生活戶-----------------------------------------
house <- fread(input="2020年村里戶數、人口數按戶別及性別.csv",colClasses="character",
                skip=1,encoding="UTF-8")
house[ , ":="(COUNTY=str_sub(string = 區域別,start=1,end=3),
               TOWN=str_sub(string = 區域別,start=4,end=-1),
               VILLAGE=村里名稱)]
house[ , ":="(COUNTY_ID=str_sub(string =區域別代碼,start=1 , end=5),
               TOWN_ID=str_sub(string =區域別代碼,start=1 , end=8),
               V_ID=paste0(str_sub(string =區域別代碼,start=1 , end=8),"-",
                           str_sub(string =區域別代碼,start=9 , end=11)))]
single <- melt(data=house,
               id.vars = c("COUNTY_ID", "COUNTY", "TOWN_ID", "TOWN","V_ID", "VILLAGE"),
               measure.vars = str_subset(string = names(house), pattern = "^[單共]"),
               variable.name="類型")
Single <- (single[ , .(總戶數=sum(as.numeric(value)),
                          單獨生活戶戶數=sum((類型=="單獨生活戶_戶數")*as.numeric(value))),
                   keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY) ])
profile <- merge(profile,Single,keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY), all.x=TRUE)
profile[ ,單獨生活戶:=單獨生活戶戶數/總戶數*100 ]


#未婚人口$喪偶人口-----------------------------------
Data <- fread(input="opendata109Y051.csv",colClasses="character", encoding = "UTF-8",skip=1)
Data[ , ":="(COUNTY=str_sub(string = 區域別,start=1,end=3),
             TOWN=str_sub(string = 區域別,start=4,end=-1),
             VILLAGE=村里)]
Data[ , ":="(COUNTY_ID=str_sub(string =區域別代碼,start=1 , end=5),
             TOWN_ID=str_sub(string =區域別代碼,start=1 , end=8),
             V_ID=paste0(str_sub(string =區域別代碼,start=1 , end=8),"-",
                         str_sub(string =區域別代碼,start=9 , end=11)))]
Data[ , Age_lwr:=as.numeric(str_extract(string = 年齡, pattern = "^[0-9]{2,3}"))]
names(Data)[9] <- "POP"
A <- Data[Age_lwr %in% c(30,35,40,45) & 性別=="男",.(Apop=sum(as.numeric(POP))),keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
B <- Data[Age_lwr %in% c(30,35,40,45) & 性別=="女",.(Bpop=sum(as.numeric(POP))),keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
C <- Data[Age_lwr %in% c(30,35,40,45) & 性別=="男" & 婚姻狀況=="未婚",.(Cpop=sum(as.numeric(POP))),keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
D <- Data[Age_lwr %in% c(30,35,40,45) & 性別=="女" & 婚姻狀況=="未婚",.(Dpop=sum(as.numeric(POP))),keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
E <- Data[Age_lwr>=65 & 性別=="男",.(Epop=sum(as.numeric(POP))),keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
F_ <- Data[Age_lwr>=65 & 性別=="女",.(Fpop=sum(as.numeric(POP))),keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
G <- Data[Age_lwr>65 & 性別=="男" & 婚姻狀況 %in% c("喪偶_不同性別","喪偶_相同性別"),.(Gpop=sum(as.numeric(POP))),keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
H <- Data[Age_lwr>65 & 性別=="女" & 婚姻狀況 %in% c("喪偶_不同性別","喪偶_相同性別"),.(Hpop=sum(as.numeric(POP))),keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]

DATA <- A[ ,.(TOWN_ID, TOWN, COUNTY_ID, COUNTY)]
DATA <- DATA[ , ":="(男性未婚=C$Cpop/A$Apop*100,
                         女性未婚=D$Dpop/B$Bpop*100,
                         男性喪偶=G$Gpop/E$Epop*100,
                         女性喪偶=H$Hpop/F_$Fpop*100)]
profile <- merge(profile,DATA,keyby=.(TOWN_ID, TOWN, COUNTY_ID, COUNTY), all.x=TRUE)

#整理&畫圖
RESULT <- profile[ , .(TOWN_ID, TOWN, COUNTY_ID, COUNTY, 超商每千人密度,工商繁榮度,老年人口百分比,單獨生活戶,
             男性未婚,女性未婚,男性喪偶,女性喪偶)]
RESULT <- RESULT[order(TOWN_ID)]
library(ggplot2)
library(GGally)
ggpairs(data=RESULT[, .(超商每千人密度,工商繁榮度,老年人口百分比,單獨生活戶,
                               男性未婚,女性未婚,男性喪偶,女性喪偶)],
        diag=list(continuous=wrap("barDiag", binwidth=1, fill=" tomato")))