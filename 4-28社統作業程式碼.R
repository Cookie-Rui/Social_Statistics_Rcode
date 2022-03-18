setwd("C:/Users/user/Downloads/Data-4-28-2021-20210428")
library(readxl)
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
store <- read_excel(path="四大超商.xlsx", sheet="7-Eleven")
setDT(store)
store <- store[ , 品牌:="7-Eleven"][] #新增品牌變項，內容為店家名稱
sheets <- excel_sheets(path="四大超商.xlsx") #列出excel檔中的各個sheet名稱
Stores <- data.table() #建立空data.table
for (i in sheets){  # 將迭代物件sheets的物件依序指定到i
  store <- read_excel(path="四大超商.xlsx", sheet=i) #讀取i表格
  setDT(store)
  store <- store[ , 品牌:=i][]
  Stores <- rbindlist(l=list(Stores,store), use.names=TRUE, fill=TRUE) 
  #用data.table的rbindlist來合併四個表格，用data.table的rbindlist可以調整是否對齊變項
  #一般的rbind無法調整，就算變項順序不一樣也不會自動對齊
}

#自定義函數
getOne <- function(i){ #創建一個函數，允許我們設定變數i，然後執行以下的東西
  store <- read_excel(path="四大超商.xlsx", sheet=i) 
  setDT(store)
  store <- store[ , 品牌:=i][]
  return(store) #該函數的結果會回傳store變數的資料 
}
Stores <- lapply(X=sheets, FUN=function(x){getOne(i=x)})#四個list
str(store)
Stores <- rbindlist(l=lapply(X=sheets, FUN=function(x){getOne(i=x)}),
                    use.names=TRUE, fill=TRUE)#加上rbindlist，變成一個data.table

STORES <- Stores[ , .(超商家數=.N), by=.(COUNTY,TOWN, COUNTY_ID, TOWN_ID)]
STORES[order(超商家數)]

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
population[order(老年人口百分比)]

fread(input="109年12月行政區工商家數 _鄉鎮市區.csv",colClasses="character",skip=1)
DATA <- fread(input="opendata109Y051.csv",colClasses="character", encoding = "UTF-8",skip=1)
DATA[ , ":="(COUNTY=str_sub(string = 區域別,start=1,end=3),
            TOWN=str_sub(string = 區域別,start=4,end=-1),
            VILLAGE=村里)]
DATA[ , ":="(COUNTY_ID=str_sub(string =區域別代碼,start=1 , end=5),
            TOWN_ID=str_sub(string =區域別代碼,start=1 , end=8),
            V_ID=paste0(str_sub(string =區域別代碼,start=1 , end=8),"-",
                        str_sub(string =區域別代碼,start=9 , end=11)))]
