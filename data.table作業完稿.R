library(dplyr)
library(data.table)
library(readxl)
library(stringr)

setwd("C:/Users/user/Downloads/Data-3-10-2021-20210310")
POP <- fread(input="2020年底村里戶數人口數單一年齡人口數.csv", skip=1,
             encoding="UTF-8",colClasses="character")
POP1 <- fread(input="2020年村里戶數、人口數按戶別及性別.csv", skip=1,
             encoding="UTF-8",colClasses="character")
POP2 <- fread(input="2020年村里15歲以上現住人口按性別、年齡、婚姻狀況及教育程度分.csv", skip=1,
             encoding="UTF-8",colClasses="character")
POP3 <- read_excel(path="鄉鎮市區土地面積.xlsx")

names(POP) <- enc2native(names(POP)) 
number <- str_subset(string = names(POP), pattern="[數歲男女]")
POP <- (copy(POP)
        [ , (number):=lapply(.SD, as.numeric), .SDcols=number]
        [])
POP<-(copy(POP)
      [,COUNTY:=str_sub(string = 區域別,start=1,end=3)]
      [,TOWN:=str_sub(string = 區域別,start=4,end=-1)]
      [,VILLAGE:=村里]
      [,":="(COUNTY_ID=str_sub(string =區域別代碼,start=1 , end=5),
             TOWN_ID=str_sub(string =區域別代碼,start=1 , end=8),
             V_ID=paste0(str_sub(string =區域別代碼,start=1 , end=8),"-",
                         str_sub(string =區域別代碼,start=9 , end=11)))]
      [])
str(POP)

names(POP1) <- enc2native(names(POP1)) 
number1 <- str_subset(string = names(POP1), pattern="[數男女]")
POP1 <- (copy(POP1)
        [ , (number1):=lapply(.SD, as.numeric), .SDcols=number1]
        [])
str(POP1)
POP1 <-(copy(POP1)
      [,COUNTY:=str_sub(string = 區域別,start=1,end=3)]
      [,TOWN:=str_sub(string = 區域別,start=4,end=-1)]
      [,VILLAGE:=村里名稱]
      [,":="(COUNTY_ID=str_sub(string =區域別代碼,start=1 , end=5),
             TOWN_ID=str_sub(string =區域別代碼,start=1 , end=8),
             V_ID=paste0(str_sub(string =區域別代碼,start=1 , end=8),"-",
                         str_sub(string =區域別代碼,start=9 , end=11)))]
      [])
str(POP1)

names(POP2) <- enc2native(names(POP2))
POP2[ , 人口數:=as.numeric(人口數)]
POP2[ , ":="(COUNTY=str_sub(string = 區域別,start=1,end=3),
            TOWN=str_sub(string = 區域別,start=4,end=-1),
            VILLAGE=村里)]
POP2[ , ":="(COUNTY_ID=str_sub(string =區域別代碼,start=1 , end=5),
            TOWN_ID=str_sub(string =區域別代碼,start=1 , end=8),
            V_ID=paste0(str_sub(string =區域別代碼,start=1 , end=8),"-",
                        str_sub(string =區域別代碼,start=9 , end=11)))]
POP2[ , Age_lwr:=as.numeric(str_extract(string = 年齡, pattern = "^[0-9]{2,3}"))]
names(POP2)[8] <- "edu"
names(POP2)[9] <- "population"
str(POP2)

Average_household_size <-(copy(POP)
                          [,.(總戶數=sum(戶數),總人口數=sum(人口數)),
                            by=.(COUNTY_ID,TOWN_ID,COUNTY,TOWN)]
                          [,戶量:=總人口數/總戶數]
                          [])

population<-melt(data=POP,
                 id.vars = c("COUNTY_ID","TOWN_ID","V_ID","COUNTY","TOWN","VILLAGE"),
                 measure.vars = str_subset(string = names(POP),pattern = "^[0-9]{0,3}歲"),
                 variable.name="AgeGroup",variable.factor=F,value.name="Population")
str(population)

Ages <- (copy(population)
         [ ,":="(性別=str_extract(string=AgeGroup, pattern = "[男女]$"),
                   年齡=as.numeric(str_extract(string = AgeGroup,pattern = "^[0-9]{1,3}")),
                   人口數=as.numeric(Population))]
         [ ,.(COUNTY_ID, TOWN_ID, V_ID, COUNTY, TOWN, VILLAGE, 性別, 年齡, 人口數) ]
         [])
Ages

#平均年齡
Town.Age <- (copy(Ages)
             [ , .(MeanAge=sum((年齡+0.5)*人口數)/sum(人口數)),
               by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
             [])
Town.Age
profile <- merge(x=Average_household_size, y=Town.Age, 
                 by=intersect(names(Average_household_size), names(Town.Age)))
profile
#老年人口百分比
Town.Old <- (copy(Ages)
             [ , .(Old=sum((年齡>=65)*人口數)/sum(人口數)*100),
               by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
             [])
Town.Old
profile1 <- merge(x=profile, y=Town.Old, 
                  by=intersect(names(profile), names(Town.Old)))
profile1

#老化指數
Town.Aging <- (copy(Ages)
               [ , .(老化指數=sum((年齡>=65)*人口數)/sum((年齡<=14)*人口數)*100),
                 by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
               [])
Town.Aging
profile2 <- merge(x=profile1, y=Town.Aging, 
                  by=intersect(names(profile1), names(Town.Aging)))
profile2

#高等教育比例
Advaced_edu <- POP2[Age_lwr>=30 & Age_lwr<65,
                   .(高等教育比例=sum((edu %in% c("博畢","碩畢","大畢"))* population) / 
                                  sum(population)), by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
profile3 <- merge(x=profile2, y=Advaced_edu, 
                  by=intersect(names(profile2), names(Advaced_edu)))
profile3

#女性未婚率
Unmarried_rate <- POP2[Age_lwr>=30 & Age_lwr<50 & 性別 %in% c("女"),
                       .(女性未婚比例=sum((婚姻狀況 %in% c("未婚")) * population) / 
                                 sum(population)), 
                       by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
profile4 <- merge(x=profile3, y=Unmarried_rate, 
                  by=intersect(names(profile3), names(Unmarried_rate)))
profile4

#0~14歲人口比例
Under15_rate <- Ages[ , .(Young=sum((年齡>=0 & 年齡<15)*人口數)/sum(人口數)*100),
                            by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
profile5 <- merge(x=profile4, y=Under15_rate, 
                  by=intersect(names(profile4), names(Under15_rate)))
profile5

#人口密度
names(POP3)[2] <- "TOWN"
profile6<- merge(x=profile5, y=POP3, 
                  by=intersect(names(profile5), names(POP3)))
profile6
profile6[ ,人口密度:=總人口數 / 土地面積]
profile6

#老年女性喪偶率
F_Widowd_rate <- POP2[Age_lwr>=65 & 性別 %in% c("女") , 
                      .(老年女性喪偶率=sum((婚姻狀況 %in% c("喪偶_不同性別","喪偶_相同性別"))*
                                      population)/sum(population)),
                      by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
profile7<- merge(x=profile6, y=F_Widowd_rate, 
                 by=intersect(names(profile6), names(F_Widowd_rate)))
profile7

#單獨生活戶百分比
unique(POP1$類型)
POP1 <-melt(data=POP1,
            id.vars = c("COUNTY_ID","TOWN_ID","V_ID","COUNTY","TOWN","VILLAGE"),
            measure.vars = str_subset(string = names(POP1),pattern = "[男女]$"),
            variable.name="類型",variable.factor=F,value.name="戶數")
single_living <- POP1[ , .(單獨生活戶百分比=
                                     sum((類型 %in% c("單獨生活戶_男","單獨生活戶_女"))*戶數)/
                                     sum(戶數)*100),
                       by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
profile8<- merge(x=profile7, y=single_living, 
                 by=intersect(names(profile7), names(single_living)))
profile8

#男性單獨生活戶百分比
M_single_living <- POP1[ ,
                         .(男性單獨生活戶百分比=
                                       sum((類型 %in% c("單獨生活戶_男"))*戶數)/
                                       sum((類型 %in% c("共同生活戶_男","共同事業戶_男",
                                                      "單獨生活戶_男"))*戶數)*100),
                       by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
profile9<- merge(x=profile8, y=M_single_living, 
                 by=intersect(names(profile8), names(M_single_living)))
profile9

#女性單獨生活戶百分比
F_single_living <- POP1[ ,
                         .(女性單獨生活戶百分比=
                                       sum((類型 %in% c("單獨生活戶_女"))*戶數)/
                                       sum((類型 %in% c("共同生活戶_女","共同事業戶_女",
                                                      "單獨生活戶_女"))*戶數)*100),
                         by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]
profile10<- merge(x=profile9, y=F_single_living, 
                 by=intersect(names(profile9), names(F_single_living)))
profile10
profile10 <- profile10[order(TOWN_ID)]
