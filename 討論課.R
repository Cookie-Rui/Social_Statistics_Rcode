setwd(你的位置)
pop <- fread(input="2020年村里15歲以上現住人口按性別、年齡、婚姻狀況及教育程度分.csv", skip=1,
             encoding="UTF-8",colClasses="character")
pop
names(pop) <- enc2native(names(pop))
pop[ , 人口數:=as.numeric(人口數)]
pop[ , ":="(COUNTY=str_sub(string = 區域別,start=1,end=3),
            TOWN=str_sub(string = 區域別,start=4,end=-1),
            VILLAGE=村里)]
pop[ , ":="(COUNTY_ID=str_sub(string =區域別代碼,start=1 , end=5),
            TOWN_ID=str_sub(string =區域別代碼,start=1 , end=8),
            V_ID=paste0(str_sub(string =區域別代碼,start=1 , end=8),"-",
                        str_sub(string =區域別代碼,start=9 , end=11)))]
pop[ , Age_lwr:=as.numeric(str_extract(string = 年齡, pattern = "^[0-9]{2,3}"))]
names(pop)[8] <- "edu"
names(pop)[9] <- "population"
pop[Age_lwr>=30 & Age_lwr<65,
    .(高等教育比例=sum((edu %in% c("博畢","碩畢","大畢"))* population) / 
                        sum(population)), by=.(TOWN_ID)]
pop[ , .(高等教育人數=sum(edu %in% c("博畢","碩畢","大畢"))* population), 
                         by=.(COUNTY_ID, TOWN_ID, COUNTY, TOWN)]