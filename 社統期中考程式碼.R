setwd("C:/Users/user/Downloads/midterm exam")
library(data.table)
normal <- fread(input="村里總人口數按單一年齡分.csv",
                encoding="UTF-8",colClasses="character")
indie <- fread(input="原住民人口按性別與原住民族別分.csv",
               encoding="UTF-8",colClasses="character")

library(stringr)
library(tidyr)
NORMAL <- pivot_longer(data=normal,
                       cols = str_subset(string=names(normal), pattern ="^[男女]"),
                       names_to="group", values_to="population")

names(indie) <- str_replace_all(string = names(indie), pattern = c("_"=""))
INDIE <- pivot_longer(data=indie,
                      cols = str_subset(string=names(indie), pattern ="[男女]$"),
                      names_to="group", values_to="population")
str(NORMAL)
str(INDIE)
NORMAL$population <- as.numeric(NORMAL$population)
INDIE$population <- as.numeric(INDIE$population)
NORMAL1 <- aggregate(formula=population~區域別代碼+區域別+村里,data=NORMAL,FUN="sum")
NORMAL1 <- NORMAL1[order(NORMAL1$區域別代碼), ]
INDIE1 <- aggregate(formula=population~區域別代碼+區域別+村里名稱,data=INDIE,FUN="sum")
INDIE1 <- INDIE1[order(INDIE1$區域別代碼), ]

ALLnormal <- sum(NORMAL1$population)
ALLindie <- sum(INDIE1$population)
NORMAL1$人口比例 <- NORMAL1$population/ALLnormal
INDIE1$人口比例 <- INDIE1$population/ALLindie
p1p2 <- abs(NORMAL1$人口比例 - INDIE1$人口比例)
dataframe <- data.frame(區域別代碼=NORMAL1$區域別代碼,
                             區域別=NORMAL1$區域別,
                             村里名稱=NORMAL1$村里,
                             比例差絕對值=p1p2)

dataframe1 <- aggregate(formula=比例差絕對值~區域別, data=dataframe, FUN="sum")
dataframe1$dissimilarity <- dataframe1$比例差絕對值*0.5
result <- data.frame(鄉鎮=dataframe1$區域別, 居住隔離指數=dataframe1$dissimilarity)
result[order(result$居住隔離指數, decreasing=TRUE), ]
nrow(result)==368
