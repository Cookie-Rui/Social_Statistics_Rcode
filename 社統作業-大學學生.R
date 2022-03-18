#為避免錯誤，因此將變項名稱「日間/進修別」改成「日間or進修別」

library(readxl)

setwd("C:/Users/user/Downloads/108 collage") #記得改成正斜線

students<-read_excel(path="108學年度大專校院校別學生數.xlsx", sheet="108_student", skip=2)

str(students) #檢察屬性，變項前面會表示$
#日間進修別那一個變項的命名規則出問題(斜線)，所以記得檢查

students

#前置作業
under<- students[students$"日間or進修別"=="D 日" & students$"等級別"=="B 學士", ]
under$男生計<-as.numeric(under$男生計)
under$女生計<-as.numeric(under$女生計)
under$總計<-as.numeric(under$總計)
under$延修生男生<-as.numeric(under$延修生男生)
under$延修生女生<-as.numeric(under$延修生女生)
str(under)
under

#大學部性別比

under$性比例<-under$男生計 / under$女生計 * 100
head(under, 1)
under<-under[order(under$性比例, decreasing=TRUE), ]
sexrate<-as.data.frame(under[ ,c("學校名稱", "性比例")])
sexrate


#大學部人數

under<-under[order(under$總計, decreasing=TRUE), ]
the_number_of_students<-as.data.frame(under[ ,c("學校名稱", "總計" )])
the_number_of_students


#延畢率

under$延畢率<-(under$延修生男生+under$延修生女生)/under$總計*100
head(under, 1)
under<-under[order(under$延畢率, decreasing=TRUE), ]
delay_graduation<-under[ ,c("學校名稱", "延畢率")]
delay_graduation
