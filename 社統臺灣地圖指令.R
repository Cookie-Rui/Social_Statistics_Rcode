load(file.choose())四次
qtm(County)
qtm(Town)
qtm(Town, fill=NULL)
qtm(Town, fill=NULL)+qtm(County, borders="想要的顏色", fill=NULL)
qtm(Town, borders="grey", fill=NULL)+qtm(County, borders="goldenrod", fill=NULL)+qtm(PxMart, symbols.shape=22, symbols.size=0.1, symbols.col="green")+tm_compass(type="8star", position=c("left", "top"), show.labels=2)

+號
esc中止
方向鍵上，重複指令

rstudio:
指令上面下面寫都可以
上面的指令執行方法:
1.全選按右上的run
2.如果只有一行，可以按ctrl+enter

r語言是「物件導向」
每個東西的都是物件
必須要做命名(naming)
l I 1
0 o O
q 9
命名要「友善」一點(不要太短無法辨識，太長也苦了自己)

a-z(開頭)
0-9(盡量不要用在開頭)
. _(也不要太常用)

其他不要用在命名
減號不要用，盡量不要空格
中文編碼：Bip-5(要小心編碼)
因此要少用中文

x<-1:49
lottery.sample<-replicate(200,sort(sample(x, size=6, replace=FALSE)))

print(lottery.sample)

tr.lottery.sample<- t(lottery.sample)

print(tr.lottery.sample)

result.numbers<-c(11,15,16,18,24,26)

n<-1
NROW<-nrow(tr.lottery.sample)-199
while(NROW <= 200){
  print(summary(tr.lottery.sample[n, ] == result.numbers))
  n<-n+1
  NROW<-NROW+1
}

如何指定物件?
透過一些行為(expression)來創造了一個物件(object)--->assignment指定
IN=3+2(只在括號裡用=)
IN<-3+2(最建議的操作)
3+2->IN(少用，不易閱讀)

** =的意義 **
1.IN=3+2 ---->將3+2的結果指定為in
2.x = 3 ------>x是否等於3?? 出來會是一個邏輯運算，而不是數值運算(x == 3)
3.括號裡的=是指定的意思