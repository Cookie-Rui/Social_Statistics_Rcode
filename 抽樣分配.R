setwd("C:/Users/user/Downloads/midterm exam/Census 2000-20201007")
load(file="Census.2000.RData")
str(Census.2000)

POP <- Census.2000$A020 #母體分布
N <- 120
asample <- sample(x=POP, size=N, replace=TRUE)
asample #樣本分布
hist(x=asample, breaks=30)

mean(asample) #樣本統計值
xbar <- mean(sample(x=POP, size=N, replace=TRUE))

plot(x=c(0,5), y=c(30,40), type="n", ylab = bquote(bar(x)), xlab="trial", las=1)
points(x=1, y=xbar, pch=24, col="orange", bg="gold", cex=2)
text(x=1.5, y=xbar, label=bquote(bar(italic(x))==.(format(xbar, digits = 4))), cex=1, col="blue")

xbar <- mean(sample(x=POP, size=N, replace=TRUE))
xbar
points(x=2, y=xbar, pch=24, col="orange", bg="gold", cex=2)

xbar <- mean(sample(x=POP, size=N, replace=TRUE))
xbar
points(x=3, y=xbar, pch=24, col="orange", bg="gold", cex=2)

plot(x=c(0,21), y=c(30,40), type="n", ylab = bquote(bar(x)), xlab="trial", las=1)
for(i in 1:20){
  xbar <- mean(sample(x=POP, size=N, replace=TRUE))
  points(x=i, y=xbar, pch=24, col="orange", bg="gold", cex=1.5)
}

N <- 120
draws <- 100000
xbars <- vector(length=draws) #一個什麼東西都沒有，長度等於100000的向量
for(i in 1:draws){
  xbars[i] <- mean(sample(x=POP, size=N, replace=TRUE))
}
xbars  
hist(x=xbars, breaks=100, col=rainbow(10))

N <- 30
draws <- 100000
xbars <- vector(length=draws) #一個什麼東西都沒有，長度等於100000的向量
for(i in 1:draws){
  xbars[i] <- mean(sample(x=POP, size=N, replace=TRUE))
}
xbars  
hist(x=xbars, breaks=100, col=rainbow(10))

N <- 120
draws <- 100000
#抽120次，重複十萬次，跟直接一次抽1200萬次，速度不一樣
xbars <- rowMeans(matrix(data=sample(x=POP, size=N*draws, replace=TRUE), 
                ncol=N, nrow=draws)) 
hist(x=xbars, breaks=100)
#抽120 * 100000次，然後每120個分成一列(一個個案)，計算每個row的平均數，放進xbars裡面

#分割子母畫面
par(mfrow=c(2,2))

N <- 30
draws <- 100000
xbars <- rowMeans(matrix(data=sample(x=POP, size=N*draws, replace=TRUE), 
                              ncol=N, nrow=draws)) 
hist(x=xbars, main=bquote( ~italic(N) == .(N)), 
     xlab="", breaks=200, border="SpringGreen", xlim=c(20,45), freq=FALSE)

N <- 120
draws <- 100000
xbars <- rowMeans(matrix(data=sample(x=POP, size=N*draws, replace=TRUE), 
                         ncol=N, nrow=draws)) 
hist(x=xbars, main=bquote( ~italic(N) == .(N)), 
     xlab="", breaks=200, border="SpringGreen", xlim=c(20,45), freq=FALSE)

N <- 360
draws <- 100000
xbars <- rowMeans(matrix(data=sample(x=POP, size=N*draws, replace=TRUE), 
                         ncol=N, nrow=draws)) 
hist(x=xbars, main=bquote( ~italic(N) == .(N)), 
     xlab="", breaks=200, border="SpringGreen", xlim=c(20,45), freq=FALSE)

N <- 1200
draws <- 100000
xbars <- rowMeans(matrix(data=sample(x=POP, size=N*draws, replace=TRUE), 
                         ncol=N, nrow=draws)) 
hist(x=xbars, main=bquote( ~italic(N) == .(N)), 
     xlab="", breaks=200, border="SpringGreen", xlim=c(20,45), freq=FALSE)

hist(POP, breaks=min(POP):max(POP), border="forestgreen", 
     main="Population Distribution of Age", right=FALSE)
mu <- mean(POP)
sigma <- sd(POP)



N <- 300
draws <- 1000000
xbars <- rowMeans(matrix(data=sample(x=POP, size=N*draws, replace=TRUE), 
                         ncol=N, nrow=draws)) 
hist(x=xbars, main=bquote( "sampling distribution of " ~bar(italic(x))), 
     xlab="", breaks=200, border="DarkOrchid", freq=FALSE)

abline(v=mu, lty=1, lwd=4, col="forestgreen")
abline(v=mean(xbars), lty=2, lwd=3, col="blue")

SD.results <- data.frame(mu_xbar=mean(xbars), sigma_xbar=sd(xbars), 
                         mu=mu, sigma=sigma, N=N)
SD.results

sigma / sqrt(N) #樣本平均數的標準差為母體標準差除以根號 N

作業1
以A060變項
抽N=20的樣本
裡面有幾個人是「婚姻解組」(離婚 or 喪偶)
重複一百萬次
然後做出機率分布
即婚姻解組人數是1的機率，2的機率，3的機率......

作業2
用一樣的條件
計算理論機率分布
用到binomial機率公式
p <- mean(Census.2000$A060 %in% c(3,4))
p = 0.06518608
階層(!) 的打法：factorial(n)
最後把作業1跟作業2整合成一個table