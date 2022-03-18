作業

n=120 的簡單隨機可替代樣本
信賴區間為95%
估計信賴區間
然後檢查母體平均數是否有落在區間內
進行十萬次
看看對幾次，錯幾次

把信賴區間改成99%，再做一次

setwd("C:/Users/user/Downloads/midterm exam/Census 2000-20201007")
load(file="Census.2000.RData")
POP <- Census.2000$A020
mu <- mean(POP)
N <- 120
alpha <- 0.05
z <- qnorm(1-alpha/2)

result<- vector(length = 100000)

for(i in 1:100000){
  asample <- sample(x=POP, size=N, replace=TRUE)
  mean <- mean(asample)
  upper <- mean + z * sd(asample) / sqrt(N)
  lower <- mean - z * sd(asample) / sqrt(N)
  result[i] <- (mu <= upper & mu >=lower)
}

table(result)

alpha <- 0.01
z <- qnorm(1-alpha/2)

result<- vector(length = 100000)

for(i in 1:100000){
  asample <- sample(x=POP, size=N, replace=TRUE)
  mean <- mean(asample)
  upper <- mean + z * sd(asample) / sqrt(N)
  lower <- mean - z * sd(asample) / sqrt(N)
  result[i] <- (mu <= upper & mu >=lower)
}

table(result)


