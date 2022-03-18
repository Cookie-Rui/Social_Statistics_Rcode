id <- 1:30
exam1 <- sample(x=40:90, size=30, replace = T)
exam2 <- sample(x=40:90, size=30, replace = T)
exam3 <- sample(x=40:90, size=30, replace = T)
exam4 <- sample(x=40:90, size=30, replace = T)

mean <- c()
for(i in 1:30){
  mean[i] <- (exam1[i]+exam2[i]+exam3[i]+exam4[i])/4
}

MEAN <- sum(mean)/30
MAX <- max(mean)
MIN <- min(mean)
SD <- sd(mean)
cat(MEAN,MAX,MIN,SD)

pass <- c()
for(i in 1:30){
  ifelse(mean[i]>=60, pass[i] <- "及格", pass[i] <- "不及格")
}

Score <- data.frame(座號=id, 考試1=exam1,考試2=exam2,考試3=exam3,考試4=exam4,
                      平均成績=mean, 及格與否=pass)
