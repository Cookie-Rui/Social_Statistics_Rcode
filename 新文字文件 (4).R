setwd("C:/Users/user/Downloads/Census 2000-20201007")
library(dplyr)
load(file="Census.2000.RData")
str(Census.2000)
table(Census.2000$A020)
freq <- count(x=Census.2000, A020)
summary(Census.2000$A020)
mean(Census.2000$A020)
range(Census.2000$A020)
var(Census.2000$A020) #變異量和標準差在r裡面的分母是n - 1, 要求分母為n的話要乘上(n-1)/n
sd(Census.2000$A020)
quantile(Census.2000$A020, probs=c(0.25, 0.975))
N <- nrow(Census.2000)
hist(x=Census.2000$A020, breaks=0:120, right=FALSE, , col="skyblue", border="salmon")

# population distribution

popdist <- count(x=Census.2000, A020)
popdist <- mutate(.data=popdist, prop=n/sum(n))

asample <- sample_n(tbl = Census.2000, size=1,replace=TRUE)
asample$A020

POP <- select(.data=Census.2000, A020)
samples <- count(x=sample_n(tbl=POP, size=1000000, replace = TRUE), A020)
samples <- mutate(.data=samples, prob=n/sum(n))
manysamples <- data.frame()
for(i in 1:100){
  manysamples <- rbind(manysamples, 
                       count(x=sample_n(tbl=POP, size=1000000, replace = TRUE), A020))
}

result <- count(x=manysamples, A020, wt=n)
result <- mutate(.data=result, prob=n/sum(n))

aSample <- sample_n(tbl=Census.2000, size=5, replace=TRUE)
aSample$A010
