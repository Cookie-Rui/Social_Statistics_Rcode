setwd("C:/Users/user/Downloads/midterm exam/Census 2000-20201007")
load(file="Census.2000.RData")

POP <- Census.2000$A060
P <- sum(POP %in% c(3,4)) / length(POP)
N <- 20
binomial <- dbinom(x=0:N, size=N, prob=P)
binomialdistribution <- data.frame(x=0:N, y=binomial)
binomialdistribution

N <- 20
draws <- 1000000
asample <- sample(x=POP, size=N * draws, replace=TRUE)
sampleboolean <- as.numeric(asample %in% c(3,4))
freqvector <- rowSums(matrix(data=sampleboolean, ncol=N, nrow=draws)) 
freq <- table(freqvector)
length(freq) <- 21
prob <- freq / (1000000)
result <- data.frame(freq=0:20,binomialprob=binomial, freqency=freq,sampleprob=prob)
result
