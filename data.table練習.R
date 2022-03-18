library(data.table)
set.seed(45L)
DT <- data.table(V1=c(1L,2L),
                 V2=LETTERS[1:3],
                 V3=round(rnorm(4),4),
                 V4=1:12)
DT

DT[3:5,]
DT[ V2 == "A"]
DT[ V2 %in% c("A","C")]
DT[ ! V2 %in% c("A","C")]

DT[,V2]
DT[,.(V2,V3)]
DT[,sum(V1)]
DT[,.(sum(V1),sd(V3))]
DT[,.(Aggregate = sum(V1), Sd.V3 = sd(V3))]
DT[,.(V1, Sd.V3 = sd(V3))]
DT[,{print(V2)
  plot(V3)
  NULL}]

DT[,.(V4.Sum = sum(V4)),by=V1]
DT[,.(V4.Sum = sum(V4)),by=.(V1,V2)]
DT[,.(V4.Sum = sum(V4)),by=sign(V1-1)]
DT[,.(V4.Sum = sum(V4)),
   by=.(V1.01 = sign(V1-1))]
DT[1:5,.(V4.Sum = sum(V4)),by=V1]
DT[,.N,by=V1]

DT

DT[, V1 := round(exp(V1),2)]
DT[, c("V1","V2") := list(round(exp(V1),2), "D")]
DT[, ":=" (V1 =round(exp(V1),2),
           V2 = "D")][]
DT[, V1 := NULL]
DT[, c("V1","V2") := NULL]

Cols.chosen = c("A","B")
DT[, Cols.chosen := NULL]

DT[, (Cols.chosen) := NULL]