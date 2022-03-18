x<-1:49

lottery<-replicate(200, sort(sample(x, size=6, replace=FALSE)))

duplicated(t(lottery))

a<-matrix(c(lottery), nrow=6)

answer<-c(11,15,16,18,24,26,46)

total<-0

for(j in 1:200){
  correct<-0
  special<-0
  for(i in 1:6){
    for(k in 1:7){
      if(a[i,j]==answer[k]){
        correct<-correct+1
        if(k==7) special<-1
      }
    }
  }
  cat("\n中獎號碼個數(包含特別號)：", correct, "\n")
  if(correct==3){
    cat("妳中了400元\n")
    total<-total+400
  }
  if(correct==2 && special==1){
    cat("妳中了400元\n")
    total<-total+400
  }
  if(correct==3 && special==1){
    cat("妳中了1000元\n")
    total<-total+1000
  }
  if(correct==4){
    cat("妳中了2000元\n")
    total<-total+2000
  }
  if(correct==4 && special==1){
    cat("妳中了13987元\n")
    total<-total+13987	
  }
  if(correct==5){
    cat("妳中了52958元\n")
    total<-total+52958
  }
  if(correct==5 && special==1){
    cat("妳中了2606333元\n")
    total<-total+2606333
  }
  if(correct==6){
    cat("妳中了63852137元\n")
    total<-total+63852137
  }
}
cat("總獎金額為", total, "元")
ROE<- total/10000
cat("報酬率:", ROE)
if(ROE>1){
  print("賺錢囉!!!!!!!!!")
}else if(ROE==1){
  print("不賺不賠，幸好")
}else if(ROE<1){
  print("賠錢囉!!!!!!!!!")
}