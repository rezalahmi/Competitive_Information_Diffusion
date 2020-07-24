calculate_penalty<-function(net){
  avgDegree<-mean(degree(net))
  for(i in V(net)[type==0]){
    count_AgreeNode <- length(neighbors(net,i)[color==V(net)[i]$color])
    if((count_AgreeNode + 1)/ length(neighbors(net,i))>= 1/avgDegree)
      V(net)[i]$penalty<-0
    else
      V(net)[i]$penalty<-1
  }
  return(net)
}