calculate_penalty<-function(net){
  avgDegree<-mean(degree(net))
  for(i in V(net)[type==0]){
    count_AgreeNode <- length(neighbors(net,i)[color==V(net)[i]$color])
    count_disAgreeNode <- length(neighbors(net,i)[color!=V(net)[i]$color])
    if(V(net)[i]$color=="white" & count_disAgreeNode>=1)
      V(net)[i]$penalty <- 1
    else if (V(net)[i]$color=="white" & count_disAgreeNode<1)
      V(net)[i]$penalty <- 0
    else if (V(net)[i]$color!="white" & count_AgreeNode>=(1/avgDegree))
      V(net)[i]$penalty <- 0
    else V(net)[i]$penalty <- 1
  }
  return(net)
}