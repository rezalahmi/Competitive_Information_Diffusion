calculate_penalty<-function(net){
  avgDegree<-mean(degree(net))
  for(i in V(net)[type==0 & color=="white"]){
    count_disagree<-length(neighbors(net,1)[color!="white"])
    if(count_disagree>=1)
      V(net)[i]$penalty<-1
    else
      V(net)[i]$penalty<-0
  }
  for(i in V(net)[type==0 & color!="white"]){
    count_agree<-length(neighbors(net,i)[color==V(net)[i]$color])
    count_dissagree<-length(neighbors(net,i)[color!="white" & color!=V(net)[i]$color])
    if(count_dissagree/(count_agree+count_dissagree)>0.5)
      V(net)[i]$penalty<-1
    else
      V(net)[i]$penalty<-0
  }
  return(net)
}