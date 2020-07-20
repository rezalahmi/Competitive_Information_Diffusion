mean_probability<-function(net){
  for(i in V(net)[type==0]){
    V(net)[i]$mean_probability[1]<-mean(neighbors(net,i)$action_prob_red)
    V(net)[i]$mean_probability[2]<-mean(neighbors(net,i)$action_prob_blue)
    V(net)[i]$mean_probability[3]<-mean(neighbors(net,i)$action_prob_white)
  }
  return(net)
}
##############################################
mean_penalty<-function(){
  for(i in V(net)[type==0]){
    N<-length(neighbors(net,i))
    for(x in 0:N/2){
      V(net)[i]$mean_penalty[1]<-V(net)[i]$mean_penalty[1]+choose(N,x)*V(net)[i]$mean_probability[1]^x*(1-V(net)[i]$mean_probability[1])^(N-x)
    }
  }
  return(net)
}