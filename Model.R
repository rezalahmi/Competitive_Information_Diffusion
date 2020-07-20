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
      V(net)[i]$mean_penalty<-V(net)[i]$mean_penalty+choose(N,x)*V(net)[i]$mean_probability^x*(1-V(net)[i]$mean_probability)^(N-x)
    }
  }
  return(net)
}