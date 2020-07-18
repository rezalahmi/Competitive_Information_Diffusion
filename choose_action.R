choose_action<-function(net){
  for(i in V(net)){
    random_number<-sample(10000,1)/10000
    p1<-V(net)[i]$action_prob_red
    p2<-V(net)[i]$action_prob_blue
    if(random_number <= p1){
      V(net)[i]$color<-"red"
    }
    else if((random_number>p1) & (random_number<=p2+p1)){
      V(net)[i]$color<-"blue"
    }
    else
      V(net)[i]$color<-"white"
    }
  return(net)
}
  