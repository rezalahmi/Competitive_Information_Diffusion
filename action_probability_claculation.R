action_prob_calculator<-function(net,a){
  for(i in V(net)[type==0]){
    p1<-V(net)[i]$action_prob_red
    p2<-V(net)[i]$action_prob_blue
    p3<-V(net)[i]$action_prob_white
    if(V(net)[i]$penalty==0){
      if(V(net)[i]$color=="red"){
        p1<-p1+a*(1-p1)
        p2<-(1-a)*p2
        p3<-(1-a)*p3
      }
      if(V(net)[i]$color=="white"){
        p3<-p3+(1-p3)*a
        p1<-(1-a)*p1
        p2<-(1-a)*p2
      }
      if(V(net)[i]$color=="blue"){
        p2<-p2+(1-p2)*a
        p1<-(1-a)*p1
        p3<-(1-a)*p3
      }
    }##end of if
    else{
      if(V(net)[i]$color=="red"){
        V(net)$count_penalty_red_action<-V(net)$count_penalty_red_action + 1
        p1<-(1-a)*p1
        p2<-a/2+(1-a)*p2
        p3<-a/2+(1-a)*p3
      }
      if(V(net)[i]$color=="blue"){
        V(net)$count_penalty_blue_action<-V(net)$count_penalty_blue_action + 1
        p2<-(1-a)*p2
        p1<-a/2+(1-a)*p1
        p3<-a/2+(1-a)*p3
      }
      if(V(net)[i]$color=="white"){
        V(net)$count_penalty_white_action<-V(net)$count_penalty_white_action + 1
        p3<-(1-a)*p3
        p1<-a/2+(1-a)*p1
        p2<-a/2+(1-a)*p2
      }
    }##end of else
    V(net)[i]$action_prob_red<-p1
    V(net)[i]$action_prob_blue<-p2
    V(net)[i]$action_prob_white<-p3
  }
  return(net)
}