mean_probability<-function(chain){
  for(i in V(chain)[type==0]){
    V(chain)[i]$mean_probability<-mean(neighbors(chain,i)$action_prob)
  }
  return(chain)
}
##############################################
mean_penalty<-function(chain){
  for(i in V(chain)[type==0]){
    N<-length(neighbors(chain,i))
    for(x in 0:N/2){
      V(chain)[i]$mean_penalty<-V(chain)[i]$mean_penalty+choose(N,x)*V(chain)[i]$mean_probability^x*(1-V(chain)[i]$mean_probability)^(N-x)
    }
  }
  return(chain)
}
################################################
markov_chain<-function(chain,alpha){
  for(i in V(chain)[type==0]){
    P<-V(net)$action_prob
    a11<-1-alpha*V(chain)[i]mean_penalty[1]
    a12<-alpha * V(chain)[i]mean_penalty[2]/2
    a13<-alpha * V(chain)[i]mean_penalty[3]/2
    a1<-c(a11,a12,a13)
    a21<-alpha * V(chain)[i]mean_penalty[1]/2
    a22<-1-alpha*V(chain)[i]mean_penalty[2]
    a23<-alpha * V(chain)[i]mean_penalty[3]/2
    a2<-c(a21,a22,a23)
    a31<-alpha * V(chain)[i]mean_penalty[1]/2
    a32<-alpha * V(chain)[i]mean_penalty[2]/2
    a33<-1-alpha*V(chain)[i]mean_penalty[3]
    a3<-c(a31,a32,a33)
    A<-cbind(a1,a2,a3)
    P<-A * P
    V(chain)[i]$action_prob<-P
  }
  return(chain)
}
#################################################
choose_color<-function(chain){
  for(i in V(chain)[type==0]){
    random_number<-sample(10000,1)/10000
    p1<-V(chain)[i]$action_prob[1]
    p2<-V(chain)[i]$action_prob[2]
    if(random_number <= p1){
      V(chain)[i]$color<-"red"
    }
    else if((random_number>p1) & (random_number<=p2+p1)){
      V(chain)[i]$color<-"blue"
    }
    else
      V(chain)[i]$color<-"white"
  }
  return(chain)
}
#################################################
report_markov_chain<-function(chain){
  report<-data.frame(
    nodeId = 1:vcount(chain),
    action_prob_red=V(chain)$action_prob[1],
    action_prob_blue=V(chain)$action_prob[2],
    action_prob_white=V(chain)$action_prob[3],
    mean_probability_red=V(chain)$mean_probability[1],
    mean_probability_blue=V(chain)$mean_probability[2],
    mean_probability_white=V(chain)$mean_probability[3],
    mean_penalty_red=V(chain)$mean_penalty[1],
    mean_penalty_blue=V(chain)$mean_penalty[2],
    mean_penalty_white=V(chain)$mean_penalty[3]
  )
  write.csv(report,"C:\\report\\model_report.csv",row.names = FALSE)
}
##################################################
lunch_model<-function(chain,number_of_repeat,alpha){
  for(i in V(net)){
    V(chain)[i]$action_prob[1]<-V(chain)[i]$action_prob_red
    V(chain)[i]$action_prob[2]<-V(chain)[i]$action_prob_blue
    V(chain)[i]$action_prob[3]<-V(chain)[i]$action_prob_white
  }
  for(i in 1:number_of_repeat){
    chain<-mean_probability(chain)
    chain<-mean_penalty(chain)
    chain<-markov_chain(chain,alpha)
    chain<-choose_color(chain)
    chain<-rewiring(chain)
  }
  report_markov_chain(chain)
}
