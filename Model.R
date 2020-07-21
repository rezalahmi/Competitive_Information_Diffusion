mean_probability<-function(chain){
  for(i in V(chain)[type==0]){
    V(chain)[i]$mean_probability_red<-mean(neighbors(chain,i)$action_prob_red)
    V(chain)[i]$mean_probability_blue<-mean(neighbors(chain,i)$action_prob_blue)
    V(chain)[i]$mean_probability_white<-mean(neighbors(chain,i)$action_prob_white)
  }
  return(chain)
}
##############################################
mean_penalty<-function(chain){
  for(i in V(chain)[type==0]){
    N<-length(neighbors(chain,i))
      for(x in 0:floor(N/2)){
       V(chain)[i]$mean_penalty_red<-V(chain)[i]$mean_penalty_red+choose(N,x)*V(chain)[i]$mean_probability_red^x*(1-V(chain)[i]$mean_probability_red)^(N-x)
       V(chain)[i]$mean_penalty_blue<-V(chain)[i]$mean_penalty_blue+choose(N,x)*V(chain)[i]$mean_probability_blue^x*(1-V(chain)[i]$mean_probability_blue)^(N-x)
       V(chain)[i]$mean_penalty_white<-V(chain)[i]$mean_penalty_white+choose(N,x)*V(chain)[i]$mean_probability_white^x*(1-V(chain)[i]$mean_probability_white)^(N-x)
      }
  }
  return(chain)
}
################################################
markov_chain<-function(chain,alpha){
  for(i in V(chain)[type==0]){
    c1<-V(chain)[i]$mean_penalty_red
    c2<-V(chain)[i]$mean_penalty_blue
    c3<-V(chain)[i]$mean_penalty_white
    V(chain)[i]$action_prob_red<-(1/c1)/(1/c1+1/c2+1/c3)
    V(chain)[i]$action_prob_blue<-(1/c2)/(1/c1+1/c2+1/c3)
    V(chain)[i]$action_prob_white<-(1/c3)/(1/c1+1/c2+1/c3)
  }
  
  return(chain)
}
#################################################
choose_color<-function(chain){
  for(i in V(chain)[type==0]){
    random_number<-sample(10000,1)/10000
    p1<-V(chain)[i]$action_prob_red
    p2<-V(chain)[i]$action_prob_blue
    if(is.nan(p1))p1<-0
    if(is.nan(p2))p2<-0
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
    action_prob_red=V(chain)$action_prob_red,
    action_prob_blue=V(chain)$action_prob_blue,
    action_prob_white=V(chain)$action_prob_white,
    mean_probability_red=V(chain)$mean_probability_red,
    mean_probability_blue=V(chain)$mean_probability_blue,
    mean_probability_white=V(chain)$mean_probability_white,
    mean_penalty_red=V(chain)$mean_penalty_red,
    mean_penalty_blue=V(chain)$mean_penalty_blue,
    mean_penalty_white=V(chain)$mean_penalty_white
  )
  write.csv(report,"C:\\report\\model_report.csv",row.names = FALSE)
}
##################################################
lunch_model<-function(chain,number_of_repeat,alpha){
  for(i in 1:number_of_repeat){
    chain<-mean_probability(chain)
    chain<-mean_penalty(chain)
    chain<-markov_chain(chain,alpha)
    chain<-choose_color(chain)
    chain<-rewiring(chain)
  }
  report_markov_chain(chain)
}
