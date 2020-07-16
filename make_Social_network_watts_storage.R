make.random.social.network<-function(node){
  library(igraph)
  set.seed(1000*runif(1))
  net<-watts.strogatz.game(3,node,1,0.5)
  net<-simplify(net)
  V(net)$color<-rep("white",node)
  V(net)$action_prob_red<-0
  V(net)$action_prob_white<-1
  V(net)$action_prob_blue<-0
  V(net)$profit<-0
  V(net)$penalty<-0
  V(net)$type<-0
  return(net)
}#make social network
########################################

