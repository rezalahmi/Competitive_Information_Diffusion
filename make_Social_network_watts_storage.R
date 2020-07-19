make.random.social.network<-function(node){
  library(igraph)
  set.seed(1000*runif(1))
  net<-watts.strogatz.game(3,node,1,0.5)
  net<-simplify(net)
  #####################################################
  #properties of node
  #we had two social institute in social network, Red and Blue
  #by default all node are uninformed and we showed this with white color
  V(net)$color<-rep("white",node)
  #the node select action in each time step based on its 
  #action probability vector
  V(net)$action_prob_red<-0
  V(net)$action_prob_white<-1
  V(net)$action_prob_blue<-0
  #after adopt its action, node receives penalty from its environment
  V(net)$penalty<-0
  #we had two type of node, seed and uninformed nodes
  V(net)$type<-0
  ######################################################################
  #for report the condition of diffusion we define some properties for node
  #penalty number received by the node for each action
  V(net)$count_penalty_red_action<-0
  V(net)$count_penalty_white_action<-0
  V(net)$count_penalty_blue_action<-0
  V(net)$average_penalty_recieved_by_red<-0
  V(net)$average_penalty_recieved_by_blue<-0
  V(net)$average_penalty_recieved_by_white<-0


  return(net)
}#make social network
########################################

