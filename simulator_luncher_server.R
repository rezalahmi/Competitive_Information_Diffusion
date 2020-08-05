#################model#############################
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
  avgDegree<-mean(degree(chain))
  for(i in V(chain)[type==0]){
    Nl<-length(neighbors(chain,i)[color!="white"])
    N<-length(neighbors(chain,i))
    for(x in 0:floor(Nl/2)){
      V(chain)[i]$mean_penalty_red<-V(chain)[i]$mean_penalty_red+choose(N,x)*(1-V(chain)[i]$mean_probability_red)^x*V(chain)[i]$mean_probability_red^(N-x)
      V(chain)[i]$mean_penalty_blue<-V(chain)[i]$mean_penalty_blue+choose(N,x)*(1-V(chain)[i]$mean_probability_blue)^x*V(chain)[i]$mean_probability_blue^(N-x)
      V(chain)[i]$mean_penalty_white<-V(chain)[i]$mean_penalty_white+choose(N,x)*(1-V(chain)[i]$mean_probability_white)^x*V(chain)[i]$mean_probability_white^(N-x)
    }
  }
  return(chain)
}
################################################
markov_chain<-function(chain){
  for(i in V(chain)[type==0]){
    c1<-V(chain)[i]$mean_penalty_red
    c2<-V(chain)[i]$mean_penalty_blue
    c3<-V(chain)[i]$mean_penalty_white
    if(c1==0){V(chain)[i]$action_prob_red<-1}
    else{V(chain)[i]$action_prob_red<-(1/c1)/(1/c1+1/c2+1/c3)}
    if(c2==0){V(chain)[i]$action_prob_blue<-1}
    else{V(chain)[i]$action_prob_blue<-(1/c2)/(1/c1+1/c2+1/c3)}
    if(c3==0){V(chain)[i]$action_prob_white<-1}
    else{V(chain)[i]$action_prob_white<-(1/c3)/(1/c1+1/c2+1/c3)}
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
lunch_model<-function(chain,number_of_repeat){
  for(i in 1:number_of_repeat){
    chain<-mean_probability(chain)
    chain<-mean_penalty(chain)
    chain<-markov_chain(chain)
    chain<-choose_color(chain)
    chain<-rewiring(chain)
  }
  report_markov_chain(chain)
}
####################rewiring#########################################
find_oldFriend_based_fermi<-function(net,i){
  ##first I chose the neighbors of Node i
  list_of_neighbors_i<-neighbors(net,i)
  #then find list of disagree node in neighbors of Node i
  dissagree_neighbors_i<-V(net)[color!=V(net)[i]$color & V(net) %in% list_of_neighbors_i]
  #return the disagree node with smallest payoff(degree)
  if(length(dissagree_neighbors_i)==0)
    return(NULL)
  return(as_ids(list_of_neighbors_i[which.min(degree(net,dissagree_neighbors_i))]))
}
##########################################################################
find_newFriend_based_fermi<-function(net,i){
  ###fermi function=1/(1+exp(-beta(pjj-pj)))
  ###beta = 1
  #assign all node 0, for value of fermi function
  result<-rep(0,vcount(net))
  #find candidate node between neighbors of node i
  pj<-find_oldFriend_based_fermi(net,i)
  if(is.null(pj))
    return(NULL)
  #calculate the fermi value for all node who agree with i or had white color
  agreeNode<-V(net)[color==V(net)[i]$color | color=="white"]
  pjj<-degree(net,agreeNode)
  fermi<-1/(1+exp(-2*(pjj-pj)))
  fermi<-cumsum(fermi)
  fermi<-fermi/fermi[vcount(net)]
  newFriend<-length(fermi[fermi<runif(1)])
  return(newFriend)
}
##################################################################################
make_friendship<-function(net,i){
  old_neighbor<-find_oldFriend_based_fermi(net,i)
  new_neighbor<-find_newFriend_based_fermi(net,i)
  if(is.null(old_neighbor) | is.null(new_neighbor))
    return(net)
  if(new_neighbor %in% neighbors(net,i))
    return(net)
  net<-simplify(net)
  net<-delete.edges(net,E(net)[i%->%old_neighbor])
  net<-net+edge(i,new_neighbor)
  return(net)
}
rewiring<-function(net){
  for(i in V(net))
    net<-make_friendship(net,i)
  return(net)
}
#######################################################
find_oldFriend_based_fermi<-function(net,i){
  ##first I chose the neighbors of Node i
  list_of_neighbors_i<-neighbors(net,i)
  #then find list of disagree node in neighbors of Node i
  dissagree_neighbors_i<-V(net)[color!=V(net)[i]$color & V(net) %in% list_of_neighbors_i]
  #return the disagree node with smallest payoff(degree)
  if(length(dissagree_neighbors_i)==0)
    return(NULL)
  return(as_ids(list_of_neighbors_i[which.min(degree(net,dissagree_neighbors_i))]))
}
#####################################
find_newFriend_based_fermi<-function(net,i){
  ###fermi function=1/(1+exp(-beta(pjj-pj)))
  ###beta = 1
  #assign all node 0, for value of fermi function
  result<-rep(0,vcount(net))
  #find candidate node between neighbors of node i
  pj<-find_oldFriend_based_fermi(net,i)
  if(is.null(pj))
    return(NULL)
  #calculate the fermi value for all node who agree with i or had white color
  agreeNode<-V(net)[color==V(net)[i]$color | color=="white"]
  pjj<-degree(net,agreeNode)
  fermi<-1/(1+exp(-2*(pjj-pj)))
  fermi<-cumsum(fermi)
  fermi<-fermi/fermi[vcount(net)]
  newFriend<-length(fermi[fermi<runif(1)])
  return(newFriend)
}
#########################################
make_friendship<-function(net,i){
  old_neighbor<-find_oldFriend_based_fermi(net,i)
  new_neighbor<-find_newFriend_based_fermi(net,i)
  if(is.null(old_neighbor) | is.null(new_neighbor))
    return(net)
  if(new_neighbor %in% neighbors(net,i))
    return(net)
  net<-simplify(net)
  net<-delete.edges(net,E(net)[i%->%old_neighbor])
  net<-net+edge(i,new_neighbor)
  return(net)
}
##########################################
rewiring<-function(net){
  for(i in V(net))
    net<-make_friendship(net,i)
  return(net)
}
############################################################
simulator_luncher<-function(number_of_node,number_of_seed,number_of_repeat,alpha,beta){
  net<-make.random.social.network(number_of_node)
  net<-set.diffuser(net,number_of_seed)
  ########################model###########################
  chain<-net
  lunch_model(chain,number_of_repeat)
  #################social network#########################
  net<-set_Action_prob(net)
  for(i in 1:number_of_repeat){
    net<-choose_action(net)
    net<-calculate_penalty(net)
    net<-action_prob_calculator(net,alpha,beta)
    net<-rewiring(net)
  }
  report_diffusion_condition(net,number_of_repeat)
}
#############################################################
report_diffusion_condition<-function(net,number_of_repeat){
  #this function create data frame and export that into csv file
  report<- data.frame(
    node_id = 1:vcount(net),
    action_prob_red = V(net)$action_prob_red,
    action_prob_blue = V(net)$action_prob_blue,
    action_prob_white = V(net)$action_prob_white,
    color = V(net)$color,
    count_penalty_red_action = V(net)$count_penalty_red_action,
    count_penalty_white_action = V(net)$count_penalty_white_action,
    count_penalty_blue_action = V(net)$count_penalty_blue_action,
    average_penalty_recieved_by_red = V(net)$count_penalty_red_action/number_of_repeat,
    average_penalty_recieved_by_white = V(net)$count_penalty_white_action/number_of_repeat,
    average_penalty_recieved_by_blue = V(net)$count_penalty_blue_action/number_of_repeat
  )
  write.csv(report,"C:\\report\\diffusion_report.csv",row.names = FALSE)
}
############################################################
action_prob_calculator<-function(net,a,b){
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
        V(net)[i]$count_penalty_red_action<-V(net)[i]$count_penalty_red_action + 1
        p1<-(1-b)*p1
        p2<-b/2+(1-b)*p2
        p3<-b/2+(1-b)*p3
      }
      if(V(net)[i]$color=="blue"){
        V(net)[i]$count_penalty_blue_action<-V(net)[i]$count_penalty_blue_action + 1
        p2<-(1-b)*p2
        p1<-b/2+(1-b)*p1
        p3<-b/2+(1-b)*p3
      }
      if(V(net)[i]$color=="white"){
        V(net)[i]$count_penalty_white_action<-V(net)[i]$count_penalty_white_action + 1
        p3<-(1-b)*p3
        p1<-b/2+(1-b)*p1
        p2<-b/2+(1-b)*p2
      }
    }##end of else
    V(net)[i]$action_prob_red<-p1
    V(net)[i]$action_prob_blue<-p2
    V(net)[i]$action_prob_white<-p3
  }
  return(net)
}
################################################################
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
    if(is.nan(count_agree))
      count_agree<-0
    count_dissagree<-length(neighbors(net,i)[color!="white" & color!=V(net)[i]$color])
    if(is.nan(count_dissagree))
      count_dissagree<-0
    ratio<-count_dissagree/(count_agree+count_dissagree)
    if(is.nan(ratio))
      ratio<-0
    if(ratio>0.5){V(net)[i]$penalty<-1}
    else{V(net)[i]$penalty<-0}
  }
  return(net)
}
#############################################################
choose_action<-function(net){
  for(i in V(net)[type==0]){
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
#############################################################
set_Action_prob<-function(net){
  #this function initialize the action probability vector of node
  for(i in V(net)[type==0]){
    red_count<-0
    blue_count<-0
    white_count<-0      
    number_of_neighbour_i<-length(neighbors(net,i))
    red_count <- length(neighbors(net,i)[color=="red"])
    blue_count <- length(neighbors(net,i)[color=="blue"])
    white_count <- length(neighbors(net,i)[color=="white"])
    V(net)[i]$action_prob_red<-red_count/number_of_neighbour_i
    V(net)[i]$action_prob_white<-white_count/number_of_neighbour_i
    V(net)[i]$action_prob_blue<-blue_count/number_of_neighbour_i
    #in some read data set we had isolated node
    if(number_of_neighbour_i==0){
      V(net)[i]$action_prob_red<-0
      V(net)[i]$action_prob_blue<-0
      V(net)[i]$action_prob_white<-1
    }#end of if
  }
  return(net)
}
#######################################################
set.diffuser<-function(net,seed.number){
  set.seed(1000*runif(1))
  seed.node<-sample(V(net),seed.number*2)
  red.seed<-seed.node[1:seed.number]
  l<-seed.number+1
  u<-seed.number*2
  blue.seed<-seed.node[l:u]
  V(net)[red.seed]$color<-"red"
  V(net)[red.seed]$action_prob_red<-1
  V(net)[red.seed]$action_prob_white<-0
  V(net)[red.seed]$action_prob_blue<-0
  V(net)[red.seed]$type<-1
  
  V(net)[blue.seed]$color<-"blue"
  V(net)[blue.seed]$action_prob_blue<-1
  V(net)[blue.seed]$action_prob_white<-0
  V(net)[blue.seed]$action_prob_red<-0
  V(net)[blue.seed]$type<-1
  return(net)
}
#########################################################
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
  #####################################################################
  V(net)$mean_probability_red<-0
  V(net)$mean_probability_blue<-0
  V(net)$mean_probability_white<-0
  
  V(net)$mean_penalty_red<-0
  V(net)$mean_penalty_blue<-0
  V(net)$mean_penalty_white<-0
  #########################################################
  return(net)
}#make social network
########################################

