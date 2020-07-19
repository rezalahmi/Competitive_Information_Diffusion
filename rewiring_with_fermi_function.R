#########################################
find_oldFriend_based_fermi<-function(net,i){
  ##first I chose the neighbors of Node i
  list_of_neighbors_i<-neighbors(net,i)
  #then find list of disagree node in neighbors of Node i
  dissagree_neighbors_i<-V(net)[color!=V(net)[i]$color & V(net) %in% list_of_neighbors_i]
  #return the disagree node with smallest payoff(degree)
  if(length(dissagree_neighbors_i)==0)
    return(NULL)
  return(list_of_neighbors_i[which.min(degree(net,dissagree_neighbors_i))])
}
#####################################

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
  fermi<-1/(1+exp(-1*(pjj-pj)))
  fermi<-cumsum(fermi)
  fermi<-fermi/fermi[vcount(net)]
  newFriend<-length(fermi[fermi<runif(1)])
  return(newFriend)
}
#########################################

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
  for(i in V(net)[type==0])
    net<-make_friendship(net,i)
  return(net)
}