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