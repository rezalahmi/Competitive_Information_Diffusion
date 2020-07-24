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