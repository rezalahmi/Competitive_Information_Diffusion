report_diffusion_condition<-function(net,number_of_repeat){
  #this function create data frame and export that into csv file
  report<- data.frame(
    node_id = 1:length(V(net)),
    action_prob_red = V(net)$action_prob_red,
    action_prob_blue = V(net)$action_prob_blue,
    action_prob_white = V(net)$action_prob_white,
    color = V(net)$color,
    count_penalty_red_action = V(net)$count_penalty_red_action,
    count_penalty_white_action = V(net)$count_penalty_white_action,
    count_penalty_blue_action = V(net)$count_penalty_blue_action,
    average_penalty_recieved_by_red = V(net)$average_penalty_red_action/number_of_repeat,
    average_penalty_recieved_by_white = V(net)$average_penalty_white_action/number_of_repeat,
    average_penalty_recieved_by_blue = V(net)$average_penalty_blue_action/number_of_repeat
  )
  write.csv(report,"C:\\report\\diffusion_report.csv",row.names = FALSE)
}