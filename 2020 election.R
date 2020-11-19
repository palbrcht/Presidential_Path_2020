
###
###  Load necessary packages
###

library(tidyverse)
library(readr)

options(scipen=999)

###
###  Clear workign environment
###

rm(list = ls())

###
###  Load 538 Data (Thanks 538!)
###
###  https://github.com/fivethirtyeight/data/tree/master/election-forecasts-2020
###
###  Load electoral college data from flat csv.
###

presidential_state_toplines_2020 <- read_csv("https://projects.fivethirtyeight.com/2020-general-data/presidential_state_toplines_2020.csv")

electoral_votes_data <- read.csv("add_path_to_csv")

###
###  Subset data for monte carlo.
###
###  Used 11/3/2020 to simulate the most recent pre-election polls/probabilities from 538.
###

recent_data <- presidential_state_toplines_2020 %>%
  filter(modeldate == '11/3/2020')  ##note that the data loads as a character class, not date class

simulation_data <- full_join(recent_data, electoral_votes_data, by = 'state' ) %>%
  select(one_of('state' , 'voteshare_inc' , 'voteshare_chal' , 'electoral_votes'))

simulation_data <- na.omit(simulation_data)

###
###  Make a list of states for the loop.
###
###  Randomize the list.
###

list_of_states <- simulation_data$state

list_of_states <- sample(list_of_states)

###
###  Set randomization seed and loop through simulation.
###
###  This is going to create 51 mini simulations (representing each state outcome) and then
###  running however many trials are run set with the trials object.
###

set.seed(7788)

trials <- 100

for (z in 1:trials){
  
  print(z)
  
  completed_iteration <- NULL
  
  list_of_states <- sample(list_of_states)
  
  
  for (temp_state in list_of_states){
    
    temp_data <- simulation_data %>%
      filter(state == temp_state)
    
    trump_chance <- temp_data$voteshare_inc
    
    biden_chance <- temp_data$voteshare_chal
    
    trump_cutoff <- 100 - trump_chance
    
    temp_electoral_votes <- temp_data$electoral_votes
    
    temp_roll <- runif(1, min = 0, max = 100)
    
    temp_data$roll <- temp_roll
    
    temp_data$winner <- ifelse(temp_roll >= trump_cutoff, 'Trump' , 'Biden')
    
    temp_data$trump_addition = ifelse(temp_roll >= trump_cutoff, temp_electoral_votes, 0)
    
    temp_data$biden_addition = ifelse(temp_roll < trump_cutoff, temp_electoral_votes, 0)
    
    if (exists(("completed_iteration"))) {completed_iteration <- rbind(completed_iteration, temp_data)} else {completed_iteration <- temp_data}
    
  }
  
  counting_iteration <- completed_iteration %>%
    mutate(trump_total_votes = cumsum(trump_addition) ,
           biden_total_votes = cumsum(biden_addition)) %>%
    mutate(index_state = row_number() ,
           iteration_number = z) %>%
    mutate(winning_status = ifelse((trump_total_votes >= 270 | biden_total_votes >= 270), 'Yes' , 'No')) %>%
    mutate(iteration_winner = ifelse( (index_state == 51 & biden_total_votes >= 270) , 'Biden' , 'Contested')) %>%
    #mutate(iteration_winner = ifelse(  'Trump' , iteration_winner)) %>%
    mutate(iteration_winner = ifelse('Biden' %in% iteration_winner, 'Biden' , 'Trump')) %>%
    mutate(iteration_winner = ifelse( (index_state == 51 & trump_total_votes < 270 & biden_total_votes < 270), 'Contested' , iteration_winner)) %>%
    mutate(winner_votes = ifelse(iteration_winner == 'Biden' , biden_total_votes , trump_total_votes)) %>%
    mutate(difference_votes = biden_total_votes - trump_total_votes )
  
  if (exists(("consolidated_data"))) {consolidated_data <- rbind(consolidated_data, counting_iteration)} else {consolidated_data <- counting_iteration}
}

###
###  Plots the results.
###
###  Each simulation is its own line.  Simulations favoring Biden go left (to represent the Left) while
###  simulations favoring Trump go right (to represent the Right).
###
###  There are also some outcomes possible where there is an undecided election. Those are green.
###

gg_pathway <- ggplot(consolidated_data, aes(x = index_state, y = difference_votes, color = iteration_winner, group = iteration_number)) +
  geom_point(size = .01, alpha = 0.1) +
  geom_line(alpha = 0.20) +
  theme_void() +
  scale_color_manual(values=c("blue","green" , "dark red")) +
  theme(legend.position = "none") +
  coord_cartesian(
    xlim = c(0, 50),
    ylim = NULL
  ) +
  theme(plot.background = element_rect(fill = "black")) +
  coord_flip() +
  scale_y_reverse()

gg_pathway

###
###  END OF SCRIPT!
###