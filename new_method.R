################################################################################
#FUNCTIONS
###############################################################################
get_rating <- function(subject, ratings, schedule_flex){
  subject_rating <- ratings %>% filter(team == subject) %>% pull(power_margin_rating)
  subject_schedule <- 
    schedule_flex %>% filter(winner == subject | loser == subject) %>% 
    mutate(subject = subject
           , subject_rating = subject_rating
           , opponent = ifelse(winner == subject, loser, winner)
           , opponent_rating = 
             ifelse(winner_rating == subject_rating, loser_rating, winner_rating)
           , true_margin = ifelse(winner == subject, true_margin, -true_margin)) %>% 
    select(subject, subject_rating, opponent, opponent_rating, true_margin, weight)
  subject_rating_grid <- seq(from = round(subject_rating,2) - 15
                             , to = round(subject_rating,2) + 15, by = 0.005)
  
  subject_new <- 
    expand_grid(subject_schedule
                , exp_rating = seq(from = round(subject_rating,2) - 15
                                   , to = round(subject_rating,2) + 15
                                   , by = 0.01)) %>% 
    mutate(proj_margin = exp_rating - opponent_rating
           , error = (true_margin - proj_margin)^2*weight) %>% 
    group_by(exp_rating) %>% summarise(error = sum(error), .groups = "drop") %>% 
    slice_min(error) %>% 
    pull(exp_rating)
  
  return(subject_new)
}


get_rating_graph <- function(subject, ratings, schedule_flex){
  subject_rating <- ratings %>% filter(team == subject) %>% pull(power_margin_rating)
  subject_schedule <- 
    schedule_flex %>% filter(winner == subject | loser == subject) %>% 
    mutate(subject = subject
           , subject_rating = subject_rating
           , opponent = ifelse(winner == subject, loser, winner)
           , opponent_rating = 
             ifelse(winner_rating == subject_rating, loser_rating, winner_rating)
           , true_margin = ifelse(winner == subject, true_margin, -true_margin)) %>% 
    select(subject, subject_rating, opponent, opponent_rating, true_margin, weight)
  
  subject_grid <- 
    expand_grid(subject_schedule
                , exp_rating = seq(from = round(subject_rating,2) - 15
                                   , to = round(subject_rating,2) + 15
                                   , by = 0.01))
  
  subject_gg <- subject_grid %>% mutate(proj_margin = exp_rating - opponent_rating
                                        , error = (true_margin - proj_margin)^2*weight
                                        , opponent = factor(opponent
                                                            , levels = rev(unique(subject_grid$opponent))))
  
  subject_gg %>% ggplot(aes(x = exp_rating, y = error, fill = opponent)) + 
    geom_bar(stat = "identity") + geom_area(position = 'stack') +
    scale_x_continuous(breaks = seq(from = round(subject_rating,0) - 15
                                    , to = round(subject_rating,0) + 15
                                    , by = 2)) + ggtitle(label = subject) + 
    theme_ipsum_es() + scale_fill_brewer(palette = "Paired")
}

################################################################################

ratings <- readRDS("ratings.rds")
schedule_flex <- readRDS("schedule_flex.rds")

new_method <- 
  ratings %>% 
  mutate(new_rating = sapply(team, get_rating, ratings, schedule_flex)) %>% 
  arrange(desc(new_rating))

get_rating_graph("Ohio State", ratings, schedule_flex)
