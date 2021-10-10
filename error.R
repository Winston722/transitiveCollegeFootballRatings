schedule <- readRDS("schedule.rds")
ratings <- readRDS("ratings.rds")

schedule_post <- 
  schedule %>% left_join(ratings, by = c("winner" = "team")) %>% 
  rename(winner_rating = power_margin_rating) %>% 
  left_join(ratings, by = c("loser" = "team")) %>% 
  rename(loser_rating = power_margin_rating) %>% 
  select(winner:loser_pts,true_margin:winner_rating,loser_rating) %>% 
  rename(winner_games = count.x, loser_games = count.y) %>% 
  mutate(
    loser_rating = 
      ifelse(is.na(loser_rating), winner_rating - true_margin, loser_rating)
    , winner_rating = 
      ifelse(is.na(winner_rating), true_margin - loser_rating, winner_rating)
    , winner_error = (winner_rating-(loser_rating + true_margin))^2
    , loser_error = (loser_rating-(winner_rating - true_margin))^2)

winners <- schedule_post %>% select(winner, winner_error, weight) %>% 
  rename(error = winner_error, team = winner)
losers <- schedule_post %>% select(loser, loser_error, weight) %>% 
  rename(error = loser_error, team = loser)

error <- 
  bind_rows(winners, losers) %>% mutate(weighted_error = error*weight) %>% 
  group_by(team) %>% summarise(weighted_error = sum(weighted_error)) %>%
  arrange(desc(weighted_error))

saveRDS(error, file = "error.RDS")