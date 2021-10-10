library(rvest)
library(tidyverse)
library(lubridate)
library(magrittr)

weight <- function(week_ago, game_count){
  game_weight <- game_count / max(game_count)
  time_weight <- 1 / week_ago^(1/3)
  #weight <- 1
  return(sqrt(game_weight*time_weight))
}

schedule_webpage <- read_html("https://www.sports-reference.com/cfb/years/2021-schedule.html")

schedule_raw <- schedule_webpage %>% html_table() %>% data.frame()

pattern <- readRDS("pattern.rds")
anchor_date <- as.Date(cut(today(), breaks='week', start.on.monday = F))

schedule <- 
  schedule_raw %>% filter(Date != "Date") %>% 
  mutate(date = as.Date(Date, format = "%b %d, %Y")
         , winner = str_sub(Winner, pmax(regexpr(pattern, Winner)+2,1))
         , winner_pts = as.numeric(Pts)
         , loser = str_sub(Loser, pmax(regexpr(pattern, Loser)+2,1))
         , loser_pts = as.numeric(Pts.1)
         , winner_location = ifelse(Var.8 == "@", "away"
                                    , ifelse(Var.8 == "N", "neutral","home"))
         , game_margin = winner_pts - loser_pts
         , true_margin = game_margin + ifelse(Var.8 == "@", 3
                                              , ifelse(Var.8 == "N", 0, -3))
         , .keep = "none"
  ) %>% 
  filter(complete.cases(.)) %>%
  mutate(week = as.numeric(round((anchor_date - date)/7)+1)) %>% 
  filter(week != 0)

all_teams <- unique(c(schedule$winner, schedule$loser))

team_count <-
  c(schedule$winner, schedule$loser) %>% 
  table() %>% data.frame() 
colnames(team_count) <- c("team", "count")
team_count %<>% mutate(team = as.character(team))

schedule <- schedule %>% 
  left_join(y= team_count, by = c("winner" = "team")) %>% 
  left_join(y= team_count, by = c("loser" = "team")) %>% 
  mutate(game_count = count.x + count.y) %>% 
  mutate(weight = weight(week_ago = week, game_count = game_count))

options(warn=-1)

teams <- 
  c(schedule$winner, schedule$loser) %>% 
  table() %>% data.frame() %>% 
  filter(Freq > 1) %>% pull(1) %>% as.character()

if(exists("team_matrix")){rm(team_matrix)}
for(i in teams){
  if(exists("team_matrix")){
    team_matrix <- team_matrix %>% 
      bind_cols((schedule$winner == i) - (schedule$loser == i))
  } else {
    team_matrix <- data.frame(team = (schedule$winner == i) - (schedule$loser == i))
  }
}

colnames(team_matrix) <- teams
team_matrix <- team_matrix %>% 
  mutate(true_margin = schedule$true_margin
         , weight = weight(week_ago = schedule$week, game_count = schedule$game_count))

team_matrix %<>% filter((team_matrix %>% select(-weight, -true_margin) %>% rowSums()) == 0)

output <- lm(data = team_matrix, true_margin~. - weight, weights = weight) %>% summary()
ratings <- output$coefficients %>% as.data.frame() %>% 
  select(1) %>% 
  bind_cols(., team = gsub("`","",rownames(.))) %>% 
  mutate(team = gsub("\\(Intercept\\)",teams[length(teams)],team)) %>% 
  arrange(desc(Estimate)) %>% 
  mutate(power_margin_rating = scale(Estimate, scale = F)) %>% 
  left_join(team_count, by = "team") %>% select(-Estimate) %>% 
  mutate(fbs = count >= max(team_count$count)*.5
         , as_of_week = max(team_count$count)-1) %>% 
  select(team, power_margin_rating, fbs, as_of_week, count) %>% 
  mutate(power_margin_rating = as.numeric(power_margin_rating))

rownames(ratings) <- NULL

saveRDS(schedule, "schedule.rds")
saveRDS(ratings, "ratings.rds")

################################################################################
schedule <- readRDS("schedule.rds")
ratings <- readRDS("ratings.rds")

schedule_post <- 
  schedule %>% left_join(ratings, by = c("winner" = "team")) %>% 
  rename(winner_rating = power_margin_rating, winner_count = count) %>% 
  left_join(ratings, by = c("loser" = "team")) %>% 
  rename(loser_rating = power_margin_rating, loser_count = count) %>% 
  select(winner, winner_pts, winner_rating, winner_count
         , loser, loser_pts, loser_rating, loser_count
         , true_margin, game_count, weight, week
         ) %>% 
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

saveRDS(error, file = "error.rds")

################################################################################

ratings <- readRDS("ratings.rds")
error <- readRDS("error.rds")

ratings_error <- ratings %>% left_join(error, by = "team")
saveRDS(ratings_error,"ratings_error.rds")
