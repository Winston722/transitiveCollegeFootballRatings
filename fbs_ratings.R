
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
  select(1) %>% filter(row_number() != 1) %>% 
  bind_cols(., team = gsub("`","",rownames(.))) %>% arrange(desc(Estimate)) %>% 
  mutate(power_margin_rating = scale(Estimate, scale = F)) %>% 
  left_join(team_count, by = "team") %>% select(-Estimate)

rownames(ratings) <- NULL

fbs_ratings <- 
  ratings %>% 
  filter(count >= max(team_count$count)*.5) %>% 
  select(team, power_margin_rating) %>% 
  mutate(power_margin_rating = as.numeric(power_margin_rating))

options(warn=0)

saveRDS(schedule, "schedule.rds")
saveRDS(ratings, "ratings.rds")


