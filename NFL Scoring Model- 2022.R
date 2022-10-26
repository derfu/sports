install.packages('tidyverse')
install.packages('nflfastR')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('ggrepel')
install.packages('hrbrthemes')
install.packages('ggimage')
install.packages('gt')

library(tidyverse)
library(nflfastR)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(hrbrthemes)
library(ggimage)
library(gt)
hrbrthemes::import_roboto_condensed()

########################################################################

games <- readRDS(url("http://www.habitatring.com/games.rds"))

games2 <- games %>%
  filter(season > 2011)

seasons <- 2012:2022
pbp <- purrr::map_df(seasons, function(x) {
  nflfastR::load_pbp(2012:2022)
})

results <- games2 %>%
  select(game_id, season, week, home_team, home_score, away_team, away_score)

#######################################################################

offense <- pbp %>%
  group_by(game_id, posteam, season, week) %>% 
  summarize(plays = n(),
            off_epa_play = mean(epa),
            off_total_epa = sum(epa),
            off_success_rate = mean(success),
            explosive_play_rate = sum(epa>0.75) / plays,
            bad_play_rate = sum(epa < -0.6)/ plays,
            avg_wpa = mean(wpa, na.rm=T),
            series_success = mean(series_success),
            cpoe = mean(cpoe, na.rm=T),
            avg_yardline = mean(100 - (yardline_100)))

results <- results %>%
  mutate(
    home_team = case_when(
      home_team == 'OAK' ~ 'LV',
      home_team == 'SD' ~ 'LAC',
      home_team == 'STL' ~ 'LA',
      TRUE ~ home_team
    )
  )

results <- results %>%
  mutate(
    away_team = case_when(
      away_team == 'OAK' ~ 'LV',
      away_team == 'SD' ~ 'LAC',
      away_team == 'STL' ~ 'LA',
      TRUE ~ away_team
    )
  )

defense <- pbp %>%
  group_by(game_id, defteam, season, week) %>% 
  summarize(plays = n(),
            def_good_play_rate = (sum(epa < -0.6)/plays))

############################################################################################

home_results <- results %>%
  select(-away_team, -away_score)

home_results <- home_results %>%
  left_join(offense, by = c('game_id', 'season', 'week', 'home_team' = 'posteam'))

home_results <- home_results %>%
  left_join(defense, by = c('game_id', 'season', 'week', 'home_team' = 'defteam'))

home_results %>% 
  select(-game_id, -season, -week, -home_team) %>%
  cor(use="complete.obs") %>%
  round(2)

names(home_results)[names(home_results) == 'plays'] <- 'home_plays'
names(home_results)[names(home_results) == 'off_epa_play'] <- 'home_off_epa_play'
names(home_results)[names(home_results) == 'off_total_epa'] <- 'home_off_total_epa'
names(home_results)[names(home_results) == 'off_success_rate'] <- 'home_off_success_rate'
names(home_results)[names(home_results) == 'off_sd_epa'] <- 'home_off_sd_epa'
names(home_results)[names(home_results) == 'explosive_play_rate'] <- 'home_explosive_play_rate'
names(home_results)[names(home_results) == 'bad_play_rate'] <- 'home_bad_play_rate'
names(home_results)[names(home_results) == 'def_good_play_rate.x'] <- 'home_def_good_play_rate'

away_results <- results %>%
  select(-home_team, -home_score)

away_results <- away_results %>%
  left_join(offense, by = c('game_id', 'season', 'week', 'away_team' = 'posteam'))

home_results <- home_results %>%
  left_join(defense, by = c('game_id', 'season', 'week', 'home_team' = 'defteam'))

away_results %>% 
  select(-game_id, -season, -week, -away_team) %>%
  cor(use="complete.obs") %>%
  round(2)

names(away_results)[names(away_results) == 'plays'] <- 'away_plays'
names(away_results)[names(away_results) == 'off_epa_play'] <- 'away_off_epa_play'
names(away_results)[names(away_results) == 'off_total_epa'] <- 'away_off_total_epa'
names(away_results)[names(away_results) == 'off_success_rate'] <- 'away_off_success_rate'
names(away_results)[names(away_results) == 'off_sd_epa'] <- 'away_off_sd_epa'
names(away_results)[names(away_results) == 'explosive_play_rate'] <- 'away_explosive_play_rate'
names(away_results)[names(away_results) == 'bad_play_rate'] <- 'away_bad_play_rate'


home_away_results <- merge(home_results, away_results, by = c('game_id', 'season', 'week'))

###########################################

home_fit <- lm(home_score ~ 
                 home_off_epa_play + home_off_total_epa + home_explosive_play_rate + home_bad_play_rate, data = home_results)
summary(home_fit)

home_preds <- predict(home_fit, home_results) %>%
  as_tibble() %>%
  rename(home_prediction = value) %>%
  round(1) %>%
  bind_cols(
    home_results) %>%
  select(game_id, season, week, home_team, home_prediction, home_score, home_off_epa_play) %>%
  mutate(prediction_minus_actual = home_prediction - home_score)

away_fit <- lm(away_score ~ 
                 away_off_epa_play + away_off_total_epa + away_explosive_play_rate + away_bad_play_rate, data = away_results)
summary(away_fit)

away_preds <- predict(away_fit, away_results) %>%
  as_tibble() %>%
  rename(away_prediction = value) %>%
  round(1) %>%
  bind_cols(
    away_results) %>%
  select(game_id, season, week, away_team, away_prediction, away_score, away_off_epa_play) %>%
  mutate(prediction_minus_actual = away_prediction - away_score)

home_away_preds <- merge(home_preds, away_preds, by = c("game_id", "season", "week"))

#################################################################################################

### Home Model Plot 2012 - YTD 2022 ###

ggplot(home_preds, aes(x=home_prediction, y=home_score)) + 
  geom_point() +
  geom_smooth() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  labs(title = "Home Team's Actual Score (Y-Axis) Vs. Modeled Score (X-Axis)",
       x = "Modeled Score", 
       y = "Actual Score") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) 
ggsave('model1home.png', dpi=300)

ggplot(home_preds, aes(x=prediction_minus_actual)) + 
  geom_histogram(fill = "dark green", binwidth = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  geom_vline(xintercept =  0, color = "black") +
  geom_vline(xintercept =  -4.2, color = "red", linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept =  3.9, color = "red", linetype = "dashed", alpha = 0.6) +
  labs(title = "Home Team's Modeled Score Vs. Actual Score",
       x = "Model's Score - Actual Score", 
       y = "Count") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) 
ggsave('model2home.png', dpi=300)

### Away Model Plot 2012 - YTD 2022###

ggplot(away_preds, aes(x=away_prediction, y=away_score)) + 
  geom_point() +
  geom_smooth() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  labs(title = "Away Team's Actual Score (Y-Axis) Vs. Modeled Score (X-Axis)",
       x = "Modeled Score", 
       y = "Actual Score") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) 
ggsave('model1away.png', dpi=300)

ggplot(away_preds, aes(x=prediction_minus_actual)) + 
  geom_histogram(fill = "dark green", binwidth = 1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_bw() +
  geom_vline(xintercept =  0, color = "black") +
  geom_vline(xintercept =  -4.2, color = "red", linetype = "dashed", alpha = 0.6) +
  geom_vline(xintercept =  3.9, color = "red", linetype = "dashed", alpha = 0.6) +
  labs(title = "Away Team's Modeled Score Vs. Actual Score",
       x = "Model's Score - Actual Score", 
       y = "Count") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) 
ggsave('model2away.png', dpi=300)

############################################

predictions_2022 <- home_away_preds %>%
  filter(season == 2022) %>%
  select(-home_off_epa_play, -away_off_epa_play) %>%
  filter(!is.na(home_score))

names(predictions_2022)[names(predictions_2022) == 'prediction_minus_actual.x'] <- 'home_pred_minus_actual'
names(predictions_2022)[names(predictions_2022) == 'prediction_minus_actual.y'] <- 'away_pred_minus_actual'

home_pred_stats <- predictions_2022 %>%
  group_by(home_team) %>%
  summarize(home_games = n(),
            total_home_pred = sum(home_prediction),
            total_home_score = sum(home_score),
            total_home_pred_minus_actual = sum(home_pred_minus_actual))

away_pred_stats <- predictions_2022 %>%
  group_by(away_team) %>%
  summarize(away_games = n(),
            total_away_pred = sum(away_prediction),
            total_away_score = sum(away_score),
            total_away_pred_minus_actual = sum(away_pred_minus_actual))

home_defense_stats <- predictions_2022 %>%
  group_by(home_team) %>%
  summarize(away_games = n(),
            total_away_pred = sum(away_prediction),
            total_away_score = sum(away_score))

names(home_defense_stats)[names(home_defense_stats) == 'total_away_pred'] <- 'defense_pred'
names(home_defense_stats)[names(home_defense_stats) == 'total_away_actual'] <- 'defense_actual'

away_defense_stats <- predictions_2022 %>%
  group_by(away_team) %>%
  summarize(home_games = n(),
            total_home_pred = sum(home_prediction),
            total_home_score = sum(home_score))

names(away_defense_stats)[names(away_defense_stats) == 'total_home_pred'] <- 'away_defense_pred'
names(away_defense_stats)[names(away_defense_stats) == 'total_home_score'] <- 'away_defense_actual'

defense_predictions_2022 <- merge(home_defense_stats, away_defense_stats, by.x="home_team", by.y="away_team")

defense_predictions_2022 <- defense_predictions_2022 %>%
  mutate(games = home_games + away_games,
         total_d_pred = defense_pred + away_defense_pred,
         total_d_points = total_away_score + away_defense_actual) %>%
  select(home_team, games, total_d_pred, total_d_points)

defense_predictions_2022 <- defense_predictions_2022 %>%
  mutate(pred_d_ppg = total_d_pred / games,
         actual_d_ppg = total_d_points / games,
         pred_minus_actual = (pred_d_ppg - actual_d_ppg))

########################################################################
final_predictions_2022 <- merge(home_pred_stats, away_pred_stats, by.x="home_team", by.y="away_team")

final_predictions_2022 <- final_predictions_2022 %>%
  mutate(games = home_games + away_games,
         total_pred = total_home_pred + total_away_pred,
         total_points = total_home_score + total_away_score) %>%
  select(home_team, games, total_pred, total_points)

final_predictions_2022 <- final_predictions_2022 %>%
  mutate(pred_points_per_game = total_pred / games,
         actual_points_per_game = total_points / games,
         pred_minus_actual = (pred_points_per_game - actual_points_per_game))

teams_colors_logos <- teams_colors_logos

names(final_predictions_2022)[names(final_predictions_2022) == 'home_team'] <- 'team'

final_predictions_2022 <- final_predictions_2022 %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))


final_predictions_2022 %>%
  ggplot(aes(x = pred_points_per_game, y = actual_points_per_game)) +
  geom_hline(yintercept = mean(final_predictions_2022$actual_points_per_game), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept = mean(final_predictions_2022$pred_points_per_game), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = team_logo_espn), asp = 16 / 9) +
  stat_smooth(geom='line', alpha=0.6, se=FALSE, method='lm')+
  labs(x = "Average Post-Game Forecasted Points",
       y = "Average Actual Points Per Game",
       title = "Which NFL Teams Have Been Predicted to Score \n More/Less Points than They Actually Did",
       subtitle = "The predicted points are determined using statistics from each game, weeks 1-7",
       caption = "Made by Gay, For Derrick Fu") +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  annotate("text", x = c(28, 22), y = c(20, 28), 
           label = c("Scored less \n than expected", 
                     "Scored more \n than expected"), color = "blue")
ggsave('model3_7.png', dpi=300)

##########################################################################
o_and_d_forecast <- defense_predictions_2022 %>%
  select(home_team, pred_d_ppg)

offense_predictions_2022 <- final_predictions_2022 %>%
  select(c(-games, total_pred, total_points, actual_points_per_game, pred_minus_actual))

o_and_d_forecast <- o_and_d_forecast %>%
  left_join(offense_predictions_2022, by = c('home_team' = 'team'))

o_and_d_forecast <- o_and_d_forecast %>%
  select(c(-total_pred, total_points, actual_points_per_game, pred_minus_actual))

o_and_d_forecast %>%
  ggplot(aes(x = pred_points_per_game, y = pred_d_ppg)) +
  geom_hline(yintercept = mean(o_and_d_forecast$pred_d_ppg), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept = mean(o_and_d_forecast$pred_points_per_game), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = team_logo_espn),  asp = 16 / 9) +
  geom_abline(slope = -1.5, intercept = c(-2, 1, 4, 7, 10, 13, 16, 19, 22, 25), alpha = .4) +
  labs(x = "Offensive Forecasted Points Average",
       y = "Defensive Forecasted Points Average",
       title = "NFL Tiers Heading Into the Playoffs",
       subtitle = "The forecasted points are determined using advanced statistics from each game, weeks 1-7",
       caption = "Made by Gay, For Derrick Fu") +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

##alpha = ifelse(o_and_d_forecast$playoff_team == "yes", 0.9, 0.2),

ggsave('preweek-8-tiers.png')



###################################################################################################

week7 <- predictions_2022 %>% 
  filter(week == 7)

home_week7 <- week7 %>%
  group_by(home_team) %>%
  summarize(forecast = home_prediction,
            score = home_score)

away_week7 <- week7 %>%
  group_by(away_team) %>%
  summarize(forecast = away_prediction,
            score = away_score)

names(home_week7)[names(home_week7) == 'home_team'] <- 'team'
names(away_week7)[names(away_week7) == 'away_team'] <- 'team'

all_week7 <- rbind(home_week7, away_week7)

all_week7 <- all_week7 %>%
left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

write.csv(all_week7, "all_week7.csv")

all_week7 %>%
  ggplot(aes(x = forecast, y = score)) +
  geom_hline(yintercept = mean(all_week7$score), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept = mean(all_week7$forecast), color = "blue", linetype = "dashed", alpha=0.5) +
  geom_image(aes(image = team_logo_espn),  asp = 16 / 9 ) +
  stat_smooth(geom='line', alpha=0.6, se=FALSE, method='lm') +
  labs(x = "Post-Game Forecasted Points",
       y = "Actual Points",
       title = "NFL Week 7 Post-Game Forecasted Points ",
       subtitle = "The forecasted points are calculated based on in-game statistics",
       caption = "Made by Gay, For Derrick Fu") +
  theme_bw() +
  theme(
    aspect.ratio = 9 / 16,
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  annotate("text", x = c(37, 15), y = c(15, 40), 
           label = c("Scored less \n than expected", 
                     "Scored more \n than expected"), color = "blue")

ggsave('week7-post-game.png', dpi=300)

