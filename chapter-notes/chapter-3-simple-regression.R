#This script shows how to create a simple linear regression model along with 
#how to formulate correlations for YtY statistics through rushing statistics.

list_of_packages_ch3 = c("tidyverse", "nflfastR")
lapply(list_of_packages_ch3, library, character.only = TRUE)

pbp = nflfastR::load_pbp(2016:2024)

pbp_run = pbp |> 
  dplyr::filter(play_type == "run" & !is.na(rusher_id)) |> 
  mutate(rushing_yards = ifelse(is.na(rushing_yards), 0, rushing_yards))


ggplot(pbp_run, aes(x = ydstogo, y = rushing_yards)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = "lm")

pbp_run_avg =
  pbp_run |> 
  dplyr::group_by(ydstogo) |> 
  summarize(ypc = mean(rushing_yards))

pbp_run_avg |> 
  head(20)

ggplot(pbp_run_avg, aes(x = ydstogo, y = ypc)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm")

yards_to_go_model = lm(rushing_yards ~ 1 + ydstogo, data = pbp_run)

pbp_run = dplyr::mutate(pbp_run, ryoe = resid(yards_to_go_model))

ryoe = pbp_run |> 
  dplyr::group_by(season, rusher_id, rusher) |> 
  dplyr::summarize(
    n = n(),
    ryoe_total = sum(ryoe),
    ryoe_per = mean(ryoe),
    yards_per_carry = mean(rushing_yards)
  ) |> 
  dplyr::arrange(-ryoe_total) |> 
  dplyr::filter(n > 50)

ryoe_2023 = dplyr::filter(ryoe, season == 2023)
ryoe_2024 = dplyr::filter(ryoe, season == 2024)

ryoe_2023 |> 
  dplyr::arrange(-ryoe_per)

ryoe_2024 |> 
  dplyr::arrange(-ryoe_per)

ryoe_2023 |> 
  dplyr::arrange(-ryoe_total)

ryoe_2024 |> 
  dplyr::arrange(-ryoe_total)

#RYOE vs YPC

ryoe_now = dplyr::select(ryoe, -n, -ryoe_total)

ryoe_last = ryoe |> 
  dplyr::select(-n, -ryoe_total) |> 
  dplyr::mutate(season = season + 1) |> 
  dplyr::rename(ryoe_per_last = ryoe_per,
                yards_per_carry_last = yards_per_carry)

ryoe_lag = ryoe_now |> 
  inner_join(ryoe_last, by = c("rusher_id", "rusher", "season")) |> 
  ungroup()

ryoe_lag |> 
  dplyr::select(yards_per_carry, yards_per_carry_last) |> 
  cor(use = "complete.obs")

ryoe_lag |> 
  dplyr::select(ryoe_per, ryoe_per_last) |> 
  cor(use = "complete.obs")
