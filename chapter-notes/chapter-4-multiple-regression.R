#This script shows how to create a multiple linear regression model along with 
#how to formulate correlations for YtY statistics through rushing statistics.

list_of_packages_ch4 = c("nflfastR", "dplyr", "ggplot2", "broom", "kableExtra")
lapply(list_of_packages_ch4, library, character.only = TRUE)

demo_data = tibble(down = c("first", "second"),
                   ydstogo = c(10,5))

model.matrix(~ ydstogo + down,
             data = demo_data)

pbp = nflfastR::load_pbp(2016:2024)

pbp_run = pbp |> 
  dplyr::filter(play_type == "run" & !is.na(rusher_id) &
                  !is.na(down) & !is.na(run_location)) |> 
  dplyr::mutate(rushing_yards = ifelse(is.na(rushing_yards),
                                       0,
                                       rushing_yards))

pbp_run = dplyr::mutate(pbp_run, down = as.character(down))

ggplot(pbp_run, aes(x = rushing_yards)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(down), ncol=2,
             labeller = label_both) +
  theme_bw() +
  theme(strip.background = element_blank())

pbp_run |> 
  dplyr::filter(ydstogo==10) |> 
  ggplot(aes(x = down, y = rushing_yards)) +
  geom_boxplot() +
  theme_bw()

ggplot(pbp_run, aes(x = yardline_100, y = rushing_yards)) +
  geom_point(alpha = 0.25) +
  stat_smooth(method = "lm") +
  theme_bw()

pbp_run |> 
  dplyr::group_by(yardline_100) |> 
  dplyr::summarize(rushing_yards_mean = mean(rushing_yards)) |> 
  ggplot(aes(x = yardline_100, y = rushing_yards_mean)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()

ggplot(pbp_run, aes(run_location, rushing_yards)) +
  geom_boxplot() +
  theme_bw()

pbp_run |> 
  dplyr::group_by(score_differential) |> 
  dplyr::summarize(rushing_yards_mean = mean(rushing_yards)) |> 
  ggplot(aes(score_differential, rushing_yards_mean)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()

expected_yards = 
  lm(rushing_yards ~ 1 + down + ydstogo + down:ydstogo +
       yardline_100 + run_location + score_differential,
     data = pbp_run)

pbp_run = dplyr::mutate(pbp_run, ryoe = resid(expected_yards))

expected_yards |> 
  tidy(conf.int = TRUE) |> 
  kbl(format = "pipe", digits = 2) |> 
  kable_styling()

ryoe = pbp_run |> 
  dplyr::group_by(season, rusher_id, rusher) |> 
  dplyr::summarize(
    n = n(), ryoe_total = sum(ryoe), ryoe_per = mean(ryoe),
    yards_per_carry = mean(rushing_yards)
  ) |> 
  dplyr::filter(n > 50)

ryoe |> 
  arrange(-ryoe_total) |> 
  print()

ryoe |> 
  dplyr::filter(season == 2024) |> 
  arrange(-ryoe_per) |> 
  print()

ryoe_now = dplyr::select(ryoe, -n, -ryoe_total)

ryoe_last = ryoe |> 
  dplyr::select(-n, -ryoe_total) |> 
  dplyr::mutate(season = season + 1) |> 
  dplyr::rename(ryoe_per_last = ryoe_per,
                yards_per_carry_last = yards_per_carry)

ryoe_lag = ryoe_now |> 
  dplyr::inner_join(ryoe_last, 
                    by = c("rusher_id", "rusher", "season")) |> 
  dplyr::ungroup()

ryoe_lag |> 
  dplyr::select(yards_per_carry, yards_per_carry_last) |> 
  cor(use = "complete.obs")

ryoe_lag |> 
  dplyr::select(ryoe_per, ryoe_per_last) |> 
  cor(use = "complete.obs")

par(mfrow = c(2,2))
plot(expected_yards)
