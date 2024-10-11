list_of_packages_ch5 = c("nflfastR", "tidyverse")
lapply(list_of_packages_ch5, library, character.only = TRUE)

pbp = nflfastR::load_pbp(2016:2024)

pbp_pass = dplyr::filter(pbp, !is.na(passer_id))

pbp_pass_td = pbp_pass |> 
  dplyr::mutate(pass_touchdown = ifelse(is.na(pass_touchdown), 0, pass_touchdown)) |> 
  dplyr::group_by(season, week, passer_id, passer) |> 
  dplyr::summarize(
    n_passes = n(),
    pass_td_y = sum(pass_touchdown),
    total_line = mean(total_line)
  ) |> 
  dplyr::filter(n_passes >= 10)

pbp_pass_td |> 
  dplyr::group_by(pass_td_y) |> 
  dplyr::summarize(n = n())

pbp_pass_td |> 
  dplyr::ungroup() |> 
  dplyr::select(-passer, -passer_id) |> 
  summary()

pass_td_mean = pbp_pass_td |> 
  dplyr::pull(pass_td_y) |> 
  mean()

plot_td =
  tibble(x = seq(0,7)) |> 
  dplyr::mutate(expected = dpois(
    x = x,
    lambda = pass_td_mean
  ))

ggplot() +
  geom_histogram(
    data = pbp_pass_td,
    aes(x = pass_td_y,
        y = after_stat(count/sum(count))
        ),
    binwidth = 0.5
  ) +
  geom_line(data = plot_td, aes(x = x, y = expected),
            color = "red", linewidth = 1) +
  theme_bw() +
  xlab("Touchdown passes per player per game for 2016 to 2024") +
  ylab("Probability")

pbp_pass_td_g_10 = pbp_pass_td |> 
  dplyr::filter(n_passes >= 10)

x = tibble()

for(season_idx in seq(2017, 2023)) {
  for(week_idx in seq(1,22)) {
    week_calc = pbp_pass_td_g_10 |> 
      dplyr::filter(season == (season_idx - 1) |
                      (season == season_idx & week < week_idx)) |> 
      dplyr::group_by(passer_id, passer) |> 
      dplyr::summarize(n_games = n(),
                       pass_td_rate = mean(pass_td_y),
                       .groups = "keep") |> 
      dplyr::mutate(season = season_idx, week = week_idx)
    
    x = bind_rows(x, week_calc)
  }
}

x |> 
  dplyr::filter(passer == "P.Mahomes") |> 
  tail()

pbp_pass_td_g_10 = pbp_pass_td_g_10 |> 
  dplyr::inner_join(x, by = c(
    "season", "week",
    "passer_id", "passer"
  ))

weekly_passing_td_plot = pbp_pass_td_g_10 |> 
  ggplot(aes(x = week, y = pass_td_y, group = passer_id)) +
  geom_line(alpha = 0.25) +
  facet_wrap(vars(season), nrow = 3) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  ylab("Total passing touchdowns") +
  xlab("Week of season")

weekly_passing_td_plot

pass_fit = glm(pass_td_y ~ pass_td_rate + total_line,
               data = pbp_pass_td_g_10,
               family = "poisson")

pbp_pass_td_g_10 = pbp_pass_td_g_10 |> 
  dplyr::ungroup() |> 
  dplyr::mutate(exp_pass_td = predict(pass_fit, type = "response"))

summary(pass_fit) |> 
  print()

library(broom)
tidy(pass_fit, exponentiate = TRUE, conf.int = TRUE)

pbp_pass_td_g_10 = pbp_pass_td_g_10 |> 
  dplyr::mutate(p_0_td = dpois(x = 0,
                               lambda = exp_pass_td),
                p_1_td = dpois(x = 1,
                               lambda = exp_pass_td),
                p_2_td = dpois(x = 2,
                               lambda = exp_pass_td),
                p_g2_td = ppois(q = 2,
                               lambda = exp_pass_td,
                               lower.tail = FALSE))

pbp_pass_td_g_10 |> 
  dplyr::filter(passer == "P.Mahomes", season == 2022, week ==22) |> 
  dplyr::select(-pass_td_y, -n_games, -n_passes,
                -passer_id, -week, -season)
