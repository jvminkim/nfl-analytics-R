#This script shows how to create a generalised linear model through various completions statistics
#along with how to formulate correlations for YtY statistic.

list_of_packages_ch4 = c("nflfastR", "tidyverse", "broom")
lapply(list_of_packages_ch4, library, character.only = TRUE)

pbp = nflfastR::load_pbp(2016:2024)

pbp_pass = pbp |> 
  dplyr::filter(play_type == "pass" & !is.na(passer_id) &
                  !is.na(air_yards))

pass_pct = pbp_pass |> 
  dplyr::filter(0 < air_yards & air_yards <= 20) |> 
  dplyr::group_by(air_yards) |> 
  dplyr::summarize(comp_pct = mean(complete_pass), 
                   .groups = "drop")

pass_pct |> 
  ggplot(aes(x = air_yards, y = comp_pct)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw() +
  ylab("Percentile completion") +
  xlab("Air yards")

complete_ay = glm(complete_pass ~ air_yards,
                  data = pbp_pass, 
                  family = "binomial")

#Lositic regression plot

ggplot(data = pbp_pass,
       aes(x=air_yards, y = complete_pass)) +
  geom_jitter(height = 0.05, width = 0,
              alpha =0.05) +
  stat_smooth(method = "glm",
              method.args = list(family="binomial")) + 
  theme_bw() +
  ylab("Completed pass (1=yes, 0 = no)") +
  xlab("air yards")

pbp_pass = dplyr::mutate(pbp_pass,
                         exp_completion = stats::predict(complete_ay, type = "resp"),
                         cpoe = complete_pass - exp_completion)

pbp_pass |> 
  dplyr::group_by(season, passer_id, passer) |> 
  dplyr::summarize(n = n(),
                   cpoe = mean(cpoe,na.rm = TRUE),
                   compl = mean(complete_pass, na.rm = TRUE),
                   .groups = "drop") |> 
  dplyr::filter(n >= 100 & season == 2023) |> 
  dplyr::arrange(-cpoe) |> 
  print(n = 10)

pbp_pass_no_miss = pbp_pass |> 
  dplyr::mutate(down = factor(down),
                qb_hit = factor(qb_hit)) |> 
  dplyr::filter(complete.cases(down, qb_hit, complete_pass,
                               ydstogo, yardline_100, air_yards,
                               pass_location, qb_hit))

complete_more = pbp_pass_no_miss |> 
  glm(formula = complete_pass ~ down + ydstogo + yardline_100 +
                  air_yards + pass_location + qb_hit,
      family = "binomial")

pbp_pass_no_miss = dplyr::mutate(pbp_pass_no_miss, 
                                 exp_completion = stats::predict(complete_more, type = "resp"),
                                 cpoe = complete_pass - exp_completion)

cpoe_more = pbp_pass_no_miss |> 
  dplyr::group_by(season, passer_id, passer) |> 
  dplyr::summarize(n = n(),
                   cpoe = mean(cpoe, na.rm = TRUE),
                   compl = mean(complete_pass),
                   exp_completion = mean(exp_completion),
                   .groups = "drop") |> 
  filter(n > 100)

cpoe_more |> 
  dplyr::arrange(-cpoe) |> 
  print(n = 20)

#Stickiness compatison

cpoe_now = cpoe_more |> 
  dplyr::select(-n)

cpoe_last = cpoe_more |> 
  dplyr::select(-n) |> 
  dplyr::mutate(season = season + 1) |> 
  rename(cpoe_last = cpoe,
         compl_last = compl,
         exp_completion_last = exp_completion)

cpoe_lag = cpoe_now |> 
  dplyr::inner_join(cpoe_last,
                    by = c("passer_id", "passer", "season")) |> 
  ungroup()

cpoe_lag |> 
  dplyr::select(compl_last, compl) |> 
  cor(use="complete.obs")

cpoe_lag |> 
  dplyr::select(cpoe_last, cpoe) |> 
  cor(use="complete.obs")

cpoe_lag |> 
  dplyr::select(exp_completion_last, exp_completion) |> 
  cor(use="complete.obs")
