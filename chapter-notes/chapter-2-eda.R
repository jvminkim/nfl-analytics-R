#This script details how to investigate passingh pbp data through summaries, boxplots and histograms.
#It also introduces how to compare year-to-year statistics.

ist_of_packages_ch2 = c("nflfastR", "tidyverse")
lapply(list_of_packages_ch2, library, character.only = TRUE)

pbp16_23 = nflfastR::load_pbp(2016:2024)

pbp16_23_pass = dplyr::filter(pbp16_23, play_type == "pass" & !is.na(air_yards))

pbp16_23_pass = dplyr::mutate(pbp16_23_pass,
                              pass_length_air_yards = ifelse(air_yards >= 20, "long", "short"),
                              passing_yards = ifelse(is.na(passing_yards), 0, passing_yards))

pbp16_23_pass |> 
  pull(passing_yards) |> 
  summary()

pbp16_23_pass |> 
  dplyr::filter(pass_length_air_yards == "long") |> 
  dplyr::pull(passing_yards) |> 
  base::summary()

#Summaries of epa by length of throws

pbp16_23_pass |> 
  dplyr::filter(pass_length_air_yards == "short") |> 
  dplyr::pull(epa) |> 
  base::summary()

pbp16_23_pass |> 
  dplyr::filter(pass_length_air_yards == "long") |> 
  dplyr::pull(epa) |> 
  base::summary()


ggplot(pbp16_23, aes(x = passing_yards)) +
  geom_histogram()

pbp16_23_pass |> 
  dplyr::filter(pass_length_air_yards == "long") |> 
  ggplot2::ggplot(aes(passing_yards)) +
  geom_histogram(binwidth = 1) +
  ylab("Count") +
  xlab("Yards gained (or lost) during passing plays on long passes") +
  theme_bw()

ggplot(pbp16_23_pass, aes(x=pass_length_air_yards, y = passing_yards)) +
  geom_boxplot() +
  theme_bw() +
  xlab("Pass length in yards (long >= 20 yards, short < 20 yards)") +
  ylab("Yards gained (or lost) during a passing play")

pbp16_23_pass_season = pbp16_23_pass |> 
  group_by(passer_player_name, passer_player_id, season) |> 
  summarize(
    ypa = mean(passing_yards, na.rm=TRUE),
    n = n(),
    .groups = "drop"
  )

pbp16_23_pass_season |> 
  arrange(-ypa) |> 
  print()
       
pbp_16_23_pass_season_100 =
  pbp16_23_pass_season |> 
  filter(n >=100) |> 
  arrange(-ypa)

pbp_16_23_pass_season_100 |> 
  print(n = 20)

#Season by season ypa

air_yards = pbp16_23_pass |> 
  dplyr::select(passer_id, passer, season,
                pass_length_air_yards, passing_yards) |> 
  dplyr::arrange(passer_id,season,
                 pass_length_air_yards, season) |> 
  group_by(passer_id, passer,
           pass_length_air_yards, season) |> 
  summarize(n = n(),
            ypa = mean(passing_yards),
            .groups = "drop") |> 
  filter((n >= 100 & pass_length_air_yards == "short") |
          (n >= 30 & pass_length_air_yards == "long")) |> 
  select(-n)

air_yards_lag = 
  air_yards |> 
  mutate(season = season + 1) |> 
  rename(ypa_last = ypa)

pbp16_23_pass_season_pl = air_yards |> 
  inner_join(air_yards_lag, by = c("passer_id", "pass_length_air_yards",
                                   "season", "passer"))
pbp16_23_pass_season_pl |> 
  filter(passer %in% c("T.Brady", "A.Rodgers")) |> 
  print(n = Inf)

pbp16_23_pass_season_pl |> 
  glimpse()

pbp16_23_pass_season_pl |> 
  distinct(passer_id) |> 
  nrow()

scatter_ypa =
  ggplot(pbp16_23_pass_season_pl, aes(x = ypa_last, y = ypa)) +
  geom_point() + 
  facet_grid(cols = vars(pass_length_air_yards)) +
  labs(
    x = "Yards per Attempt, Year n",
    y = "Yards per Attempt, Year n + 1"
  ) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  geom_smooth( method = "lm")

scatter_ypa

pbp16_23_pass_season_pl |> 
  filter(!is.na(ypa) & !is.na(ypa_last)) |> 
  group_by(pass_length_air_yards) |> 
  summarize(correlation = cor(ypa, ypa_last))
