install.packages("nflfastR")
library(nflfastR)
library(tidyverse)

pbp_2023 = nflfastR::load_pbp(2023)
pbp_2024 = nflfastR::load_pbp(2024)


pbp_2023_p = 
  pbp_2023 |> 
  dplyr::filter(play_type == "pass" & !is.na(air_yards))

pbp_2024_p = 
  pbp_2024 |> 
  dplyr::filter(play_type == "pass" & !is.na(air_yards))

pbp_2023_p |> 
  dplyr::group_by(passer_id, passer) |> 
  dplyr::summarize(n = n(), adot = mean(air_yards)) |> 
  dplyr::filter(n >= 100 & !is.na(passer)) |> 
  arrange(-adot) |> 
  print(n = Inf)

pbp_2024_p |> 
  dplyr::group_by(passer_id, passer) |> 
  dplyr::summarize(n = n(), adot = mean(air_yards)) |> 
  dplyr::filter(n >= 100 & !is.na(passer)) |> 
  arrange(-adot) |> 
  print(n = Inf)
