#This script scrapes NFL draft data and analyzes historical draft value to decipher 
#winners and losers of previous trades and drafts.

list_of_packages_ch7 = c("janitor", "tidyverse", "rvest", "htmlTable", "zoo", "kableExtra")
lapply(list_of_packages_ch7, library, character.only = TRUE)

draft = tibble()

for(i in seq(from = 2000, to = 2024)) {
  url = paste0(
    "https://www.pro-football-reference.com/years/",
    i,
    "/draft.htm"
  )
  web_data =
    rvest::read_html(url) |>
    rvest::html_nodes(xpath = '//*[@id="drafts"]') |> 
    rvest::html_table()
  web_df = web_data[[1]]
  web_df_clean =
    web_df |> 
    janitor::row_to_names(row_number = 1) |> 
    janitor::clean_names(case = "none") |> 
    dplyr::mutate(Season = i) |> 
    dplyr::filter(Tm != "Tm")
  
  draft= bind_rows(draft, web_df_clean)
}

draft = draft |> 
  dplyr::mutate(Tm = case_when(Tm == "SDG" ~ "LAC",
                               Tm == "OAK" ~ "LVR",
                               Tm == "STL" ~ "LAR",
                               TRUE ~ Tm),
                DrAV = ifelse(is.na(DrAV), 0, DrAV))
draft_use = draft |> 
  dplyr::select(Season, Pick, Tm, Player, Pos, wAV, DrAV)

draft_pre_2019 = draft_use |> 
  dplyr::mutate(DrAV = as.numeric(DrAV),
                wAV = as.numeric(wAV),
                Pick = as.integer(Pick)) |>
  dplyr::filter(Season <= 2019)

ggplot(draft_pre_2019, aes(Pick, DrAV)) +
  geom_point(alpha = 0.2) +
  stat_smooth() +
  theme_bw()

draft_chart = draft_pre_2019 |> 
  dplyr::group_by(Pick) |> 
  dplyr::summarize(mean_DrAV = mean(DrAV, na.rm = TRUE)) |> 
  dplyr::mutate(mean_DrAV = ifelse(is.na(mean_DrAV), 0, mean_DrAV)) |> 
  dplyr::mutate(
    roll_DrAV = 
      zoo::rollapply(mean_DrAV,
                     width = 13,
                     FUN = mean,
                     na.rm = TRUE,
                     fill = "extend",
                     partial = TRUE)
  )
ggplot(draft_chart, aes(Pick, roll_DrAV)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  ylab("Rolling Average (\u00B1 ^) DrAV") +
  xlab("Draft pick")

DrAV_pick_fit = draft_chart |> 
  lm(formula = log(roll_DrAV + 1) ~ Pick)

draft_chart = draft_chart |> 
  dplyr::mutate(fitted_DrAV = pmax(0, exp(predict(DrAV_pick_fit)) - 1))

draft_chart

#Jets Colts 2018 Trade

future_pick = tibble(
  Pick = "Future 2nd Round",
  Value = "14.8 (discounted at rate of 25%)"
)

team = tibble("Receiving team" = c("Jets", rep("Colts", 4)))

tbl_1 = draft_chart |> 
  dplyr::filter(Pick %in% c(3,6,37,49)) |> 
  dplyr::select(Pick, fitted_DrAV) |> 
  dplyr::rename(Value = fitted_DrAV) |> 
  dplyr::mutate(Pick = as.character(Pick),
                Value = as.character(round(Value,1))) |> 
  bind_rows(future_pick)

team |> 
  bind_cols(tbl_1) |> 
  kbl(format = "pipe") |> 
  kable_styling()

results_trade = 
  tibble(
    Team = c("Jets", rep("Colts", 5)),
    Pick = c(
      3, 6, 37,
      "49-traded for 52",
      "49-traded for 169",
      "52 in 2019"
    ),
    Player = c(
      "Sam Darnold",
      "Quenton Nelson",
      "Braden Smith",
      "Kemoko Turay",
      "Jordan Wilkins",
      "Rock Ya-Sin"
    ),
    "DrAV" = c(25,55,32,5,8,11)
  )

draft_pre_2019 =
  draft_pre_2019 |> 
  dplyr::left_join(draft_chart |> dplyr::select(Pick, fitted_DrAV),
                   by = "Pick"
                   )
draft_pre_2019 |> 
  dplyr::group_by(Tm) |> 
  dplyr::summarize(
    total_picks = n(),
    DrAV_OE = mean(DrAV - fitted_DrAV, na.rm=TRUE),
    DrAV_sigma = sd(DrAV - fitted_DrAV, na.rm = TRUE)
  ) |> 
  dplyr::mutate(se = DrAV_sigma / sqrt(total_picks),
                lower_bound = DrAV_OE - 1.96* se,
                upper_bound = DrAV_OE + 1.96 * se) |> 
  dplyr::arrange(-DrAV_OE) |> 
  print(n = Inf)
