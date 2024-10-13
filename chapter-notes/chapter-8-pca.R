list_of_packages_ch8 = c("nflfastR", "tidyverse", "rvest", "htmlTable", "multiUS", "ggthemes")
lapply(list_of_packages_ch8, library, character.only = TRUE)

combine = tibble()
for(i in seq(from = 2000, to = 2024)) {
  url = paste0("https://www.pro-football-reference.com/draft/",
               i,
               "-combine.htm")
  web_data = 
    rvest::read_html(url) |> 
    rvest::html_table()
  web_data_clean = web_data[[1]] |> 
    dplyr::mutate(Season = i) |> 
    dplyr::filter(Ht != "Ht")
  combine = bind_rows(combine, web_data_clean)
}

combine = combine |> 
  dplyr::mutate(ht_ft = as.numeric(str_sub(Ht, 1, 1)),
                ht_in = (str_sub(Ht, 2, 4)),
                ht_in = as.numeric(str_remove(ht_in, "-")),
                Ht = ht_ft * 12 + ht_in,
                Ht = as.numeric(Ht),
                Wt = as.numeric(Wt),
                `40yd` = as.numeric(`40yd`),
                Vertical = as.numeric(Vertical),
                Bench = as.numeric(Bench),
                `Broad Jump`= as.numeric(`Broad Jump`),
                `3Cone` = as.numeric(`3Cone`),
                Shuttle = as.numeric(Shuttle),
                Season = as.numeric(Season)) |> 
  dplyr::select(-ht_ft, -ht_in)
summary(combine)

write_csv(combine,"combine.csv")


ggplot(combine, aes(x=Ht, y=Wt)) +
  geom_point() +
  theme_bw() +
  xlab("Player Height (inches)") +
  ylab("Player Weight (pounds)") +
  geom_smooth(method = "lm", formula = y ~ x)

ggplot(combine, aes(x=Wt, y=`40yd`)) +
  geom_point() +
  theme_bw() +
  xlab("Player Weight (pounds)") +
  ylab("Player 40-yard dash (seconds)") +
  geom_smooth(method = "lm", formula = y ~ x)

ggplot(combine, aes(x = `40yd`, y = Vertical)) +
  geom_point() +
  theme_bw() +
  xlab("Player 40-yard dash (seconds)") +
  ylab("Player Vertical Leap (inches)") +
  geom_smooth(method = "lm", formula = y ~ x)

ggplot(combine, aes(x = `40yd`, y = `3Cone`)) +
  geom_point() +
  theme_bw() +
  xlab("Player 40-yard dash (seconds)") +
  ylab("Player 3 cone drill (inches)") +
  geom_smooth(method = "lm")

#KNN for missing combine data

combine_knn_file = "combine_knn.csv"

if (!file.exists(combine_knn_file)) {
  impute_input = combine |> 
    dplyr::select(Ht:Shuttle) |> 
    as.data.frame()
  knn_out = multiUS::KNNimp(impute_input, k = 10,
                            scale = TRUE,
                            meth = "median") |> 
    as_tibble()
  
  combine_knn = combine |> 
    dplyr::select(Player:College, Season) |> 
    bind_cols(knn_out)
  write_csv(x = combine_knn,
            file = combine_knn_file)
} else(
  combine_knn = read_csv(combine_knn_file)
)
