list_of_packages_ch8 = c("nflfastR", "tidyverse", "rvest", "htmlTable", "multiUS", "ggthemes", "RColorBrewer")
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
read_csv("combine.csv")

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

wt_ht = combine |> 
  dplyr::select(Wt, Ht) |> 
  dplyr::filter(!is.na(Wt) & !is.na(Ht))

pca_fit_wt_ht = 
  prcomp(wt_ht)

pca_fit_wt_ht$x |> 
  as_tibble() |> 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point() +
  theme_bw()

#scaled input data
scaled_combine_knn = 
  scale(combine_knn |> dplyr::select(Ht:Shuttle))

pca_fit = prcomp(scaled_combine_knn)

pca_var = pca_fit$sdev^2
pca_percent = round(pca_var / sum(pca_var) * 100, 2)
print(pca_percent)

combine_knn = bind_cols(combine_knn, pca_fit$x)

ggplot(combine_knn, aes(x = PC1, y = PC2, color = PC3)) +
  geom_point() +
  theme_bw() +
  xlab(paste0("PC1 = ", pca_percent[[1]], "%")) +
  ylab(paste0("PC2 = ", pca_percent[[2]], "%")) +
  scale_color_continuous(
    paste0("PC3 = ", pca_percent[3], "%"),
    low = "skyblue", high = "navyblue"
  )

color_count = length(unique(combine_knn$Pos)) 
get_palette = colorRampPalette(brewer.pal(9, "Set1"))
  
ggplot(combine_knn, aes(x = PC1, y = PC2, color = Pos)) +
  geom_point(alpha = 0.75) +
  theme_bw() +
  xlab(paste0("PC1 = ", pca_percent[[1]], "%")) +
  ylab(paste0("PC2 = ", pca_percent[[2]], "%")) +
  scale_color_manual("Player position",
                     values = get_palette(color_count))

#Kmeans clustering

set.seed(123)
k_means_fit = kmeans(combine_knn |> dplyr::select(PC1, PC2),
                     centers = 6, iter.max = 10)

combine_knn = dplyr::mutate(combine_knn,cluster = k_means_fit$cluster)

combine_knn |> 
  dplyr::select(Pos, Ht:Shuttle, cluster) |> 
  head()

combine_knn |> 
  dplyr::filter(cluster == 1) |> 
  dplyr::group_by(Pos) |> 
  dplyr::summarize(n = n(), Ht = mean(Ht), Wt = mean(Wt)) |> 
  dplyr::arrange(-n) |> 
  print(n=Inf)

combine_knn_cluster = combine_knn |> 
  dplyr::group_by(cluster, Pos) |> 
  dplyr::summarize(n = n(), Ht = mean(Ht), Wt = mean(Wt),
                   .groups ="drop")

combine_knn_cluster |> 
  ggplot(aes(x = n, y = Pos)) +
  geom_col(position = 'dodge') +
  theme_bw() +
  facet_wrap(vars(cluster)) +
  theme(strip.background = element_blank()) +
  ylab("Position") +
  xlab("Count")

combine_knn_cluster |> 
  dplyr::group_by(cluster) |> 
  dplyr::summarize(ave_ht = mean(Ht),
                   ave_wt = mean(Wt))
