library(haven)
library(fuzzyjoin)

# 1. DATA
ethnic_shares <- read_dta("data/Burgess_replication/main-tables-figures/data-preparation-main-tables-figures/ethnic_shares.dta")


# 2. JOIN
districts_1979 <- districts_1979 %>%
  mutate(join_name = toupper(ADMIN_NAME))

ethnic_shares <- ethnic_shares %>%
  mutate(join_name = toupper(distname_1979)) %>%
  mutate(join_name = ifelse(join_name == "TAITA", "TAITA TAVE", join_name))

districts_map_data <- districts_1979 %>%
  stringdist_left_join(ethnic_shares, by = "join_name", max_dist = 1) %>%
  st_as_sf()


# 3. DRAW
plot_ss_kenyatta <- ggplot() +
  geom_sf(data = waterbodies_1979, fill = "lightblue") +
  geom_sf(data = districts_map_data, aes(fill = kikuyu_share62), color = "black", linewidth = 0.2) +
  scale_fill_gradient(low = "white", high = "orange", na.value = "grey90",
                      name = "Kikuyu share\nin 1962") +
  
  geom_sf(data = settlements_kenyatta, fill = "steelblue", color = "darkblue", alpha = 0.8, linewidth = 0.3) +
  
  theme_minimal() +
  labs(
    title = "Settlement schemes allocated during Kenyatta presidency",
    subtitle = "Red = Settlement Schemes | Orange = Kikuyu share in 1962",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

plot_ss_moi <- ggplot() +
  geom_sf(data = waterbodies_1979, fill = "lightblue") +
  geom_sf(data = districts_map_data, aes(fill = kalenjin_share62), color = "black", linewidth = 0.2) +
  
  scale_fill_gradient(low = "white", high = "orange", na.value = "grey90",
                      name = "Kalenjin share\nin 1962") +
  
  geom_sf(data = settlements_moi, fill = "red", color = "darkred", alpha = 0.8, linewidth = 0.3) +
  
  theme_minimal() +
  labs(
    title = "Settlement schemes allocated during Moi presidency",
    subtitle = "Red = Settlement Schemes | Orange = Kalenjin share in 1962",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

plot_ss_kibaki <- ggplot() +
  geom_sf(data = waterbodies_1979, fill = "lightblue") +
  geom_sf(data = districts_map_data, aes(fill = kikuyu_share62), color = "black", linewidth = 0.2) +
  scale_fill_gradient(low = "white", high = "orange", na.value = "grey90",
                      name = "Kikuyu share\nin 1962") +
  
  geom_sf(data = settlements_kibaki, fill = "forestgreen", color = "darkgreen", alpha = 0.8, linewidth = 0.3) +
  
  theme_minimal() +
  labs(
    title = "Settlement schemes allocated during Kibaki presidency",
    subtitle = "Red = Settlement Schemes | Orange = Kikuyu share in 1962",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )


ggsave("output/plots/settlement_schemes_kenyatta.png", plot = plot_ss_kenyatta, width = 30, height = 24)
ggsave("output/plots/settlement_schemes_moi.png", plot = plot_ss_moi, width = 30, height = 24)
ggsave("output/plots/settlement_schemes_kibaki.png", plot = plot_ss_kibaki, width = 30, height = 24)