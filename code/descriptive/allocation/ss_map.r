district_labels <- districts_1979 %>%
  st_point_on_surface()

plot_ss_map <- ggplot() +
  geom_sf(data = waterbodies_1979, fill = "lightblue") +
  geom_sf(data = districts_1979, fill = "antiquewhite", linewidth = 0.3) +
  geom_sf(data = settlements_kenyatta, fill = "steelblue", color = "darkblue") +
  geom_sf(data = settlements_moi, fill = "red", color = "darkred") +
  geom_sf(data = settlements_kibaki, fill = "forestgreen", color = "darkgreen") +
  geom_sf(data = districts_1979, fill = NA, color = "black", linewidth = 0.3) +
  geom_sf_label(
    data = district_labels,
    aes(label = ADMIN_NAME),
    size = 2,
    fill = "white",
    color = "black",
    label.size = 0.1,
  ) +
  theme_minimal() +
  labs(
    title = "Kenya Settlement Schemes grouped by Presidential Era",
    x = "Longitude",
    y = "Latitude",
  )

print(plot_ss_map)
ggsave("output/plots/settlement_schemes_by_era.png", plot = plot_ss_map, width = 30, height = 24)