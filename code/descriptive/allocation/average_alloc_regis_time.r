
settlements_unregistered <- st_read("data/Resettlement_schemes/Kenya Unknown Location Settlement Schemes (V2)_FILES/UNKNOWN_schemes_pnts.shp")

cols_to_keep <- c("Year_Regis", "Allocation", "Remarks")

combined_data <- bind_rows(
  settlements %>% st_drop_geometry() %>% select(all_of(cols_to_keep)),
  settlements_unregistered %>% st_drop_geometry() %>% select(all_of(cols_to_keep))
)


result <- combined_data %>%
  st_drop_geometry() %>%
  filter(str_starts(Remarks, "Registered")) %>%
  summarise(avg_dist = mean(abs(Year_Regis - Allocation))) %>%
  pull(avg_dist)

print(result)

not_registered_count <- combined_data %>%
  filter(str_starts(Remarks, "Not Registered")) %>%
  nrow()

print(not_registered_count)