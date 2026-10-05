library(tidyverse)
library(sf)
library(ggplot2)

districts_1979 <- st_read("data/Kenya_districts/geo2_ke1979/geo2_ke1979.shp")
settlements <- st_read("data/Resettlement_schemes/Kenya Settlement Schemes (V2)_FILES/Settlement_Schemes.shp")
unknown_settlements <- st_read("data/Resettlement_schemes/Kenya Unknown Location Settlement Schemes (V2)_FILES/UNKNOWN_schemes_pnts.shp")


# De facto allocation
filter_column <- settlements$Year_Regis

# De jure allocation
# filter_column <- settlements$Allocation

settlements_kenyatta <- settlements %>%
  filter(filter_column >= 1962 & filter_column <= 1979)
settlements_moi <- settlements %>%
  filter(filter_column >= 1980 & filter_column <= 2002)
settlements_kibaki <- settlements %>%
  filter(filter_column >= 2003)

waterbodies_1979 <- districts_1979 %>%
  filter(ADMIN_NAME == "Waterbodies")

districts_1979 <- districts_1979 %>%
  filter(ADMIN_NAME != "Waterbodies")