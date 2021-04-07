library(tidyverse)
library(sf)
library(cartogram)
library(rnaturalearth)
library(showtext)

font_add("treasure_map", regular = "fonts/Treasuremap-nRxz0.ttf")
showtext_auto()

forest <- read_csv("data/forest-area-km.csv") %>% 
  janitor::clean_names() %>% 
  filter(year == "2020") %>% 
  mutate(code = ifelse(code == "SSD", "SDS", code)) %>% 
  drop_na()

forest_change <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv') %>% 
  filter(year %in% c("1990", "2015"), !is.na(code)) %>% 
  pivot_wider(names_from = "year", values_from = "forest_area") %>% 
  janitor::clean_names() %>% 
  mutate(delta = x2015- x1990,
         delta = ifelse(is.na(delta), 0, delta))
quantiles <- quantile(forest_change$delta, probs = seq(0, 1, 0.1))
forest_change$quantile <- findInterval(forest_change$delta,quantiles)

world <- ne_countries(scale = 50, returnclass = "sf") %>%
  select(admin, geometry, adm0_a3)

proj <- "+proj=moll"

forest_country <- world %>% 
  left_join(forest, by = c("adm0_a3" = "code")) %>% 
  mutate(forest_area = ifelse(is.na(forest_area), 1e4, forest_area)) %>% 
  st_transform(proj) %>% 
  cartogram_cont(weight = "forest_area", itermax = 40) %>% 
  left_join(forest_change, by = c("adm0_a3" = "code"))

forest_country <- forest_country %>% 
  left_join(forest_change, by = c("adm0_a3" = "code"))

ggplot(forest_country, aes(fill = quantile.y)) + 
  geom_sf(color = "grey30", size = 0.2, show.legend = F) + 
  coord_sf(label_graticule = "SW") +
  scale_fill_gradient2(low = "darkred", 
                       mid = "#D8F3DC", 
                       high = "darkgreen", 
                       midpoint = 4.5) +
  labs(title = "Cartogram of Trees",
       caption = "#TidyTuesday 2021-04-06",
       subtitle = "<i style='color: #888888'>Area depends on Number of Trees in 2015.  Colour Based on </i><b><i style='color: #006400; font-size: 25px;'>Increase</i></b><i style='color: #888888'> or </i><b><i style='color: #8B0000; font-size: 25px;'>Decrease </i></b><i style='color: #888888'> Compared to 1990</i>") +
  theme_ipsum() +
  theme(plot.title = ggtext::element_markdown(family = "treasure_map", size = 24),
        plot.subtitle = ggtext::element_markdown(family = "treasure_map"))
           