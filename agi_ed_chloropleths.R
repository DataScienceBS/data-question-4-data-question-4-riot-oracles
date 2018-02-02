## Cloropleth

## Total across zip code - removes agi ranges
zip_total <- combined_df %>% 
  filter(zip_code != 0 & is.na(agi_range))

## Filtering and summarising subject proficiency
chloro_df <- zip_total %>% 
  filter(is.na(agi_range)) %>% 
  select(CORE_region, system_name, county, Pct_ED, AlgI, AlgII, Math, 
         BioI, Chemistry, Science, ACT_Composite) %>%
  group_by(county) %>% 
  summarise_all(funs(mean)) %>% 
  filter(!is.na(county))

## Filter data to just include TN

TN_data <- map_data("state") %>% 
  filter(region =='tennessee')

TN_counties <- map_data("county") %>% 
  subset(., region == "tennessee")

TN_counties$subregion <- TN_counties$subregion %>% 
  gsub('de kalb', 'dekalb', .)

chloro_df$county_l <- sapply(chloro_df$county, tolower) %>% 
  gsub(' county','', .)

## Left join to merge polygon and county data

chloro_df <- left_join(x = TN_counties, y = chloro_df, by = c("subregion" = "county_l"))

## Switching to AGI data

agi_chloro_df <- zip_total %>% 
  filter(is.na(agi_range)) %>% 
  mutate(avg_agi = (agi_a * 1000)/return_c) %>% 
  select(CORE_region, system_name, county, avg_agi) %>%
  group_by(county) %>% 
  summarise_all(funs(mean)) %>% 
  filter(!is.na(county))

## Filter data to just include TN

agi_chloro_df$county_l <- sapply(agi_chloro_df$county, tolower) %>% 
  gsub(' county','', .)

## Left join to merge polygon and county data

agi_chloro_df <- left_join(x = TN_counties, y = agi_chloro_df, by = c("subregion" = "county_l"))

## Create maps

TN_agi_map <- ggplot() + 
  geom_polygon(data = agi_chloro_df, 
               aes(x = long, y = lat, group = group, fill = avg_agi),
               color = "white", size = 0.25) +
  coord_map() +
  scale_fill_distiller(name = "Average AGI", palette = "YlGn") +
  theme_nothing(legend = TRUE) +
  labs(title = "Average AGI") +
  theme(plot.title = element_text(hjust = 0.5)) 

TN_ed_map <- ggplot() + 
  geom_polygon(data = chloro_df, 
               aes(x = long, y = lat, group = group, fill = Pct_ED),
               color = "white", size = 0.25) +
  coord_map() +
  scale_fill_distiller(name = "Percent ED", palette = "YlGn", trans = "reverse") +
  theme_nothing(legend = TRUE) +
  labs(title = "% of Economically Disadvantaged Students") +
  theme(plot.title = element_text(hjust = 0.5))

TN_agi_map
TN_ed_map

## Define map properties and convert to plotly

TN_agi_chloro <- plotly::ggplotly(TN_agi_map, width = 1000, height = 400) 
TN_agi_chloro

TN_ED_chloro <- plotly::ggplotly(TN_ed_map, width = 1000, height = 400)
TN_ED_chloro

## Create a subplot

TN_maps <- subplot(TN_ED_chloro, TN_agi_chloro, nrows = 2)
TN_maps
