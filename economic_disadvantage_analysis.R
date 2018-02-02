load("data/merged_df.rds")
library(dplyr)
library(plotly)
library(maps)
library(mapdata)
library(mapproj)
library(ggmap)

## Total across zip code - removes agi ranges
zip_total <- merged_df %>% 
  filter(zip_code != 0 & is.na(agi_range))

## Filtering and summarising subject proficiency
subjects_df <- zip_total %>% 
  select(CORE_region, system_name, Pct_ED, AlgI, AlgII, Math, 
         BioI, Chemistry, Science) %>%
  group_by(CORE_region,
           system_name) %>% 
  summarise_all(funs(mean)) %>% 
  filter(!is.na(CORE_region))

## Visualising correlations
PerformanceAnalytics::chart.Correlation(subjects_df[3:9], histogram = TRUE, pch = 21)

## Looking at the data from our outliers based on LM results
## Davidson County, Fentress County, Germantown City
subjects_df[c(30, 41, 43),]

## Creating a scatter plot with regression line
ggplot(subjects_df, aes(x = BioI, y = Pct_ED)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm')

## Melting data into tidy
ed_df <- reshape2::melt(subjects_df, id = c('CORE_region', 'system_name', 'Pct_ED'))
names(ed_df) <- c("CORE_region", "system_name", "Pct_ED", "Subject", "Pct_proficient")

## Plotting new data
ED_subject_plot <- ggplot(ed_df, aes(x = Pct_ED, y = Pct_proficient, color = Subject)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = 'lm')

ED_CORE_plot <- ggplot(ed_df, aes(x = Pct_ED, y = Pct_proficient, color = CORE_region)) +
  geom_point(aes(text = paste('<br>System Name: ', system_name, '<br>Subject: ', Subject)), alpha = 0.4) +
  geom_smooth(method = 'lm')

int_ed_core_plot <- plotly::ggplotly(ED_CORE_plot)
                            
int_ed_core_plot
## Cloropleth
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

## Create map

TN_map <- ggplot() + 
  geom_polygon(data = chloro_df, 
               aes(x = long, y = lat, group = group, fill = Pct_ED),
               color = "black", size = 0.25) +
               coord_map() +
               scale_fill_distiller(name="Percent", palette = "YlGn") +
               theme_nothing(legend = TRUE) +
               labs(title = "% of Economically Disadvantaged Students")

TN_ED_chloro <- plotly::ggplotly(TN_map, width = 1000, height = 400) 
  
TN_ED_chloro

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

## Create map

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
