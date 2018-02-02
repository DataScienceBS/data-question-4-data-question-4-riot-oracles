load("data/merged_df.rds")

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


## Melting data into tidy
ed_df <- reshape2::melt(subjects_df, id = c('CORE_region', 'system_name', 'Pct_ED'))
names(ed_df) <- c("CORE_region", "system_name", "Pct_ED", "Subject", "Pct_proficient")

ed_df <- ed_df %>% 
          filter(Pct_proficient > 0, system_name != "Achievement School District",
                 Subject == "Science" | Subject == "Math")

x_prof <- list(title = "% Economically Disadvantaged")
y_prof <- list(title = "Proficiency Rate")

prof_ed_plot<- plot_ly(ed_df, y = (~Pct_proficient), x = ~Pct_ED, color = ~Subject,
                        type = "scatter",
                        mode = "markers",
                        alpha = 0.7,
                        text = ~paste('CORE Region: ', CORE_region,
                                      '<br> School System: ', system_name,
                                      '<br> Subject: ', Subject)) %>% 
  layout(xaxis = x_prof, yaxis = y_prof, legend = list(orientation = 'v'))

prof_ed_plot
