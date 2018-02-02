library(ggplot2)
library(dplyr)

merged_df <- load("data/merged_df.rds")

## Filtering df to show the agi range for each zip code
by_agi_range <- merged_df %>% 
  filter(zip_code != 0 & !is.na(agi_range))

## Total across zip code - removes agi ranges
zip_total <- merged_df %>% 
  filter(zip_code != 0 & is.na(agi_range))

## Filtering df to show the agi range for each zip code
bhn_grad <- merged_df %>% 
  filter(zip_code != 0 & !is.na(Pct_BHN)) %>% 
  group_by(zip_code) %>% 
  summarise(avg_BHN = mean(Pct_BHN),
            avg_Graduation = mean(Graduation))

ggplot(bhn_grad, aes(x = avg_BHN, y = avg_Graduation)) +
  geom_point(alpha = .3)

## Exploring the relationship between % of minority students with other variables:
bhn_agi_plot <- zip_total %>% 
  filter(agi_a > 0) %>% 
  mutate(avg_agi = (agi_a * 1000) / return_c,
         avg_unemp = (unemp_comp_c / return_c)) %>% 
  group_by(system_name) %>% 
  summarise_all(funs(mean))

## Code for exploratory analysis
pairs(~AlgI + AlgII + Math + BioI + Chemistry + Science + Pct_ED, data = bhn_agi_plot)

bhn_lm <- lm(Pct_ED ~ BioI, data = subjects_df, na.action = na.exclude)
plot(bhn_lm)

ggplot(bhn_agi_plot, aes(x = avg_agi, y = avg_unemp)) +
  geom_point(alpha = 0.3)

cor.test(bhn_agi_plot$AlgI, bhn_agi_plot$Pct_ED, method = "pearson")

## Correlation scores for Pct_BHN with:
## Pct_Suspended = 0.6944
## Graduation = -0.3064
## Pct_Chronically_Absent = 0.0245
## avg_agi = 0.3505
## ACT_Composite = -0.2214
## Dropout = 0.2709
## Math = -0.2098
## Science = -0.3804
## Pct_ED = 0.2343

## Correlations with Pct_ED:
## avg_unemp = -0.4314
## 

subjects_df <- zip_total %>% 
  select(system_name, Pct_ED, AlgI, AlgII, Math, 
         BioI, Chemistry, Science) %>%
  group_by(system_name) %>% 
  summarise_all(funs(mean))

PerformanceAnalytics::chart.Correlation(subjects_df[2:8], histogram = TRUE, pch = 19)

ggplot(subjects_df, aes(x = BioI, y = Pct_ED)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method='lm')


## Avg by_agi_range to show mean AGI and graduation rates by zip, including CORE region
agi_grad_df <- merged_df %>% 
  filter(is.na(agi_range) & !is.na(zip_code)) %>% 
  mutate(avg_agi = (agi_a * 1000) / return_c) %>% 
  group_by(zip_code) %>% 
  summarise(mean_agi = mean(avg_agi),
            mean_grad = mean(Graduation))

 core_zip<- merged_df %>% 
  filter(is.na(agi_range) & !is.na(zip_code)) %>% 
   select(zip_code, CORE_region, system_name)

agi_grad_plot <- agi_grad_df %>% 
  left_join(core_zip[c("zip_code", "CORE_region", "system_name")], by = "zip_code") %>% 
  filter(!is.na(CORE_region))

## Boxplot
x_grad <- list(title = "", showticklabels = FALSE)
y_grad <- list(title = "Graduation Rate")
  
grad_box_plot<- plot_ly(agi_grad_plot, y = (~mean_grad), x = ~CORE_region, color = ~CORE_region,
                    type = "scatter",
                    mode = "markers",
                    alpha = 0.3,
                    text = ~paste('Zip Code: ', zip_code,
                                  '<br> System Name: ', system_name)) %>% 
                    layout(xaxis = x_grad, yaxis = y_grad, legend = list(orientation = 'h')) %>% 
                    add_trace(agi_grad_plot, y = ~mean_grad, color = ~CORE_region, type = "box")

grad_box_plot


## Subject plot

ed_df <- reshape2::melt(subjects_df, id = c('CORE_region', 'system_name', 'Pct_ED'))
names(ed_df) <- c("CORE_region", "system_name", "Pct_ED", "Subject", "Pct_proficient")
  