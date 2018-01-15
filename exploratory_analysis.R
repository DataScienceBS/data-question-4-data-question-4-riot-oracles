library(ggplot2)
library(dplyr)

load("data/merged_df.rds")

## Filtering df to show the agi range for each zip code
by_agi_range <- merged_df %>% 
  filter(zip_code != 0 & !is.na(agi_range))

## Avg by_agi_range to show mean AGI and graduation rates by zip, including CORE region
agi_grad_plot <- by_agi_range %>% 
  filter(agi_a > 0,
         !is.na(Graduation)) %>% 
  mutate(avg_agi = (agi_a * 1000) / return_c) %>% 
  group_by(zip_code) %>% 
  summarise(mean_agi = mean(avg_agi),
            mean_grad = mean(Graduation)) %>% 
  left_join(by_agi_range[c("zip_code", "CORE_region")], by = "zip_code") %>% 
  filter(!is.na(CORE_region))

## Scatter
ggplot(agi_grad_plot, aes(x = mean_agi, y = mean_grad, color = CORE_region)) +
  geom_point(alpha = 0.5)

## Boxplot
ggplot(agi_grad_plot, aes(x = CORE_region, y = mean_grad)) +
  geom_boxplot()

## Southwest/Memphis CORE shows a wide variation in graduation rates, so exploring further
southwest_core <- by_agi_range %>% 
  filter(CORE_region == "Southwest/Memphis CORE") %>% 
  mutate(avg_agi = (agi_a * 1000) / return_c) %>% 
  group_by(zip_code) %>% 
  summarise(mean_agi = mean(avg_agi),
            mean_grad = mean(Graduation))

ggplot(southwest_core, aes(x = mean_grad, y = mean_agi)) +
  geom_point(alpha = 0.5)
