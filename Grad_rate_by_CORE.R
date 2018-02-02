## Avg by_agi_range to show mean AGI and graduation rates by zip, including CORE region
merged_df <- readRDS("data/merged_df.rds")
View(merged_df)
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