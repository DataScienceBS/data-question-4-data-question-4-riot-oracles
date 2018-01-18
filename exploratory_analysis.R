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
#ggplot(agi_grad_plot, aes(x = mean_agi, y = mean_grad, color = CORE_region)) +
#  geom_point(alpha = 0.5)

## Boxplot
# ggplot(agi_grad_plot, aes(x = CORE_region, y = mean_grad)) +
#   geom_boxplot()

## Southwest/Memphis CORE shows a wide variation in graduation rates, so exploring further
southwest_core <- by_agi_range %>%
  filter(CORE_region == "Southwest/Memphis CORE") %>% 
  mutate(avg_agi = (agi_a * 1000) / return_c) %>% 
  group_by(zip_code) %>% 
  summarise(mean_agi = mean(avg_agi),
            mean_grad = mean(Graduation))

# ggplot(southwest_core, aes(x = mean_grad, y = mean_agi)) +
#   geom_point(alpha = 0.5)

## Math vs Science
courses <- c("AlgI", "AlgII", "Biol", "Chemistry", "ELA", "EngI",
             "EngII", "EngIII", "Math", "Science")

overall_score <- by_agi_range %>% 
  filter(!is.na(AlgI |  AlgII |  BioI |  Chemistry |  ELA |  EngI | 
                  EngII |  EngIII |  Math |  Science)) %>% 
  group_by(zip_code) %>%
  summarise(mean_chr_absent = mean(Pct_Chronically_Absent),
            mean_algI = mean(AlgI),
            mean_algII = mean(AlgII),
            mean_math = mean(Math),
            mean_bioI = mean(BioI),
            mean_chemistry = mean(Chemistry),
            mean_science = mean(Science),
            mean_ela = mean(ELA),
            mean_engI = mean(EngI),
            mean_engII = mean(EngII),
            mean_engIII = mean(EngIII)
            )


score_plot <- function(df, score1, score2)
  ggplot(df, aes(x = score1, y = score2)) +
  geom_point(alpha = 0.5)

mean_algI <- score_plot(overall_score, mean_algI, mean_chr_absent)
mean_algII <- score_plot(overall_score, mean_algII, mean_chr_absent)
mean_math <- score_plot(overall_score, mean_math, mean_chr_absent)
mean_bioI <- score_plot(overall_score, mean_bioI, mean_chr_absent)
mean_chemistry <- score_plot(overall_score, mean_chemistry, mean_chr_absent)
mean_science <- score_plot(overall_score, mean_science, mean_chr_absent)
mean_ela <- score_plot(overall_score, mean_ela, mean_chr_absent)
mean_engI <- score_plot(overall_score, mean_engI, mean_chr_absent)
mean_engII <- score_plot(overall_score, mean_engII, mean_chr_absent)
mean_engIII <- score_plot(overall_score, mean_engIII, mean_chr_absent)

grid