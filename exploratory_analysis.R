library(ggplot2)
library(dplyr)

load("data/merged_df.rds")

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

# ## Function that doesn't work, sadly.
# score_plot <- function(df, score1, score2) {
#   ggplot(df, aes(x = score1, y = score2)) +
#   geom_point(alpha = 0.5)
# }
# 
# ## Function calls that failed
# mean_algI <- score_plot(overall_score, mean_algI, mean_chr_absent)
# mean_algII <- score_plot(overall_score, mean_algII, mean_chr_absent)
# mean_math <- score_plot(overall_score, mean_math, mean_chr_absent)
# mean_bioI <- score_plot(overall_score, mean_bioI, mean_chr_absent)
# mean_chemistry <- score_plot(overall_score, mean_chemistry, mean_chr_absent)
# mean_science <- score_plot(overall_score, mean_science, mean_chr_absent)
# mean_ela <- score_plot(overall_score, mean_ela, mean_chr_absent)
# mean_engI <- score_plot(overall_score, mean_engI, mean_chr_absent)
# mean_engII <- score_plot(overall_score, mean_engII, mean_chr_absent)
# mean_engIII <- score_plot(overall_score, mean_engIII, mean_chr_absent)
# 
# ggplot(overall_score, aes(x = mean_algI, y = mean_chr_absent)) +
#   geom_point(alpha = 0.5)
# ggplot(overall_score, aes(x = mean_algII, y = mean_chr_absent)) +
#   geom_point(alpha = 0.5)
# ggplot(overall_score, aes(x = mean_math, y = mean_chr_absent)) +
#   geom_point(alpha = 0.5)


