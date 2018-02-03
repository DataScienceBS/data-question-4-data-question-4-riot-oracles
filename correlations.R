# SUCCESS! Removed DeKalb County from training data and was able to accurately predict ACT Composite Score using ACME (AlgII, Chem, Math, ELA)
# correlation between Dropout and ACT Composite is -0.336, removing 1 outlier ACT<15 adjusts to -0.243
# correlation between Dropout and Per Pupil Expenditures is 0.274
# correlation between Pct_ED (Econ Disadv) and Per Pupil Expenditures is 0.157

library(tidyr)
library(dplyr)
library(ggplot2)

combined_df <- readRDS("combined_df.RDS")
merged_df <- readRDS("merged_df.RDS")
school_cross <- readRDS("school_cross.RDS")

# -0.546 correlation between ACT Composite and Dropout rate #
## 95% confidence interval:  -0.564, -0.527 ##
### moderate negative linear correlation between ACT Composite score and the Dropout rate ###
#### Plot of ACL vs. Dropout ####

school_cross %>% 
  filter(ACT_Composite > 15) %>% 
  ggplot(., aes(x = ACT_Composite, y = Dropout)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) +
  ylim(-5,35)

corr_testing <- school_cross %>% 
  filter(Dropout != 'NA') %>% 
  filter(ACT_Composite > 15)

cor.test(corr_testing$Dropout, corr_testing$ACT_Composite, method = "pearson")
cor.test(school_cross$Dropout, school_cross$ACT_Composite, method = "pearson")
# correlation between Dropout and ACT Composite is -0.336, removing 1 outlier ACT<15 adjusts to -0.243
  
##############################
#   begin analysis on farms  #
##############################

farm_by_county <- merged_df %>% 
  select(county, zip_code, agi_range, return_c, farm_c) %>% 
  group_by(county, agi_range) %>% 
  summarise(filed_per_agi = sum(return_c),
            farm_per_agi = sum(farm_c)) 

  farm_by_county %<>%
    group_by(county) %>% 
    mutate(filed_per_county = sum(filed_per_agi),
           farm_per_county = sum(farm_per_agi),
           pct_farm = round((farm_per_county/filed_per_county)*100,1)) %>% 
    select(county,pct_farm) %>% 
    distinct(.)

school_cross$county <- school_cross$`County Name`
school_farm <- merge(x = school_cross, y = farm_by_county, by='county', all.x=TRUE)

cor.test(school_farm$pct_farm, school_farm$Dropout, method = 'pearson')    #  -0.113
cor.test(school_farm$pct_farm, school_farm$ACT_Composite, method = 'pearson')   # -0.241
cor.test(school_farm$pct_farm, school_farm$Pct_ED, method = 'pearson') #  0.229
cor.test(school_farm$pct_farm, school_farm$Pct_Suspended, method = 'pearson') #  -0.353 (there is a negative correlation between Farm/Susp)
cor.test(school_farm$pct_farm, school_farm$Pct_BHN, method = 'pearson') # -0.380
cor.test(school_farm$pct_farm, school_farm$Graduation, method = 'pearson') #0.151
cor.test(school_farm$pct_farm, school_farm$Enrollment, method = 'pearson') # -0.111

cor.test(school_farm$pct_farm, school_farm$Pct_Suspended, method = 'pearson')

farm_map <- school_farm %>% 
  filter(CORE_region != 'NA') %>% 
  filter(Pct_Suspended != 0) %>% 
  ggplot(., aes( x=Pct_Suspended, y=pct_farm, color=County_Name)) +
  geom_point() 

farm_map <-  farm_map + facet_grid(. ~ CORE_region)

farm_map <- ggplotly(farm_map, width=1500, height=500)
farm_map

saveRDS(farm_map, file="farm_map.RDS")


######################################
##   begin ACT Composite analysis   ##
######################################
pairs(~ ACT_Composite + AlgI + AlgII + Math, data=school_farm) # Math
pairs(~ ACT_Composite + BioI + Chemistry + Science, data=school_farm) # Sciences

# correlate ACT with subjects
cor.test(school_cross$ACT_Composite, school_cross$AlgI, method = 'pearson')    # 0.436
cor.test(school_cross$ACT_Composite, school_cross$AlgII, method = 'pearson')   # 0.607
cor.test(school_cross$ACT_Composite, school_cross$Math, method = 'pearson')    # 0.660
cor.test(school_cross$ACT_Composite, school_cross$Science, method = 'pearson') # 0.715
cor.test(school_cross$ACT_Composite, school_cross$BioI, method = 'pearson')    # 0.678
cor.test(school_cross$ACT_Composite, school_cross$Chemistry, method = 'pearson') # 0.678
cor.test(school_cross$ACT_Composite, school_cross$ELA, method = 'pearson')    # 0.787
cor.test(school_cross$ACT_Composite, school_cross$EngI, method = 'pearson')   # 0.739
cor.test(school_cross$ACT_Composite, school_cross$EngII, method = 'pearson')  # 0.734
cor.test(school_cross$ACT_Composite, school_cross$EngIII, method = 'pearson') # 0.605

# correlate ACT with non-academic features
cor.test(school_cross$ACT_Composite, school_cross$Graduation, method = 'pearson')             # 0.357
cor.test(school_cross$ACT_Composite, school_cross$Pct_Chronically_Absent, method = 'pearson') # -0.392
cor.test(school_cross$ACT_Composite, school_cross$Per_Pupil_Expenditures, method = 'pearson') # -0.083

cor.test(school_cross$ACT_Composite, school_cross$Pct_BHN, method = 'pearson')        # -0.21
cor.test(school_cross$ACT_Composite, school_cross$Pct_Hispanic, method = 'pearson')   # 0.073
cor.test(school_cross$ACT_Composite, school_cross$Pct_Black, method = 'pearson')      # -0.251
cor.test(school_cross$ACT_Composite, school_cross$Pct_Native_American, method = 'pearson') # 0.257


##############################################
#     function to remove rows, for outliers  #
#         and for prediction purposes.       #
#     Don't want test data in training data  #
##############################################
remove_rows <- function(x, rows) x[-rows,, drop = FALSE]
school_cross_no_dekalb <- remove_rows(school_cross,37)

# first iteration of model (model_full) #
model_full <- lm(formula = ACT_Composite ~ AlgI + AlgII + Math + Science + BioI + Chemistry + ELA + EngI + EngII + EngIII, data = school_cross_no_dekalb)
plot(model_full)
summary(model_full)

# tweaking model for best features (model_acme) #  ACME = AlgII, Chem, Math, ELA   #
model_acme <- lm(formula = ACT_Composite ~ AlgII + Math + Chemistry + ELA, data = school_cross_no_dekalb)
summary(model_acme)
plot(model_acme)

# plots suggest 3 main outliers: 18, 30, 40. Removing and creating new models #
remove_rows <- function(x, rows) x[-rows,, drop = FALSE]
school_cross_no_dekalb_no_outliers <- remove_rows(school_cross_no_dekalb, c(18,30,40))

### redo model_full and model_acme without 3 outliers ###
model_full_2 <- lm(formula = ACT_Composite ~ AlgI + AlgII + Math + Science + BioI + Chemistry + ELA + EngI + EngII + EngIII, data = school_cross_no_dekalb_no_outliers)
plot(model_full_2)
summary(model_full_2)

model_acme_2 <- lm(formula = ACT_Composite ~ AlgII + Math + Chemistry + ELA, data = school_cross_no_dekalb_no_outliers)
summary(model_acme_2)
plot(model_acme_2)


#########################################################################   
######  testing prediction data based on 4 proficiency categories  ######
######            ACME: AlgII, Chemistry, Math, and ELA            ######
#########################################################################
dekalb_acme <- data.frame(AlgII = 55.6, Chemistry = 53.9, Math = 54.4, ELA = 45.7)
dekalb_full <- data.frame(AlgI = 52.9, AlgII = 55.6, Math = 54.4, Science = 65.9, BioI = 72.1, Chemistry = 53.9, ELA = 45.7, EngI = 73.4, EngII = 72.0, EngIII = 31.0)
  
predict(model_acme, dekalb_acme)
predict(model_full,dekalb_full)
#########################################################################   
######  using DeKalb County, ACME model estimates 19.1 ACT Score   ######
######  using DeKalb County, full model estimates 19.4 ACT Score   ######
######         Actual DeKalb County ACT Score was 19.1             ######
#########################################################################
######            removing 3 outliers from the data...             ######
#########################################################################
predict(model_full_2,dekalb_full)
predict(model_acme_2,dekalb_full)
school_cross_no_dekalb_no_outliers
##########################################################################
#####     Predicting with 3 outliers removed, per model summary:     #####
#####  using DeKalb County, ACME_model_2 estimates 19.07 ACT Score   #####
#####  using DeKalb County, full_model_2 estimates 19.22 ACT Score   #####
####           Actual DeKalb County ACT Score was 19.1               #####
##########################################################################
#####   SUCCSES! in predicting ACT Composite based on proficiency    #####
##########################################################################


#### correlation mapping with PerformanceAnalytics package ####
library(PerformanceAnalytics)
my_data <- school_cross[, c('ACT_Composite', 'AlgII', 'Math', 'Chemistry', 'ELA')]
chart.Correlation(my_data, histogram=TRUE, pch=21)
