# correlation between Dropout and ACT Composite is -0.336, removing 1 outlier ACT<15 adjusts to -0.243
# correlation between Dropout and Per Pupil Expenditures is 0.274
# correlation between Pct_ED (Econ Disadv) and Per Pupil Expenditures is 0.157


library(tidyr)
library(dplyr)
library(ggplot2)

combined_df <- readRDS("combined_df.RDS")
merged_df <- readRDS("merged_df.RDS")
school_cross <- readRDS("school_cross.RDS")

#### Adding AGI Amount per Return ####

combined_df %<>% 
  filter(return_c > 0) %>% 
  mutate(agi_per_return = agi_a / return_c)  
## consider multiplying by 1k ##

    
##################################
#    starting plot potential     #
##################################


combined_df %>%
  filter(return_c > 0) %>% 
  mutate(agi_per_return = (agi_a / return_c)) %>% 
  group_by(county, agi_range, agi_per_return) %>% 
  summarise(avg_act = mean(ACT_Composite),
            avg_agi = mean(agi_per_return)) %>% 
  ggplot(., aes( x=avg_act, y=avg_agi, color=agi_range)) +
  geom_point()


combined_df %>% 
  filter(zip_code != 0 | zip_code != 99999) %>% 
  filter(agi_range != 'Total') %>% 
  group_by(agi_range) %>% 
  ggplot(., aes(x=agi_range, y=agi_a)) +
  geom_point()


#### boxplot for ACT scores as a function of CORE Region ####
combined_df %>% 
  filter(zip_code != 0 | zip_code != 99999) %>% 
  filter(agi_range != 'Total') %>% 
  ggplot(., aes(x=CORE_region, y=ACT_Composite)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


###########################
#### mapping Tennessee ####
###########################
# help from: http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
###########################
###      data prep      ###
###########################

library(maps)
library(mapdata)  #preloaded state/county coordinates

TN_data <- map_data("state") %>% 
  filter(region =='tennessee')

TN_counties <- map_data("county") %>% subset(., region == "tennessee")


##########################################
# converting tax$county to lcase for map #
##########################################
combined_df$county_l <- sapply(combined_df$county, tolower) %>% gsub(' county','',.)
tax_school_map_df <- left_join(combined_df, TN_counties, by=c('county_l'='subregion')) 
##########################################

TN_map <- ggplot(data = TN_data, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +  # prevents axis skew
  geom_polygon(data = TN_counties, fill = "orange", color = "white") +  #county fill and borders
  geom_polygon(color = "black", fill = NA) #state outline and fill

### need to remove plot background ###

TN_map

##################################
###   Top 10 counties by pop   ###
##################################
top_10_pop <- zip_codes %>%  
  group_by(county) %>% 
  summarize(sum_pop = sum(irs_estimated_population_2014, na.rm = TRUE)) %>% 
  filter(sum_pop > 100000) %>% 
  arrange(desc(sum_pop)) %>% 
  head(n=10)  

top_10_pop  


##########################################################
#  join tax/count_l before mapping,  fill counties by:  
# 
# 1. Group By county, sum/avg with enrollment weighted
# 2. avg. AGI amt per return filed
# 3. ACT composite scores
# 4. % of returns in AGI Range (lowest, highest
# 5. dropout rate
# 6. graduation rate 
# 7. total Math / Science 
# 8. % non-white
# 9. find correlations, colorfill Top/Bottom 10
#   9a. percentage of AGI Group count per county => Top 10, corr w/ graduation rate 
#   9b. percentage of AGI group, Top 10/Bottom 10, corr w/  ACT composite 
#   9c. 3-dimensional heat map, x = Percentage, y = AGI Group, z = Graduation Rate | ACT Comp
# 
########################################################## 

###################
## pairs testing ##
###################

pairs(~ agi_per_return + ACT_Composite + ratio_by_agi + Dropout, data=combined_df)

# scatter with RATIO_BY_AGI and ACT score #
combined_df %>% 
  filter(zip_code != 0 | zip_code != 99999) %>% 
  filter(agi_range != 'Total') %>% 
  #  group_by(agi_range) %>% 
  ggplot(., aes(x=ratio_by_agi, y=ACT_Composite, color=CORE_region)) +
  geom_point() +
  facet_grid(. ~ agi_range)
# the county ratio by agi group does not visually appear to have correlation with ACT scores.

# scatter with RATIO_BY_AGI and DROPOUT #
combined_df %>% 
  filter(zip_code != 0 | zip_code != 99999) %>% 
  filter(agi_range != 'Total') %>% 
  #  group_by(agi_range) %>% 
  ggplot(., aes(x=ratio_by_agi, y=Dropout, color=CORE_region)) +
  geom_point() +
  facet_grid(. ~ agi_range)
# the county ratio by agi group does not visually appear to have correlation with ACT scores.


# scatter with ACT and Dropout, facet by CORE, color by AGI_Range
combined_df %>% 
  filter(zip_code != 0 | zip_code != 99999) %>% 
  filter(agi_range != 'Total') %>% 
  filter(CORE_region != 'NA') %>% 
  #  group_by(agi_range) %>% 
  ggplot(., aes(x=ACT_Composite, y=Dropout, color=agi_range)) +
  geom_point() +
  facet_grid(. ~ CORE_region)


combined_df %>% 
  filter(zip_code != 0 | zip_code != 99999) %>% 
  filter(agi_range != 'Total') %>% 
  filter(CORE_region != 'NA') %>% 
  #  group_by(agi_range) %>% 
  ggplot(., aes(x=ACT_Composite, y=Dropout, color=agi_range)) +
  geom_point()



# -0.546 correlation between ACT Composite and Dropout rate #
## 95% confidence interval:  -0.564, -0.527 ##
### moderate negative linear correlation between ACT Composite score and the Dropout rate ###
#### Plot of ACL vs. Dropout ####

school %>% 
  filter(ACT_Composite > 15) %>% 
  ggplot(., aes(x = ACT_Composite, y = Dropout)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) +
  ylim(-5,35)

corr_testing <- school %>% 
  filter(Dropout != 'NA') %>% 
  filter(ACT_Composite > 15)

cor.test(corr_testing$Dropout, corr_testing$ACT_Composite, method = "pearson")
cor.test(school$Dropout, school$ACT_Composite, method = "pearson")
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

# first iteration of model #
model <- lm(formula = ACT_Composite ~ AlgI + AlgII + Math + Science + BioI + Chemistry + ELA + EngI + EngII + EngIII, data = school_cross)
plot(model)
summary(model)

# tweaking model for best features #
model_math_ela_algII_chem <- lm(formula = ACT_Composite ~ AlgII + Math + Chemistry + ELA, data = school_cross)
summary(model_math_ela_algII_chem)
plot(model_math_ela_algII_chem)

# working to identify outliers from the plots #
school_cross[c(18,30,40),]


# correlate ACT with non-academic features
cor.test(school_cross$ACT_Composite, school_cross$Graduation, method = 'pearson')             # 0.357
cor.test(school_cross$ACT_Composite, school_cross$Pct_Chronically_Absent, method = 'pearson') # -0.392
cor.test(school_cross$ACT_Composite, school_cross$Per_Pupil_Expenditures, method = 'pearson') # -0.083

cor.test(school_cross$ACT_Composite, school_cross$Pct_BHN, method = 'pearson')        # -0.21
cor.test(school_cross$ACT_Composite, school_cross$Pct_Hispanic, method = 'pearson')   # 0.073
cor.test(school_cross$ACT_Composite, school_cross$Pct_Black, method = 'pearson')      # -0.251
cor.test(school_cross$ACT_Composite, school_cross$Pct_Native_American, method = 'pearson') # 0.257

