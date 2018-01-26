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

TN_map


############ testing other map options ############
# color_df <- school_cross %>%
#   select(CORE_region, County_Name, Enrollment, Per_Pupil_Expenditures, ACT_Composite) %>%
#   group_by(County_Name) %>%
#   summarise(Average_ACT_Composite = mean(ACT_Composite, na.rm = TRUE),
#             Total_Per_Pupil_Expenditure = sum(Per_Pupil_Expenditures, na.rm = TRUE),
#             Total_Enrollment  = sum(Enrollment, na.rm = TRUE),
#             Avg_Per_Pupil_Exp = Total_Enrollment/Total_Per_Pupil_Expenditure)
# 
# color_df <- left_join(x = TN_counties, y = color_df, by = c("subregion" = "county_l"))
# 
# TN_map <- ggplot() +
#   geom_polygon(data = color_df,
#                aes(x = long.x, y = lat.x, group = group.x, fill = # need fill from Pupils #),
#                color = "white", size = 0.25) +
#   coord_map() +
#   scale_fill_distiller(name="Percent", palette = "YlGn") +
# #  theme_nothing(legend = TRUE) +
#   labs(title = "Per Puil Expenditures by County")
############ testing other map options ############







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


