library(tidyr)
library(dplyr)
library(ggplot2)

combined_df <- readRDS("combined_df.RDS")
combined_df

#### Adding AGI Amount per Return ####

combined_df %>% 
  filter(return_c > 0) %>% 
  mutate(agi_per_return = agi_a / return_c) 


    
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
# 
########################################################## 

###################
## pairs testing ##
###################

pairs(~ CORE_region + ACT_Composite + irs_estimated_population_2014 + agi_a, data=combined_df)
