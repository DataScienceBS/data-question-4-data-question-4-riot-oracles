library(tidyr)
library(dplyr)

####################################
#  read 2015 Taxes for Tennessee   # 
# skip non essential header/footer #
####################################

df <- readxl::read_xls("data/TN_taxes_2015.xls", range = "A6:DY4725")

#### naming columns, using drop prefixes for pre-defined non-usable data to easily drop later ####
names(df) <- c('zip_code', 'agi_range', 'return_c', 'drop1', 'drop2', 'drop3', 'paid_prep', 'exempt_c', 'depend_c', 'drop4', 'drop5', 'drop6', 'drop7', 'drop8', 'drop9', 'drop10', 'agi_a', 'drop11', 'drop12', 'wage_c', 'wage_a', 'tax_int_c', 'tax_int_a', 'div_c', 'div_a', 'drop13', 'drop14', 'drop15', 'drop16', 'biz_inc_c', 'biz_inc_a', 'cap_gain_c', 'cap_gain_a', 'ira_c', 'ira_a', 'pension_c', 'pension_a', 'farm_c', 'unemp_comp_c', 'unemp_comp_a', 'ss_ben_c', 'ss_ben_a', 'drop17', 'drop18', 'drop19', 'drop20', 'drop21', 'drop22', 'drop23', 'drop24', 'drop25', 'drop26', 'drop27', 'drop28', 'drop29', 'drop30', 'drop31', 'drop32', 'drop33', 'drop34', 'deductions_c', 'deductions_a', 'drop35', 'state_inc_tax_c', 'state_inc_tax_a', 'sales_tax_c', 'sales_tax_a', 'prop_tax_c', 'prop_tax_a', 'drop36', 'drop37', 'mortgage_c', 'mortgage_a', 'contrib_c', 'contrib_a', 'taxable_c', 'taxable_a', 'drop38', 'drop39', 'drop40', 'drop41', 'drop42', 'drop43', 'credits_c', 'credits_a', 'drop44', 'drop45', 'drop46', 'drop47', 'drop48', 'drop49', 'drop50', 'drop51', 'drop52', 'drop53', 'drop54', 'drop55', 'drop56', 'drop57', 'drop58', 'drop59', 'drop60', 'drop61', 'drop62', 'drop63', 'drop64', 'drop65', 'eic_c', 'eic_a', 'excess_eic_c', 'excess_eic_a', 'drop66', 'drop67', 'drop68', 'drop69', 'drop70', 'drop71', 'drop72', 'drop73', 'tax_liab_c', 'tax_liab_a', 'drop74', 'drop75', 'drop76', 'drop77', 'tax_due_c', 'tax_due_a', 'refund_c', 'refund_a')
df <- subset(df, select = -c(drop1, drop2, drop3, drop4, drop5, drop6, drop7, drop8, drop9, drop10, drop11, drop12, drop13, drop14, drop15, drop16, drop17, drop18, drop19, drop20, drop21, drop22, drop23, drop24, drop25, drop26, drop27, drop28, drop29, drop30, drop31, drop32, drop33, drop34, drop35, drop36, drop37, drop38, drop39, drop40, drop41, drop42, drop43, drop44, drop45, drop46, drop47, drop48, drop49, drop50, drop51, drop52, drop53, drop54, drop55, drop56, drop57, drop58, drop59, drop60, drop61, drop62, drop63, drop64, drop65, drop66, drop67, drop68, drop69, drop70, drop71, drop72, drop73, drop74, drop75, drop76, drop77))

#### removing rows with no values ####
df <- df %>% 
  drop_na()

#### renaming working df for sensibility ####
TN_tax_2015 <- df

##################################
#    loading TN zip code data    #
# to add county name to tax data #
##################################

zip_codes <- readxl::read_excel("data/zip_code_database.xlsx") %>% 
  filter(state == "TN")

merged_df <- left_join(TN_tax_2015, zip_codes, by=c('zip_code' = 'zip'))

#### adding feature: total returns filed by county ####
filed_by_county <- merged_df %>% 
  select(county, zip_code, agi_range, return_c) %>% 
  group_by(county, agi_range) %>% 
  summarise(filed_per_agi = sum(return_c)) 

filed_by_county %<>%
  group_by(county) %>% 
  mutate(filed_per_county = sum(filed_per_agi))

filed_by_county %<>%
  group_by(county) %>% 
  mutate(ratio_by_agi = round((filed_per_agi/filed_per_county)*100,1))

merged_df <- left_join(merged_df, filed_by_county, by = c('county' = 'county', 'agi_range' = 'agi_range'))

View(filed_by_county)

################################
#     loading school data      #
#    loading zip crosswalk     #
# using crosswalk for tax join #
################################

school <- read.csv('data/achievement_profile_data_with_CORE.csv')
zip_cross <- readxl::read_excel("data/county_ref_data.xls")
school_cross <- left_join(school, zip_cross, by=c('system' = 'District Number'))

##################################
#  joining tax data with school  #
##################################

combined_df <- left_join(merged_df, school_cross, by=c('county'='County Name'))

saveRDS(merged_df, file="merged_df.RDS")
saveRDS(school_cross, file="school_cross.RDS")
saveRDS(combined_df, file="combined_df.RDS")

