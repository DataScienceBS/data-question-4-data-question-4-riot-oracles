library(dplyr)

load("data/TN_taxes_2015.rds")
load("data/zip_df.rds")

## Joining tax data and zip data into single dataframe: combined_df
combined_df <- left_join(x = TN_taxes_2015, y = zip_df, by = c("zip_code" = "zip"))
length(unique(combined_df$zip_code))

## Bring in school data
school_df <- read.csv("data/school_df.csv")

county_school_df <- school_df %>% 
  filter(!grepl("Special", system_name), !grepl("Deaf", system_name), !grepl("Blind", system_name))

special_school_df <- school_df %>% 
  filter(grepl("Special", system_name) | grepl("Deaf", system_name) | grepl("Blind", system_name))

## Bring in county reference data
county_ref_df <- readxl::read_xls("data/county_ref_data.xls")

## Join school and county reference data
combined_df_2 <- left_join(x = county_school_df, y = county_ref_df, by = c("system" = "District Number"))

## Joining tax/zip data to school data
merged_df <- left_join(x = combined_df, y = combined_df_2, by = c("county" = "County Name"))

## Unmatched data from prior join
unmatched_df <- anti_join(x = combined_df, y = combined_df_2, by = c("county" = 'County Name'), all = TRUE)

## Output to csv and rds
write.csv(merged_df, file = 'data/merged_df.csv')
save(merged_df, file = "data/merged_df.rds")
write.csv(unmatched_df, file = 'data/unmatched_df.csv')
save(unmatched_df, file = "data/unmatched_df.rds")
