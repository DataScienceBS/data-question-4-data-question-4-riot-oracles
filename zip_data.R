library(dplyr)

## Importing zip_code_database for cleanup

zip_df <- readxl::read_xlsx("data/zip_code_database.xlsx")

## Filter by state

by_zip_df <- zip_df %>% 
  filter(state == "TN")

## Define columns to keep, then save as a new df

keep <- c("county", "zip", "type", "primary_city", "state", "latitude", "longitude", "irs_estimated_population_2014")
zip_df <- by_zip_df[keep]
head(by_zip_df)
str(by_zip_df)

## Save df as .rds
save(zip_df, file = "data/zip_df.rds")
