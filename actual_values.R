load("data/merged_df.rds")

## Removing na values
merged_df <- merged_df %>% 
  filter(!is.na(zip_code) & !is.na(agi_range))

## Defining function to apply to our 'amount' values
real_val <- function(x) {
  x * 1000
}

## Creating new df for 'amount' values that need to be converted to actual values
calculated_df <- merged_df %>%
  select(grep("_a", colnames(merged_df)))

keep_df <- merged_df %>%
  select(grep("_a", colnames(merged_df))) 

keep <- list(colnames(keep_df))
head(keep)

## Apply function to each element in the df
calculated_df[] <- data.frame(apply(calculated_df, 2, real_val))