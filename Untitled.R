# set the directory
setwd("/Users/smitaupadhyay/Desktop/data_science01/repogit/data-question-4-data-question-4-riot-oracles")
#download the packages
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(gapminder)
library(ggplot2)
library(corrplot)
library(GGally)
library(ggcorrplot)
library(PerformanceAnalytics)
library(MASS)
#read the garde gender county etc dat for school
school_mem2015 <- read_csv("data/data_2015_membership_school.csv")
#county crosswalk dat to get the county names
crosswalk <- read_xls("data/county_crosswalk.xls")


TN_data <- map_data("state") %>% 
  filter(region =='tennessee')

TN_counties <- map_data("county") %>% subset(., region == "tennessee")