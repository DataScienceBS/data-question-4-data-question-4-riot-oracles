# set the directory
setwd("/Users/smitaupadhyay/Desktop/data_science01/repogit/data-question-4-data-question-4-riot-oracles")
#download the packages
library(readxl)
library(tidyr)
library(tidyverse)
library(dplyr)
library(gapminder)
library(ggplot2)

#read the data from the dowloads Airs is the irs_2015 file with the column names changed as per the read.me
irs_2015A <- read_excel("data/Airs_2015.xls")
head(irs_2015A)
View(irs_2015A)

#droping the columns by column number which are not mentionedin read.me
#replacing all the NA with 0

irs_2015B <- irs_2015A[, -c(4:7,10:16,18,19,26:29,43:60,78:105,112:119,122:125)]
View(irs_2015B)

#drop the rows 1-3
irs_2015<- irs_2015B[-c(0:3), ]

#use the row 1 as the col names
colnames(irs_2015) = irs_2015[1, ]

#drop the row 1 and 2 after new header is created
irs_2015 <- irs_2015[-c(1:2), ]
View(irs_2015)

#drop all the rows where all the col are NA
irs_2015 <- irs_2015%>%
  drop_na()
#replacing all the na with 0
irs_2015[is.na(irs_2015)]<- 0
View(irs_2015)

# find how many zip codes are there
unique(irs_2015['Zip code'])

#there are 590 unique Zip codes each with 6 categories of tax bracket 
#row 1 is total of subcategories
#Zip 0 is total and Zip 99999 is other so there are 588 zipcodes

max_agi_row <- irs_2015 %>%
  filter(`AGI amount` == max(irs_2015$`AGI amount`))
irsA <- read_excel("data/irs_2015.xls")

tax2015 <- readxl::read_xls("data/tax2015.xls", range = "A6:DY4725")

