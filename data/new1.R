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

#read the data from the dowloads had to redo as editing the column names in xcel  converted the 
#numeric values to character values

tax2015 <- readxl::read_xls("data/tax2015.xls", range = "A6:DY4725")

#naming the columns in tax2015 using Brandon's style
#### naming columns, using drop prefixes for pre-defined non-usable data to easily drop later ####

names(tax2015) <- c('zip_code', 'agi_range', 'return_c', 'drop1', 'drop2', 'drop3', 'paid_prep', 
                    'exempt_c', 'depend_c', 'drop4', 'drop5', 'drop6', 'drop7', 'drop8', 'drop9', 
                    'drop10', 'agi_a', 'drop11', 'drop12', 'wage_c', 'wage_a', 'tax_int_c', 
                    'tax_int_a', 'div_c', 'div_a', 'drop13', 'drop14', 'drop15', 'drop16', 
                    'biz_inc_c', 'biz_inc_a', 'cap_gain_c', 'cap_gain_a', 'ira_c', 'ira_a', 
                    'pension_c', 'pension_a', 'farm_c', 'unemp_comp_c', 'unemp_comp_a', 'ss_ben_c',
                    'ss_ben_a', 'drop17', 'drop18', 'drop19', 'drop20', 'drop21', 'drop22',
                    'drop23', 'drop24', 'drop25', 'drop26', 'drop27', 'drop28', 'drop29', 'drop30',
                    'drop31', 'drop32', 'drop33', 'drop34', 'deductions_c', 'deductions_a', 'drop35', 'state_inc_tax_c',
                    'state_inc_tax_a', 'sales_tax_c', 'sales_tax_a', 'prop_tax_c', 'prop_tax_a', 
                    'drop36', 'drop37', 'mortgage_c', 'mortgage_a', 'contrib_c', 'contrib_a', 
                    'taxable_c', 'taxable_a', 'drop38', 'drop39', 'drop40', 'drop41', 'drop42', 
                    'drop43', 'credits_c', 'credits_a', 'drop44', 'drop45', 'drop46', 'drop47', 
                    'drop48', 'drop49', 'drop50', 'drop51', 'drop52', 'drop53', 'drop54', 'drop55', 
                    'drop56', 'drop57', 'drop58', 'drop59', 'drop60', 'drop61', 'drop62', 'drop63', 
                    'drop64', 'drop65', 'eic_c', 'eic_a', 'excess_eic_c', 'excess_eic_a', 'drop66',
                    'drop67', 'drop68', 'drop69', 'drop70', 'drop71', 'drop72', 'drop73', 
                    'tax_liab_c', 'tax_liab_a', 'drop74', 'drop75', 'drop76', 'drop77', 'tax_due_c',
                    'tax_due_a', 'refund_c', 'refund_a')

tax2015a <- subset(tax2015, select = -c(drop1, drop2, drop3, drop4, drop5, drop6, drop7, drop8, drop9, 
                                        drop10, drop11, drop12, drop13, drop14, drop15, drop16, drop17, 
                                        drop18, drop19, drop20, drop21, drop22, drop23, drop24, drop25, 
                                        drop26, drop27, drop28, drop29, drop30, drop31, drop32, drop33, 
                                        drop34, drop35, drop36, drop37, drop38, drop39, drop40, drop41, 
                                        drop42, drop43, drop44, drop45, drop46, drop47, drop48, drop49, 
                                        drop50, drop51, drop52, drop53, drop54, drop55, drop56, drop57, 
                                        drop58, drop59, drop60, drop61, drop62, drop63, drop64, drop65, 
                                        drop66, drop67, drop68, drop69, drop70, drop71, drop72, drop73, 
                                        drop74, drop75, drop76, drop77))
View(tax2015a)

#drop the rows in which all the col values is 0
tax2015a <- tax2015a%>%
  drop_na(zip_code)
View(tax2015a)



#droping the value for the total tax for the zipcodes
tax2015b <- tax2015a%>%
  drop_na(agi_range)
#selecting the values from tax2015a df where they have total for each zip code

totaltax_zip <- tax2015a%>%
  filter(is.na(agi_range))

#to replace the na with 0 in tax2015b data
tax2015b[is.na(tax2015b)]<- 0


#read the zip code data
zip_TN <- read.csv("data/zip_TN.csv")
View(zip_TN)

#look at the col names for zip_TN
zip_TN[0, ]
zip_TNa <- zip_TN[, -c(2,3,5:7,9:11)]
View(zip_TNa)
#read the school dat
school <- read.csv("data/school_ach.csv")
View(school)

sc_cr_a <- school%>%
  group_by(CORE_region)%>%
  summarise(mean_grad = mean(Graduation, na.rm = TRUE), 
            mean_composite = mean(ACT_Composite, na.rm = TRUE),
            mean_sus = mean(Pct_Suspended, na.rm = TRUE),
            mean_exp = mean(Pct_Expelled, na.rm = TRUE),
            mean_dropout = mean(Dropout, na.rm = TRUE),
            mean_enrol = mean(Enrollment, na.rm = TRUE),
            mean_expense = mean(Per_Pupil_Expenditures, na.rm = TRUE),
            mean_absent = mean(Pct_Chronically_Absent, na.rm = TRUE))
##sum up the row which will give total % of student leaving in middle
##due to expel and dropout
sc_cr_b <- sc_cr_a %>%
  rowwise()%>%
  mutate(left = sum(mean_exp, mean_dropout))
#drop the row which is for all TN
sc_cr_b <- sc_cr_b[-9, ]

##plots using the sc_cr_b df
ggplot( sc_cr_b, aes(x=mean_expense, y=mean_enrol, color = CORE_region, size = mean_grad)) + 
  geom_point() + ggtitle("Comparing Enrollment to expense per pupil") +
  xlab("Average expense per student") + ylab("Average Enrollment")

ggplot( sc_cr_b, aes(x=mean_expense, y=mean_enrol, color = CORE_region, size = left)) + 
  geom_point() + ggtitle("Comparing Enrollment to expense per pupil") +
  xlab("Average expense per student") + ylab("Average Enrollment")
