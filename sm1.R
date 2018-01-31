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

#read the garde gender county etc dat for school
school_mem2015 <- read_csv("data/data_2015_membership_school.csv")
#county crosswalk dat to get the county names
crosswalk <- read_xls("data/county_crosswalk.xls")

mem <- filter(school_mem2015, grade %in% c("9", "10", "11", "12"))

pairs(~Graduation + Dropout + Per_Pupil_Expenditures, data=school)
pairs(~ELA + Math + Science + ACT_Composite + Per_Pupil_Expenditures, data = school)
mem <- filter(school_mem2015, grade %in% c("9", "10", "11", "12"))

mem_grade<- mem%>%
  group_by(grade)
ggplot(mem_grade, aes(x = gender, y = enrollment)) +
  geom_col()
ggplot(mem_grade, aes(x = race_or_ethnicity, y = enrollment, 
                      color = race_or_ethnicity)) +
         geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("Number of Enrollment") +
  ggtitle("Ethnic Representation of Student body across TN High Schools")


library(PerformanceAnalytics)
chart.Correlation(school[, 3:6], histogram = TRUE, pch="+")

#to find the perunit tax return in various cat by amount/count####
tax2015c <- mutate(tax2015b, agi = agi_a/return_c,
                   wage = wage_a/wage_c,
                   tax_int = tax_int_a/tax_int_c,
                   div = div_a/div_c,
                   biz_inc = biz_inc_a/biz_inc_c,
                   cap_gain = cap_gain_a/cap_gain_c,
                   ira = ira_a/ira_c,
                   pension = pension_a/pension_c,
                   unemp_comp = unemp_comp_a/unemp_comp_c,
                   ss_ben = ss_ben_a/ss_ben_c,
                   deduction = deductions_a/deductions_c,
                   state_inc = state_inc_tax_a/state_inc_tax_c,
                   sales_tax = sales_tax_a/sales_tax_c,
                   prop = prop_tax_a/prop_tax_c,
                   mortgage = mortgage_a/mortgage_c,
                   contrib = contrib_a/contrib_c,
                   taxable = taxable_a/taxable_c,
                   credits = credits_a/credits_c,
                   eic = eic_a/eic_c,
                   excess_eic = excess_eic_a/excess_eic_c,
                   tax_liab = tax_liab_a/tax_liab_c,
                   tax_due = tax_due_a/tax_due_c,
                   refund = refund_a/refund_c)
View(tax2015c)

totaltax <- mutate(totaltax_zip, agi = agi_a/return_c,
                   wage = wage_a/wage_c,
                   tax_int = tax_int_a/tax_int_c,
                   div = div_a/div_c,
                   biz_inc = biz_inc_a/biz_inc_c,
                   cap_gain = cap_gain_a/cap_gain_c,
                   ira = ira_a/ira_c,
                   pension = pension_a/pension_c,
                   unemp_comp = unemp_comp_a/unemp_comp_c,
                   ss_ben = ss_ben_a/ss_ben_c,
                   deduction = deductions_a/deductions_c,
                   state_inc = state_inc_tax_a/state_inc_tax_c,
                   sales_tax = sales_tax_a/sales_tax_c,
                   prop = prop_tax_a/prop_tax_c,
                   mortgage = mortgage_a/mortgage_c,
                   contrib = contrib_a/contrib_c,
                   taxable = taxable_a/taxable_c,
                   credits = credits_a/credits_c,
                   eic = eic_a/eic_c,
                   excess_eic = excess_eic_a/excess_eic_c,
                   tax_liab = tax_liab_a/tax_liab_c,
                   tax_due = tax_due_a/tax_due_c,
                   refund = refund_a/refund_c)
View(totaltax)
##drop all the col which are not per count
totaltax <- totaltax[-(2:52)]

#to replace the na with 0 in totaltax data
totaltax[is.na(totaltax)]<- 0

max_agi <- totaltax%>%
  filter(agi == max(totaltax$agi))
pairs(~agi + wage + pension + ss_ben + ira, data=totaltax)
ggplot(totaltax, aes(x = agi)) +
  geom_histogram(binwidth = 5)
ggplot(school, aes(x = CORE_region, y = Enrollment)) +
  geom_boxplot()

school[is.na(school)]<- 0
View(school)

sc_cr <- school%>%
  group_by(CORE_region)%>%
  summarise(mean_grad = mean(Graduation), 
            mean_composite = mean(ACT_Composite),
            mean_sus = mean(Pct_Suspended),
            mean_exp = mean(Pct_Expelled),
            mean_dropout = mean(Dropout),
            mean_enrol = mean(Enrollment),
            mean_expense = mean(Per_Pupil_Expenditures),
            mean_absent = mean(Pct_Chronically_Absent))
##sum up the row which will give total % of student leaving in middle
##due to expel and dropout
sc_cr2 <- sc_cr %>%
  rowwise()%>%
  mutate(left = sum(mean_exp, mean_dropout))
#drop the row which is for all TN
sc_cr2 <- sc_cr2[-9, ]
  
ggplot( sc_cr2, aes(x=mean_expense, y=mean_enrol, color = CORE_region)) + 
          geom_point() + ggtitle("Comparing Enrollment to expense per pupil")
ggplot( sc_cr2, aes(x=CORE_region, y=mean_grad, color = CORE_region)) + 
  geom_boxplot() + 
  ggtitle("Comparing Graduation rate across TN core region") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("Percent Graduation")

school_crosswalk <- left_join(school, crosswalk, by = c("system" = "District Number"))
ttax_zip <- left_join(totaltax, zip_TNa, by = c("zip_code" = "zip")) 
###meged the school+crosswalk+totaltax+zip

merged  <- left_join(school_crosswalk, ttax_zip, by = c("County Name" = "county"))
merged2 <- left_join(ttax_zip, school_crosswalk, by = c("county" = "County Name"))


corr <- round(cor(totaltax), 1)
head(corr[, 1:6])

library(MASS)
plot(sc_cr2$mean_enrol, sc_cr2$mean_grad, 
     pch = 17, col = 'red', ylim = c(0, 100))
points(sc_cr2$mean_enrol, sc_cr2$mean_composite, 
       pch = 16, col = 'blue')
abline(a = 0, b = 1, lty = 2)
##finding relation in core region and agi
ggplot(merged2, aes(x= CORE_region, y = agi, 
       color = CORE_region, size = irs_estimated_population_2014)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("Income per return")
##finding relation between the core and population and grad
ggplot(merged2, aes(x= CORE_region, y = irs_estimated_population_2014, 
                    color = Pct_ED, size = Per_Pupil_Expenditures)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("Population")

#relation between 
ggplot(merged2, aes(x = CORE_region, y = Graduation)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("Percent Graduation")
#putting races
plot(merged2$CORE_region, merged2$Pct_Chronically_Absent, 
     pch = 17, col = 'red', ylim = c(0, 100))
points(merged2$CORE_region, merged2$Pct_BHN, 
       pch = 16, col = 'blue') 
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + ylab("Pct_Absent/Pct_BHN")

mem_school <- left_join(mem, school_crosswalk, 
                        by = c("district_name" = "County Name"))

ggplot(mem_school, aes(x = CORE_region, y = Graduation, 
                       color = gender, size = enrollment)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#removing all gender from the mem_school
mem2<- subset(mem_school, gender!="All Genders")
mem3 <- subset(mem2, race_or_ethnicity!= "All Race/Ethnic Groups")
ggplot(mem3, aes(x= CORE_region, y = enrollment, 
                 color = gender)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("") + ylab("Number Enrolled") 

ggplot(mem3, aes(x= CORE_region, y = enrollment, 
                 color = race_or_ethnicity)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("") + ylab("Number Enrolled") + ggtitle("Ethnic distribution in High School 
                                               across TN core region")

ggplot(mem3, aes(x= race_or_ethnicity, y = enrollment, 
                 color = CORE_region)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("") + ylab("Number Enrolled") + ggtitle("Ethnic distribution in High School 
                                               across TN core region")
ggplot(mem3, aes(x= race_or_ethnicity, y = Graduation, 
                 color = CORE_region)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("") + ylab("Graduation") + ggtitle("Ethnic distribution and Graduation in High School 
                                               across TN core region")
ggplot(mem3, aes(x= CORE_region, y = Graduation, 
                 color = race_or_ethnicity)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("") + ylab("Percent Graduation") + ggtitle("Graduation and Ethnicity in High School 
                                               across TN core region")

ggplot(mem3, aes(x= CORE_region, y = ACT_Composite, 
                 color = gender)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("") + ylab("ACT_Composite") + ggtitle("ACT scores between Male and Female students in High School 
                                                  across TN core region")

ggplot(mem3, aes(x= CORE_region, y = ACT_Composite, 
                 color = race_or_ethnicity)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("") + ylab("ACT_Composite") + ggtitle("ACT scores distribution and ethnic orientation 
                                                  of students across TN core region")

header <- merged2[0, ]
chart.Correlation(merged2[, c(2:6, 10 ,16)], histogram = TRUE, pch=".")
chart.Correlation(merged2[, c(2, 56, 52)], histogram = TRUE, pch=".")

chart.Correlation(merged2[, c(2, 56, 44:46, 49)], histogram = TRUE, pch=".")

chart.Correlation(merged2[, c(2, 43, 50 ,56, 49)], histogram = TRUE, pch=".")

ggplot(merged2, aes(x = ACT_Composite, y = agi, 
                    color = CORE_region, size = Per_Pupil_Expenditures)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("ACT_Composite") + ylab("AGI")
merged3 <- subset(merged2, ACT_Composite != 0)
ggplot(merged3, aes(x = ACT_Composite, y = agi, 
                    color = CORE_region)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("ACT_Composite") + ylab("AGI") +
  coord_flip()


ggplot(merged3, aes(x = Per_Pupil_Expenditures, y = agi, 
                    color = CORE_region)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Per Pupil Expenditure") + ylab("AGI") +
  coord_flip()

mer4<- merged3%>%
  group_by(CORE_region)%>%
  mutate(AGI = mean(agi),
         Expense = mean(Per_Pupil_Expenditures),
         ACT = mean(ACT_Composite),
         PCT_Graduation = mean(Graduation))

ggplot(mer4, aes(x = AGI, y = ACT, 
                    color = CORE_region, size = Expense)) +
  geom_point() 
ggplot(mer4, aes(x = PCT_Graduation, y = ACT, 
                 color = CORE_region, size = Expense)) +
  geom_point()  

library(GGally)
ggcorr(merged2)
ggcorr(school)
ggcorr(totaltax)

plot(merged2$agi, merged2$wage)


library(maps)
library(mapdata)
library(ggmap)#preloaded state/county coordinates

TN_data <- map_data("state") %>% 
  filter(region =='tennessee')

TN_counties <- map_data("county") %>% subset(., region == "tennessee")

ggplot() + geom_polygon(data = TN_counties, aes(x=long, y = lat, group = group), 
                        fill = NA, color = "red") + 
  coord_fixed(1.3) 





ggplot() + geom_polygon(data = merged2, aes(x=longitude, y = latitude, group = county), 
                        fill = NA, color = "red") +
  coord_fixed(1.3)

##geom_polygon(data = TN_counties, fill = "orange", color = "white") 
+  
  
##geom_polygon(color = "black", fill = NA)

TN_map <- ggplot(data = TN_data, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +  
  geom_polygon(data = TN_counties, fill = "orange", color = "white") +  
  geom_polygon(color = "black", fill = NA) 


merged3$county <- sapply(merged3$county, tolower)%>%
  gsub("county", "", .)

#testing commit for branch
