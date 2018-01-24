max_agi_row <- irs_2015 %>%
  filter(`AGI amount` == max(irs_2015$`AGI amount`))
irsA <- read_excel("data/irs_2015.xls")

tax2015 <- readxl::read_xls("data/tax2015.xls", range = "A6:DY4725")


