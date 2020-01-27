# RScript to import, bind, and export Epic data for 2019 from multiple reports
rm(list = ls())

library(readxl)
library(writexl)

getwd()
setwd("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Epic Daily Discharge Timing Reports\\Updated Reports")

# Import and bind monthly Epic data files -----------------
epic_raw_jan19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_January 2019.xls", col_names = TRUE, na = c("", "NA"))
epic_raw_feb19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_February 2019.xls", col_names = TRUE, na = c("", "NA"))
epic_raw_mar19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_March 2019.xls", col_names = TRUE, na = c("", "NA"))
epic_raw_apr19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_April 2019.xls", col_names = TRUE, na = c("", "NA"))
epic_raw_may19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_May 2019.xls", col_names = TRUE, na = c("", "NA"))
epic_raw_jun19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_June 2019.xls", col_names = TRUE, na = c("", "NA"))
epic_raw_jul19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_July 2019.xls", col_names = TRUE, na = c("", "NA"))
epic_raw_aug19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_August 2019.xls", col_names = TRUE, na = c("", "NA"))
epic_raw_sep19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_September 2019.xls", col_names = TRUE, na = c("", "NA"))
epic_raw_oct19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_October 2019.xls", col_names = TRUE, na = c("", "NA"))
epic_raw_nov19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_November 2019.xls", col_names = TRUE, na = c("", "NA"))
epic_raw_dec19 <- read_excel("Monthly Reports 2019 Updated Filters\\Discharge Timings Report monthly_December 2019.xls", col_names = TRUE, na = c("", "NA"))

epic_raw_2019 <- rbind(epic_raw_jan19, epic_raw_feb19, epic_raw_mar19, epic_raw_apr19, epic_raw_may19, epic_raw_jun19, epic_raw_jul19, epic_raw_aug19, epic_raw_sep19, epic_raw_oct19, epic_raw_nov19, epic_raw_dec19)

write_xlsx(epic_raw_2019, path = paste0("Epic Discharge Timings Report FY2019  ", Sys.Date(), ".xlsx"))

