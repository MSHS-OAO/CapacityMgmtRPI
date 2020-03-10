rm(list = ls())

#Install and load necessary packages --------------------
#install.packages("readraw_dfl")
#install.packages("writeraw_dfl")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("svDialogs")
#install.packages("stringr")

#Analysis for weekend discharge tracking
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(svDialogs)
library(stringr)

# Set working directory and select raw data ----------------------------
getwd()
setwd("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Discharge Billing Data")

# Data used to establish baseline and targets remains constant
raw_base <- read.csv("Crosstab_Discharges_YTD_Test 2019-11-22.csv", header = TRUE, na.strings = c("", "NA"))

# Save raw data in new dataframes
data_base <- raw_base

# Reference files and constants ----------------------------------------
ref_file <- "Analysis Reference 2019-11-22.xlsx"
site_dict <- read_excel(ref_file, sheet = "Sites")
dispo_dict <- read_excel(ref_file, sheet = "DischDispo")
dispo_dict[is.na(dispo_dict$`Discharge Disposition Desc Msx`), 1] <- "Unknown"
service_line_dict <- read_excel(ref_file, sheet = "ServiceLines")

site_order <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSSL")
DischDOW_Order <- c("Sat-Mon", "Tue", "Wed", "Thu", "Fri")

output_location <- choose.dir(caption = "Select script output folder", default = "J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management")

# Function to determine week number of year using Sat as first DOW --------------------------------------------------
weeknum <- function(x) {
  # yr <<- year(x)
  # new_yr <<- as.Date(paste0("1/1/", yr), format = "%m/%d/%Y")
  # new_yr_wkday <<- wday(new_yr, label = FALSE)
  # new_yr_sat <<- new_yr + (7 - new_yr_wkday)
  first_sat_2019 <<- as.Date("1/5/19", format = "%m/%d/%y")
  elapsed_days <<- as.numeric(x - first_sat_2019)
  week_number <<- ifelse(elapsed_days < 0, 1, as.integer(elapsed_days/7)+2)
  week_number
}

# Create a function to preprocess and format billing data -----------------
# Function as 3 input arguments:
# raw_df is the name of the existing dataframe with the raw data
# raw_df_as_char is the name of the raw data after it has been preprocessed entered as a string
# incl_df_as_char is the name of the dataframe after it has been preprocessed and exclusions removed entered as a string
preprocess <- function(raw_df, raw_df_as_char, incl_df_as_char) {
  # Format admit and discharge dates and pull out year, month, DOW, etc.
  raw_df[ , c("AdmitDate", "DischDate")] <- lapply(raw_df[ , c("Admit.Dt.Src", "Dsch.Dt.Src")], as.Date, "%m/%d/%Y")
  # raw_df$DischDate <- as.Date(raw_df$Dsch.Dt.Src, "%m/%d/%Y")
  raw_df$DischDateTime <- as.POSIXct(as.character(paste(raw_df$Dsch.Dt.Src, raw_df$Dsch.Time.Src)),  tz = "", format = "%m/%d/%Y %H:%M")
  raw_df[ , c("AdmitYr", "AdmitMo")] <- c(year(raw_df$AdmitDate), month(raw_df$AdmitDate))
  raw_df[ , c("DischYr", "DischMo", "DischHr")] <- c(year(raw_df$DischDate), month(raw_df$DischDate), hour(raw_df$DischDateTime))
  raw_df$DischDOW <- wday(raw_df$DischDate, label = TRUE, abbr = TRUE)
  
  # Lookup tables for site, discharge disposition, service line inclusion/eraw_dfclusion ----------------------------------------------
  # Site lookup
  raw_df$Facility.Msx <- as.character(raw_df$Facility.Msx)
  raw_df <- left_join(raw_df, site_dict, by = c("Facility.Msx" = "Facility Msx"))
  raw_df$Site <- as.factor(raw_df$Site)
  
  # Discharge disposition formatting and lookup
  raw_df$Discharge.Disposition.Desc.Msx <- factor(raw_df$Discharge.Disposition.Desc.Msx, levels = c(levels(raw_df$Discharge.Disposition.Desc.Msx), "Blank"))
  raw_df[is.na(raw_df$Discharge.Disposition.Desc.Msx), "Discharge.Disposition.Desc.Msx"] <- "Blank"
  raw_df$Discharge.Disposition.Desc.Msx <- as.character(raw_df$Discharge.Disposition.Desc.Msx)
  raw_df <- left_join(raw_df, dispo_dict, by = c("Discharge.Disposition.Desc.Msx" = "Discharge Disposition Desc Msx"))
  colnames(raw_df)[ncol(raw_df)] <- "DispoRollUp"
  raw_df[is.na(raw_df$DispoRollUp), "DispoRollUp"] <- "Unknown"
  raw_df$Discharge.Disposition.Desc.Msx <- as.factor(raw_df$Discharge.Disposition.Desc.Msx)
  raw_df$DispoRollUp <- as.factor(raw_df$DispoRollUp)
  
  # Service line inclusion / exclusion lookup
  colnames(raw_df)[colnames(raw_df) == "Service.Desc.Msx"] <- "ServiceLine"
  raw_df$ServiceLine <- as.character(raw_df$ServiceLine)
  raw_df$ServiceLine <- ifelse(is.na(raw_df$ServiceLine), "Unknown", raw_df$ServiceLine)
  raw_df <- left_join(raw_df, service_line_dict[ , c(1,3)], by = c("ServiceLine" = "Service Desc Msx"))
  colnames(raw_df)[ncol(raw_df)] <- "ServiceLineInclude"
  raw_df$ServiceLine <- as.factor(raw_df$ServiceLine)
  raw_df$ServiceLineInclude <- as.factor(raw_df$ServiceLineInclude)
  
  # Exclude expired patients and specified service lines
  raw_df$Include <- ifelse(raw_df$DispoRollUp == "Expired" | raw_df$ServiceLineInclude == "No", FALSE, TRUE)
  
  raw_df$Week_Num <- weeknum(raw_df$DischDate)
  raw_df$Weekend <- ifelse(raw_df$DischDOW == "Sat" | raw_df$DischDOW == "Sun" | raw_df$DischDOW == "Mon", TRUE, FALSE)
  
  # Create new dataframe with only included data --------------------------------
  assign(raw_df_as_char, raw_df, envir = .GlobalEnv)
  assign(incl_df_as_char, raw_df[raw_df$Include == TRUE, ], envir = .GlobalEnv)
  
}

# Preprocess baseline data and updated weekly datasets -----------------------------
preprocess(data_base, "baseline_preprocess", "baseline_include")


# Aggregate and format baseline data from Jan-Sep 2019 for future use --------------------------------
billing_oct <- baseline_preprocess[baseline_preprocess$DischMo == 10, ]
billing_oct_msw_msh <- billing_oct[billing_oct$Site == "MSH" | billing_oct$Site == "MSW", ]

msw_epic_raw <- read_excel(choose.files(caption = "Select MSW October Epic Data"), col_names = TRUE, na = c("", "NA"))
msh_epic_raw <- read_excel(choose.files(caption = "Select MSH October Epic Data"), col_names = TRUE, na = c("", "NA"))

msw_epic_raw$Site <- "MSW"
msh_epic_raw$Site <- "MSH"
epic_raw <- rbind(msw_epic_raw, msh_epic_raw)
epic_df <- epic_raw
epic_df$DispoInclude <- ifelse(epic_df$DISPOSITION == "Expired", 0, 1)

epic_unit_exclusions <- read_excel("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Epic Daily Discharge Timings\\Monthly Reports Jan-Oct 2019 Original Filters\\Analysis and Exclusions\\List of Exclusions.xlsx")
epic_df$UnitInclude <- ifelse(is.na(match(epic_df$`DISCHARGE UNIT`, epic_unit_exclusions$Units)), 1, 0)

epic_df$Include <- ifelse(epic_df$DispoInclude == 0 | epic_df$UnitInclude == 0, 0, 1)
epic_df %>%
  group_by(Site, Include) %>%
  summarize(TotalDisch = n())

billing_oct_msw_msh %>%
  group_by(Site, Include) %>%
  summarize(TotalDisch = n())

billing_oct_msw_msh$Encounter <- as.character(billing_oct_msw_msh$Encounter.No)
billing_oct_msw_msh$Encounter <- ifelse(str_sub(billing_oct_msw_msh$Encounter, start = nchar(billing_oct_msw_msh$Encounter), end = nchar(billing_oct_msw_msh$Encounter)) == "I", 
                                        as.integer(str_sub(billing_oct_msw_msh$Encounter, start = 1, end = nchar(billing_oct_msw_msh$Encounter)-1)), as.integer(billing_oct_msw_msh$Encounter))

epic_df$`VISIT ID` <- as.integer(epic_df$`VISIT ID`)
epic_df$VisitInBilling <- ifelse(is.na(match(epic_df$`VISIT ID`, billing_oct_msw_msh$Encounter)), 0, 1)

billing_oct_msw_msh$EncounterInEpic <- ifelse(is.na(match(billing_oct_msw_msh$Encounter, epic_df$`VISIT ID`)), 0, 1)

incl_bill_not_in_epic <- billing_oct_msw_msh[billing_oct_msw_msh$DispoRollUp != "Expired" & billing_oct_msw_msh$EncounterInEpic == 0, ]
billing_oct_msw_msh <- left_join(billing_oct_msw_msh, epic_df[ , c("VISIT ID", "DISCHARGE UNIT", "DISPOSITION", "DBS", "Include")], by = c("Encounter" = "VISIT ID"))

incl_bill_excl_epic <- billing_oct_msw_msh[billing_oct_msw_msh$Include.x == TRUE & billing_oct_msw_msh$Include.y == 0 & !is.na(billing_oct_msw_msh$Include.y), ]

msh_diff <- incl_bill_excl_epic[incl_bill_excl_epic$Site == "MSH", ]
msw_diff <- incl_bill_excl_epic[incl_bill_excl_epic$Site == "MSW", ]
