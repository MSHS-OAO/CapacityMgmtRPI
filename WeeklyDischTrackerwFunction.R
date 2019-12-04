#Install and load necessary packages --------------------
#install.packages("readraw_dfl")
#install.packages("writeraw_dfl")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("reshape")

#Analysis for weekend discharge tracking
library(readxl)
library(writerxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape)

# Set working directory and select raw data ----------------------------
getwd()
setwd("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Discharge Billing Data")

raw_base <- read.csv("Crosstab_Discharges_YTD_Test 2019-11-22.csv", header = TRUE, na.strings = c("", "NA"))
raw_update <- read.csv(choose.files(caption = "Select Most Recent Billing Data"), header = TRUE, na.strings = c("", "NA"))

# Reference files and constants ----------------------------------------
ref_file <- "Analysis Reference 2019-11-22.xlsx"
site_dict <- read_excel(ref_file, sheet = "Sites")
dispo_dict <- read_excel(ref_file, sheet = "DischDispo")
dispo_dict[is.na(dispo_dict$`Discharge Disposition Desc Msx`), 1] <- "Unknown"
service_line_dict <- read_excel(ref_file, sheet = "ServiceLines")

site_order <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSSL")
rpi_start <- as.Date("10/26/2019", "%m/%d/%Y")

# Week number function --------------------------------------------------
weeknum <- function(x) {
  yr <<- year(x)
  new_yr <<- as.Date(paste0("1/1/", yr), format = "%m/%d/%Y")
  new_yr_wkday <<- wday(new_yr, label = FALSE)
  new_yr_sat <<- new_yr + (7 - new_yr_wkday)
  elapsed_days <<- as.numeric(x - new_yr_sat)
  week_number <<- ifelse(elapsed_days < 0, 1, as.integer(elapsed_days/7)+2)
  week_number
}

# Create a function to preprocess and format billing data -----------------
data_base <- raw_base
data_update <- raw_update

preprocess <- function(raw_df, incl_df_as_char) {
  # Format admit and discharge dates and pull out year, month, DOW, etc.
  raw_df$AdmitDate <- as.Date(raw_df$Admit.Dt.Src, "%m/%d/%Y")
  raw_df$DischDate <- as.Date(raw_df$Dsch.Dt.Src, "%m/%d/%Y")
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
  raw_df$Discharge.Disposition.Desc.Msx <- factor(raw_df$Discharge.Disposition.Desc.Msx, levels = c(levels(raw_df$Discharge.Disposition.Desc.Msx), "Unknown"))
  raw_df[is.na(raw_df$Discharge.Disposition.Desc.Msx), "Discharge.Disposition.Desc.Msx"] <- "Unknown"
  raw_df$Discharge.Disposition.Desc.Msx <- as.character(raw_df$Discharge.Disposition.Desc.Msx)
  raw_df <- left_join(raw_df, dispo_dict, by = c("Discharge.Disposition.Desc.Msx" = "Discharge Disposition Desc Msx"))
  colnames(raw_df)[ncol(raw_df)] <- "DispoRollUp"
  raw_df$Discharge.Disposition.Desc.Msx <- as.factor(raw_df$Discharge.Disposition.Desc.Msx)
  raw_df$DispoRollUp <- as.factor(raw_df$DispoRollUp)
  
  # Service line inclusion / exclusion lookup
  colnames(raw_df)[colnames(raw_df) == "Service.Desc.Msx"] <- "ServiceLine"
  raw_df$ServiceLine <- as.character(raw_df$ServiceLine)
  raw_df <- left_join(raw_df, service_line_dict[ , c(1,3)], by = c("ServiceLine" = "Service Desc Msx"))
  colnames(raw_df)[ncol(raw_df)] <- "ServiceLineInclude"
  raw_df$ServiceLine <- as.factor(raw_df$ServiceLine)
  raw_df$ServiceLineInclude <- as.factor(raw_df$ServiceLineInclude)
  
  # Exclude expired patients and specified service lines
  raw_df$Include <- ifelse(raw_df$DispoRollUp == "Expired" | raw_df$ServiceLineInclude == "No", FALSE, TRUE)
  
  raw_df$Week_Num <- weeknum(raw_df$DischDate)
  raw_df$Weekend <- ifelse(raw_df$DischDOW == "Sat" | raw_df$DischDOW == "Sun" | raw_df$DischDOW == "Mon", TRUE, FALSE)
  
  # Create new dataframe with only included data --------------------------------
  assign(incl_df_as_char, raw_df[raw_df$Include == TRUE, ], envir = .GlobalEnv)
  
}


preprocess(data_base, "data2_base")
preprocess(data_update, "data2_update")

