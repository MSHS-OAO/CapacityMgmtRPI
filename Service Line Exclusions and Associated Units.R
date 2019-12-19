# This script is used to identify the dominant units for each site for excluded service lines

#Install and load necessary packages --------------------
#install.packages("readraw_dfl")
#install.packages("writeraw_dfl")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("svDialogs")

#Analysis for weekend discharge tracking
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(svDialogs)

# Set working directory and select raw data ----------------------------
getwd()
setwd("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Discharge Billing Data")

# Select raw data file
raw_data <- read.csv(choose.files(caption = "Select Raw Data"), header = TRUE, na.strings = c("", "NA"))

# Save raw data in new dataframes
data_df <- raw_data

# Reference files and constants ----------------------------------------
ref_file <- "Analysis Reference 2019-11-22.xlsx"
site_dict <- read_excel(ref_file, sheet = "Sites")
dispo_dict <- read_excel(ref_file, sheet = "DischDispo")
dispo_dict[is.na(dispo_dict$`Discharge Disposition Desc Msx`), 1] <- "Unknown"
service_line_dict <- read_excel(ref_file, sheet = "ServiceLines")

site_order <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSSL")
DischDOW_Order <- c("Sat-Mon", "Tue", "Wed", "Thu", "Fri")

# Preprocess raw data
data_df[ , c("AdmitDate", "DischDate")] <- lapply(data_df[ , c("Admit.Dt.Src", "Dsch.Dt.Src")], as.Date, "%m/%d/%Y")
data_df$DischDateTime <- as.POSIXct(as.character(paste(data_df$Dsch.Dt.Src, data_df$Dsch.Time.Src)),  tz = "", format = "%m/%d/%Y %H:%M")
data_df[ , c("AdmitYr", "AdmitMo")] <- c(year(data_df$AdmitDate), month(data_df$AdmitDate))
data_df[ , c("DischYr", "DischMo", "DischHr")] <- c(year(data_df$DischDate), month(data_df$DischDate), hour(data_df$DischDateTime))
data_df$DischDOW <- wday(data_df$DischDate, label = TRUE, abbr = TRUE)

# Lookup tables for site, discharge disposition, service line inclusion/eraw_dfclusion ----------------------------------------------
# Site lookup
data_df$Facility.Msx <- as.character(data_df$Facility.Msx)
data_df <- left_join(data_df, site_dict, by = c("Facility.Msx" = "Facility Msx"))
data_df$Site <- as.factor(data_df$Site)

# Discharge disposition formatting and lookup
data_df$Discharge.Disposition.Desc.Msx <- factor(data_df$Discharge.Disposition.Desc.Msx, levels = c(levels(data_df$Discharge.Disposition.Desc.Msx), "Blank"))
data_df[is.na(data_df$Discharge.Disposition.Desc.Msx), "Discharge.Disposition.Desc.Msx"] <- "Blank"
data_df$Discharge.Disposition.Desc.Msx <- as.character(data_df$Discharge.Disposition.Desc.Msx)
data_df <- left_join(data_df, dispo_dict, by = c("Discharge.Disposition.Desc.Msx" = "Discharge Disposition Desc Msx"))
colnames(data_df)[ncol(data_df)] <- "DispoRollUp"
data_df[is.na(data_df$DispoRollUp), "DispoRollUp"] <- "Unknown"
data_df$Discharge.Disposition.Desc.Msx <- as.factor(data_df$Discharge.Disposition.Desc.Msx)
data_df$DispoRollUp <- as.factor(data_df$DispoRollUp)

# Service line inclusion / exclusion lookup
colnames(data_df)[colnames(data_df) == "Service.Desc.Msx"] <- "ServiceLine"
data_df$ServiceLine <- as.character(data_df$ServiceLine)
data_df$ServiceLine <- ifelse(is.na(data_df$ServiceLine), "Unknown", data_df$ServiceLine)
data_df <- left_join(data_df, service_line_dict[ , c(1,2)], by = c("ServiceLine" = "Service Desc Msx"))
colnames(data_df)[ncol(data_df)] <- "ServiceLineExclude"
data_df$ServiceLine <- as.factor(data_df$ServiceLine)
data_df$ServiceLineExclude <- as.factor(data_df$ServiceLineExclude)

data_excl <- droplevels(data_df[data_df$ServiceLineExclude == "Yes", ])

disch_by_unit <- data_excl %>%
  group_by(Site, DischYr, DischMo, ServiceLine, Unit.Desc.Msx) %>%
  summarize(UnitDisch = n())

disch_by_service_line <- data_excl %>%
  group_by(Site, DischYr, DischMo, ServiceLine) %>%
  summarize(ServiceLineDisch = n())

disch_join <- left_join(disch_by_unit, disch_by_service_line, by = c("Site" = "Site", "DischYr" = "DischYr", "DischMo" = "DischMo", "ServiceLine" = "ServiceLine"))
disch_join$Percent <- round(disch_join$UnitDisch / disch_join$ServiceLineDisch*100, digits = 1)

msh_units <- disch_join[disch_join$Site == "MSH", ]
msbi_units <- disch_join[disch_join$Site == "MSBI", ]
msw_units <- disch_join[disch_join$Site == "MSW", ]
mssl_units <- disch_join[disch_join$Site == "MSSL", ]

msh_units_2 <- dcast(msh_units, ServiceLine + Unit.Desc.Msx ~ DischMo, value.var = "Percent")
msbi_units_2 <- dcast(msbi_units, ServiceLine + Unit.Desc.Msx ~ DischMo, value.var = "Percent")
msw_units_2 <- dcast(msw_units, ServiceLine + Unit.Desc.Msx ~ DischMo, value.var = "Percent")
mssl_units_2 <- dcast(mssl_units, ServiceLine + Unit.Desc.Msx ~ DischMo, value.var = "Percent")
