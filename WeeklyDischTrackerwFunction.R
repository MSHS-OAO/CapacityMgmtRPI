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

# Data used to establish baseline and targets remains constant
raw_base <- read.csv("Crosstab_Discharges_YTD_Test 2019-11-22.csv", header = TRUE, na.strings = c("", "NA"))

# Select most recent data pull of billing data to update weekly discharge data
raw_update <- read.csv(choose.files(caption = "Select Most Recent Billing Data"), header = TRUE, na.strings = c("", "NA"))

# Save raw data in new dataframes
data_base <- raw_base
data_update <- raw_update

# Reference files and constants ----------------------------------------
ref_file <- "Analysis Reference 2019-11-22.xlsx"
site_dict <- read_excel(ref_file, sheet = "Sites")
dispo_dict <- read_excel(ref_file, sheet = "DischDispo")
dispo_dict[is.na(dispo_dict$`Discharge Disposition Desc Msx`), 1] <- "Unknown"
service_line_dict <- read_excel(ref_file, sheet = "ServiceLines")

site_order <- c("MSH", "MSQ", "MSBI", "MSB", "MSW", "MSSL")
DischDOW_Order <- c("Sat-Mon", "Tue", "Wed", "Thu", "Fri")
rpi_start <- as.Date("10/26/2019", "%m/%d/%Y")
rpi_end <- as.Date(dlg_input(message = "Enter Friday of last RPI cycle in data pull in mm/dd/yy format")$res, "%m/%d/%y")

output_location <- choose.dir(caption = "Select script output folder", default = "J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management")

# Function to determine week number of year using Sat as first DOW --------------------------------------------------
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
preprocess(data_update, "updated_preprocess", "updated_include")


# Aggregate and format baseline data from Jan-Sep 2019 for future use --------------------------------
baseline_jansep <- baseline_include[baseline_include$DischMo <= 9, ]

serviceline_daily <- as.data.frame(baseline_jansep %>%
  group_by(Site, DischDate, DischDOW, ServiceLine) %>%
  summarize(TotalDisch = n()))

hospital_daily <- as.data.frame(baseline_jansep %>%
  group_by(Site, DischDate, DischDOW) %>%
  summarize(TotalDisch = n()))

hospital_disch_dow <- as.data.frame(hospital_daily %>%
  group_by(Site, DischDOW) %>%
  summarize(TotalDischarges = as.numeric(sum(TotalDisch)), AverageDischarges = mean(TotalDisch)))

# Total discharges by day of week for each site
total_hosp_disch_dow_tbl <- dcast(hospital_disch_dow, Site ~ DischDOW, value.var = "TotalDischarges")
total_hosp_disch_dow_tbl <- total_hosp_disch_dow_tbl[order(factor(total_hosp_disch_dow_tbl$Site, levels = site_order)), ]
rownames(total_hosp_disch_dow_tbl) <- 1:nrow(total_hosp_disch_dow_tbl)

# Average discharges by day of week for each site
avg_hosp_disch_dow_tbl <- dcast(hospital_disch_dow, Site ~ DischDOW, value.var = "AverageDischarges")
avg_hosp_disch_dow_tbl <- avg_hosp_disch_dow_tbl[order(factor(avg_hosp_disch_dow_tbl$Site, levels = site_order)), ]
rownames(avg_hosp_disch_dow_tbl) <- 1:nrow(avg_hosp_disch_dow_tbl)

avg_hosp_stats <- cbind(avg_hosp_disch_dow_tbl, 
                   WeekendTotal = avg_hosp_disch_dow_tbl$Sat + avg_hosp_disch_dow_tbl$Sun + avg_hosp_disch_dow_tbl$Mon, 
                   TargetDelta = (avg_hosp_disch_dow_tbl[ , "Mon"]*0.1))
avg_hosp_stats$TargetTotal = round(avg_hosp_stats$WeekendTotal, 0) + round(avg_hosp_stats$TargetDelta, 0)
avg_hosp_dow_print <- format(avg_hosp_disch_dow_tbl, digits = 0)
avg_hosp_stats_print <- format(avg_hosp_stats, digits = 0)

hosp_baseline_target <- avg_hosp_stats[ , c("Site", "WeekendTotal", "TargetDelta", "TargetTotal")]
colnames(hosp_baseline_target) <- c("Site", "Weekend Baseline", "Target Change", "Weekend Target")

# Aggregate, format, and track discharges by week----------------
# Determine if date is after RPI cycles began
updated_include$PostRPI <- updated_include$DischDate >= rpi_start
updated_include <- updated_include[ updated_include$DischDate <= rpi_end, ]

# Create daily list of discharges by site
daily_disch_site <- as.data.frame(updated_include %>%
  group_by(Site, DischDate, Week_Num, DischDOW, Weekend, PostRPI) %>%
  summarize(TotalDisch = n()))

week_num_dates <- as.data.frame(daily_disch_site %>%
  group_by(Week_Num) %>%
  summarize(SatDate = min(DischDate), FriDate = max(DischDate)))

week_num_mon <- as.data.frame(daily_disch_site[daily_disch_site$DischDOW == "Mon", ] %>%
                                group_by(Week_Num) %>%
                                summarize(MonDate = min(DischDate)))

week_num_dates <- left_join(week_num_dates, week_num_mon, by = c("Week_Num"="Week_Num"))

week_num_dates[ , c("SatDate", "FriDate", "MonDate")] <- lapply(week_num_dates[ , c("SatDate", "FriDate", "MonDate")], format, "%m/%d/%y")

week_num_dates$WeekOf <- paste0(week_num_dates$SatDate, "-", week_num_dates$FriDate)
week_num_dates$WeekendOf <- paste0(week_num_dates$SatDate, "-", week_num_dates$MonDate)
week_num_dates <- week_num_dates[ , c("Week_Num", "SatDate", "WeekOf", "WeekendOf")]

daily_disch_site <- left_join(daily_disch_site, week_num_dates, by = c("Week_Num" = "Week_Num"))
daily_disch_site[ , c("WeekOf", "SatDate")] <- lapply(daily_disch_site[ , c("WeekOf", "SatDate")], factor)

# Create new dataframes for weekdays and weekends
wkday_disch_site <- daily_disch_site[daily_disch_site$Weekend != TRUE, ]
wkend_disch_site <- daily_disch_site[daily_disch_site$Weekend == TRUE, ]

wkend_disch_site_2 <- as.data.frame(wkend_disch_site %>%
  group_by(Site, Week_Num, Weekend, PostRPI, SatDate, WeekOf, WeekendOf) %>%
  summarize(DischDOW = "Sat-Mon", DischDate = min(DischDate), TotalDisch = sum(TotalDisch)))

wkday_disch_site_2 <- wkday_disch_site[ , c("Site", "Week_Num", "SatDate", "WeekOf", "WeekendOf", "DischDate",
                                            "DischDOW", "Weekend", "PostRPI", "TotalDisch")]
wkend_disch_site_2 <- wkend_disch_site_2[ , c("Site", "Week_Num", "SatDate", "WeekOf", "WeekendOf", "DischDate",
                                            "DischDOW", "Weekend", "PostRPI", "TotalDisch")]

daily_disch_site_2 <- rbind(wkday_disch_site_2, wkend_disch_site_2)
daily_disch_site_2$DischDOW <- factor(daily_disch_site_2$DischDOW, levels = DischDOW_Order)
daily_disch_site_2 <- daily_disch_site_2[order(daily_disch_site_2$Site, daily_disch_site_2$Week_Num, daily_disch_site_2$DischDOW), ]
rownames(daily_disch_site_2) <- 1:nrow(daily_disch_site_2)

# Data analysis since RPI cycles began -----------------------------------------------
# Create dataframes with discharges since RPI cycles began
daily_disch_site_rpi <- daily_disch_site[daily_disch_site$PostRPI == TRUE, ]
daily_disch_site_2_rpi <- daily_disch_site_2[daily_disch_site_2$PostRPI == TRUE, ]

# Create table showing weekend data since RPI cycles began
wkend_totals_site <- dcast(daily_disch_site_2_rpi[daily_disch_site_2_rpi$Weekend == TRUE, ], 
                           Site ~ WeekendOf, value.var = "TotalDisch")

wkend_totals_site <- wkend_totals_site[order(factor(wkend_totals_site$Site, levels = site_order)), ]
rownames(wkend_totals_site) <- 1:nrow(wkend_totals_site)

# Create table to show baseline, target, and weekly data since RPI cycles began
wkend_rpi_tracker <- left_join(hosp_baseline_target[ , c("Site", "Weekend Baseline", "Weekend Target")], 
                              wkend_totals_site, by = c("Site" = "Site"))

# weekend_rpi_tracker <- weekend_rpi_tracker[order(factor(weekend_rpi_tracker$Site, levels = site_order)), ]
# weekend_rpi_tracker_print <- format(weekend_rpi_tracker, digits = 0)

# write_xlsx(weekend_rpi_tracker_print, path = paste0("J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Script Outputs",
#                                                     "\\Weekend Discharge RPI Tracker ", Sys.Date(), ".xlsx"))


# Aggregate, format, and track discharges by day of week -----------------------------------
weekly_totals <- as.data.frame(daily_disch_site_2 %>%
  group_by(Site, Week_Num, SatDate, WeekOf, WeekendOf, PostRPI) %>%
  summarize(WkendTotal = sum(TotalDisch[Weekend == TRUE]), WklyTotal = sum(TotalDisch), WkendPercent = WkendTotal/WklyTotal*100))

weekly_totals_rpi <- melt(weekly_totals[weekly_totals$PostRPI == TRUE, ], id.vars = c("Site", "WeekOf"), measure.vars = c("WklyTotal", "WkendPercent"))
  
weekly_totals_rpi <- dcast(weekly_totals_rpi, Site + variable ~ WeekOf)

# Create a summary table of total weekly discharges and weekend discharges as % of total discharges-----------
site_weekly_table <- function(site) {
  weekly_totals_rpi <- weekly_totals_rpi[weekly_totals_rpi$Site == site, ]
  weekly_totals_rpi$Site <- NULL
  colnames(weekly_totals_rpi)[1] <- paste(site, "Metric")
  rownames(weekly_totals_rpi) <- 1:nrow(weekly_totals_rpi)
  weekly_totals_rpi[1, 2:ncol(weekly_totals_rpi)] <- round(weekly_totals_rpi[1, 2:ncol(weekly_totals_rpi)], digits = 0)
  weekly_totals_rpi[2, 2:ncol(weekly_totals_rpi)] <- paste(round(weekly_totals_rpi[2, 2:ncol(weekly_totals_rpi)], digits = 1), "%")
  name <- paste0(site, "WeeklyTotalPercent")
  assign(name, weekly_totals_rpi, envir = .GlobalEnv)
}

site_weekly_table("MSH")
site_weekly_table("MSQ")
site_weekly_table("MSBI")
site_weekly_table("MSB")
site_weekly_table("MSW")
site_weekly_table("MSSL")

export_table_list = list("WeekendSummary" = wkend_rpi_tracker,
                         "MSH Total & Percent" = MSHWeeklyTotalPercent, 
                         "MSQ Total & Percent" = MSQWeeklyTotalPercent,
                         "MSBI Total & Percent" = MSBIWeeklyTotalPercent,
                         "MSB Total & Percent" = MSBWeeklyTotalPercent,
                         "MSW Total & Percent" = MSWWeeklyTotalPercent, 
                         "MSSL Total & Percent" = MSSLWeeklyTotalPercent)

write_xlsx(export_table_list, path = paste0(output_location, "\\Weekly Discharge Stats Summary ", Sys.Date(), ".xlsx"))

# export_list = list(DischargeCensus = daily_disch_rpi,
#                    DischargeCensusWkndGroup = daily_disch_rpi2)

# write_xlsx(export_list, path = "J:\\Presidents\\HSPI-PM\\Operations Analytics and Optimization\\Projects\\Service Lines\\Capacity Management\\Data\\Script Outputs\\Export Discharge Census Data 2019-12-06.xlsx")

# Plot discharge trends by day of week for each site ---------------------------------------------------------
sinai_colors <- c("#221f72", "#00AEEF", "#D80B8C", "#B2B3B2", "#C7C6EF", "#DDDEDD", "#FCC9E9")

stacked_bar <- function(site) {
  ggplot(data = daily_disch_site_2_rpi[daily_disch_site_2_rpi$Site == site, ]) +
  geom_col(mapping = aes(x = SatDate, y = TotalDisch, fill = DischDOW), 
             position = position_stack(reverse = TRUE)) +
  labs(title = paste(site, "Weekly Discharges by DOW"), x = "Week Of", y = "Discharge Volume") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  guides(fill = guide_legend(reverse = FALSE, title = "Day of Week")) +
  geom_text(aes(x = SatDate, y = TotalDisch, label = TotalDisch), color = "white", position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = sinai_colors) +
  scale_y_continuous(expand = c(0, 0)) 
}

stacked_bar("MSH")
stacked_bar("MSQ")
stacked_bar("MSBI")
stacked_bar("MSB")
stacked_bar("MSW")
stacked_bar("MSSL")

msh_graph_dow <- stacked_bar("MSH")
msq_graph_dow <- stacked_bar("MSQ")
msbi_graph_dow <- stacked_bar("MSBI")
msb_graph_dow <- stacked_bar("MSB")
msw_graph_dow <- stacked_bar("MSW")
mssl_graph_dow <- stacked_bar("MSSL")

ggsave(path = output_location, file = paste("MSH Stacked Bar DOW", Sys.Date(), ".png"), plot = msh_graph_dow, device = "png", width = 4.8, height = 4, units = "in")
ggsave(path = output_location, file = paste("MSQ Stacked Bar DOW", Sys.Date(), ".png"), plot = msq_graph_dow, device = "png", width = 4.8, height = 4, units = "in")
ggsave(path = output_location, file = paste("MSBI Stacked Bar DOW", Sys.Date(), ".png"), plot = msbi_graph_dow, device = "png", width = 4.8, height = 4, units = "in")
ggsave(path = output_location, file = paste("MSB Stacked Bar DOW", Sys.Date(), ".png"), plot = msb_graph_dow, device = "png", width = 4.8, height = 4, units = "in")
ggsave(path = output_location, file = paste("MSW Stacked Bar DOW", Sys.Date(), ".png"), plot = msw_graph_dow, device = "png", width = 4.8, height = 4, units = "in")
ggsave(path = output_location, file = paste("MSSL Stacked Bar DOW", Sys.Date(), ".png"), plot = mssl_graph_dow, device = "png", width = 4.8, height = 4, units = "in")
