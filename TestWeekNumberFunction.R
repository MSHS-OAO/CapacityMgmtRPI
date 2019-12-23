library(lubridate)

a <- as.Date("8/4/19", format = "%m/%d/%y")

weeknum <- function(x) {
  yr <<- year(x)
  new_yr <<- as.Date(paste0("1/1/", yr), format = "%m/%d/%Y")
  new_yr_wkday <<- wday(new_yr, label = FALSE)
  new_yr_sat <<- new_yr + (7 - new_yr_wkday)
  elapsed_days <<- as.numeric(x - new_yr_sat)
  week_number <<- ifelse(elapsed_days < 0, 1, as.integer(elapsed_days/7)+2)
  week_number
}

weeknum(a)


weeknum_running <- function(x) {
  # new_yr_2019 <<- as.Date(paste0("1/1/19"), format = "%m/%d/%y")
  # new_yr_wkday_2019 <<- wday(new_yr_2019, label = FALSE)
  # new_yr_sat_2019 <<- new_yr_2019 + (7 - new_yr_wkday_2019)
  first_sat_2019 <<- as.Date("1/5/19", format = "%m/%d/%y")
  elapsed_days_running <<- as.numeric(x - first_sat_2019)
  week_number_running <<- ifelse(elapsed_days_running < 0, 1, as.integer(elapsed_days_running/7)+2)
  week_number_running
}

weeknum_running(a)

b <- as.Date("1/10/21", format = "%m/%d/%y")
weeknum_running(b)

days <- seq(as.Date("12/25/19", format = "%m/%d/%y"), as.Date("12/30/20", format = "%m/%d/%y"), by = 1)
df <- c(days, weeknum_running(days))
start <- as.Date("12/25/19", format = "%m/%d/%y")
vec <- start + 0:372
