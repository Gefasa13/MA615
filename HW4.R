library(data.table)
library(lubridate)
library(dplyr)

file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
tail <- ".txt.gz&dir=data/historical/stdmet/"

years <- 1985:2023

ndbc_list <- list()

for (year in years) {
  path <- paste0(file_root, year, tail)
  header <- scan(path, what = 'character', nlines = 1)
  skip <- ifelse(year >= 2007, 2, 1)
  ndbc <- fread(path, header = FALSE, skip = skip)
  num_cols <- ncol(ndbc)
  if (length(header) > num_cols) {
    header <- header[1:num_cols]
  } else if (length(header) < num_cols) {
    header <- c(header, paste0("V", (length(header) + 1):num_cols))
  }
  
  colnames(ndbc) <- header
    if ("YY" %in% colnames(ndbc)) {
    ndbc$YY <- ifelse(ndbc$YY < 100, ifelse(ndbc$YY > 20, 1900 + ndbc$YY, 2000 + ndbc$YY), ndbc$YY)
  }
  
  if ("YY" %in% colnames(ndbc) & "MM" %in% colnames(ndbc) & "DD" %in% colnames(ndbc) & "hh" %in% colnames(ndbc) & "mm" %in% colnames(ndbc)) {
    ndbc$Date <- ymd_hms(paste(ndbc$YY, ndbc$MM, ndbc$DD, ndbc$hh, ndbc$mm))
  }
  
  
  ndbc_list[[as.character(year)]] <- ndbc
}

ndbc_list <- rbindlist(ndbc_list, fill = TRUE)
ndbc_list <- ndbc_list %>%
  mutate(Year = coalesce(as.numeric(YY), as.numeric(YYYY), as.numeric(`#YY`))) %>%
  select(-YY, -YYYY, -`#YY`) %>%
  select(Year, everything())
ndbc_list <- ndbc_list %>%
  mutate(Wind_Direction = coalesce(WD, WDIR)) %>%
  select(-WD, -WDIR)
ndbc_list <- ndbc_list %>%
  mutate(Pressure = coalesce(BAR, PRES)) %>%
  select(-BAR, -PRES)
if (all(c("Year", "MM", "DD", "hh", "mm") %in% colnames(ndbc_list))) {
  ndbc_list[, date := ymd_hms(paste(Year, MM, DD, hh, mm))]
}

str(ndbc_list)
ndbc_list <- ndbc_list %>%
  mutate(date = ifelse(complete.cases(Year, MM, DD, hh, mm),
                       make_datetime(year = Year, month = MM, day = DD, hour = hh, min = mm),
                       as.POSIXct(NA)))
ndbc_list$date <- make_datetime(
  year = ifelse(is.na(ndbc_list$Year), 2000, ndbc_list$Year),
  month = ifelse(is.na(ndbc_list$MM), 1, ndbc_list$MM),
  day = ifelse(is.na(ndbc_list$DD), 1, ndbc_list$DD),
  hour = ifelse(is.na(ndbc_list$hh), 0, ndbc_list$hh),
  min = ifelse(is.na(ndbc_list$mm), 0, ndbc_list$mm)
)
#(b)
ndbc_list[ndbc_list == 999] <- NA
#I don't think it's appropriate because in this data, 999 may mean that it is beyond the forecast range, which cannot be simply represented by NA, so I don't think it's appropriate. I found that the data before 2008 are missing PRES data.


#(c)
ndbc_list$date <- as.Date(ndbc_list$date)

library(ggplot2)
library(dplyr)

ggplot(ndbc_list, aes(x = date, y = WTMP)) + 
  geom_line(color = "blue") +
  labs(title = "Sea Surface Temperature Over Time", x = "Year", y = "Sea Surface Temperature (C)") +
  theme_minimal()
#Sea surface temperature over time: This visualization helps us see the long-term warming trend of the ocean

ggplot(ndbc_list, aes(x = date, y = ATMP)) + 
  geom_line(color = "red") +
  labs(title = "Air Temperature Over Time", x = "Year", y = "Air Temperature (C)") +
  theme_minimal()

#Rising air temperatures above bodies of water are an indicator of global warming. Looking at trends in air temperature and sea surface temperature can provide a more complete picture of climate change.

