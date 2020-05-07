#####################################
#           Preparations            #
#####################################

# requiring the necessary packages
library(dplyr)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(forecast)
library(timeSeries)
library(zoo)

# library(xts)

# reading the data
df <- read.csv('gercek_tuketim.csv', sep = '"', header = TRUE)

# tidying the data
df <- df %>% 
  select(c('Tarih.', 'Saat', 'Tuketim.Miktari..MWh.')) %>%
  rename(Date = 'Tarih.', Hour = 'Saat', Consumption = 'Tuketim.Miktari..MWh.') 

df$Date <- gsub(",", "", df$Date)
df$Consumption <- as.numeric(gsub(",", ".", gsub("\\.", "", df$Consumption)))

df <- df %>%
  mutate(Datetime = as.POSIXct(paste(df$Date, df$Hour),format="%d.%m.%Y %H:%M"), 'GMT') %>%
  select(Datetime, Consumption)

# missing data
missing_day <- setdiff(seq(as.Date('2016-01-01'),as.Date('2020-04-26'), by=1), as_date(df$Datetime))
as.Date(missing_day, origin="1970-01-01")


#####################################
#  Basic Observations and Plotting  #
#####################################

# actual hourly data
ggplot(df, aes(x=df$Datetime, df$Consumption)) +
  theme_minimal() +
  geom_line(color='red', alpha=0.5) + 
  scale_x_datetime(breaks = '3 months', date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = 'Date', 
       y = 'Consumption (MWh)', 
       title = "Turkey's Electricity Consumption")


# daily data
df %>% 
  group_by(as_date(Datetime)) %>%
  summarise(Consumption = mean(Consumption)) %>%
  ggplot(aes(x=`as_date(Datetime)`, y=Consumption)) +
    theme_minimal() +
    geom_line(color='red', alpha=0.5) +
    scale_x_date(breaks = '3 months', date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = 'Date', 
         y = 'Consumption (MWh)', 
         title = 'Daily Average Consumption')

# day of the month
day_of_month <- df %>%
  group_by(day(Datetime)) %>%
  summarise(Consumption = mean(Consumption))

ggplot(day_of_month, aes(x=day_of_month$`day(Datetime)`, y=day_of_month$Consumption)) +
  theme_minimal() +
  geom_line(color='red', alpha=0.5) +
  geom_point(color='red') +
  scale_x_continuous(breaks = seq(1,31,2)) +
  labs(x = 'Day', 
       y = 'Consumption (MWh)', 
       title = 'Average Consumption by Days of a Month')

# month of the year
month_of_year <- df %>%
  group_by(month(Datetime)) %>%
  summarise(Consumption = mean(Consumption))

ggplot(month_of_year, aes(x=month_of_year$`month(Datetime)`, y=month_of_year$Consumption)) +
  theme_minimal() +
  geom_line(color='red', alpha=0.5) +
  geom_point(color='red') +
  scale_x_continuous(breaks = seq(1,12,1)) +
  labs(x = 'Month', 
       y = 'Consumption (MWh)', 
       title = 'Average Consumption by Months of a Year')

# monthly averages
monthly <- df %>%
  group_by(year(Datetime), month(Datetime)) %>%
  summarise(Consumption = mean(Consumption)) %>%
  ungroup() %>%
  mutate(Date = as.yearmon(paste(monthly$`year(Datetime)`, monthly$`month(Datetime)`, sep = ' '), 
                           format = '%Y %m'))

ggplot(monthly, aes(x=monthly$Date, y=monthly$Consumption)) +
  theme_minimal() +
  geom_line(color='red', alpha=0.5) +
  scale_x_yearmon(format = "%b %Y", n=9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = 'Date', 
       y = 'Consumption (MWh)', 
       title = 'Monthly Average Consumption')

# hour of the day
hour_of_day <- df %>%
  group_by(hour(Datetime)) %>%
  summarise(Consumption = mean(Consumption))

ggplot(hour_of_day, aes(x=hour_of_day$`hour(Datetime)`, y=hour_of_day$Consumption)) +
  theme_minimal() +
  geom_line(color='red', alpha=0.5) +
  geom_point(color='red') +
  scale_x_continuous(breaks = seq(0,23,1)) +
  labs(x = 'Hours', 
       y = 'Consumption (MWh)', 
       title = 'Average Consumption by Hours of a Day')

# Day of the week
day_of_week <- df %>%
  group_by(wday(Datetime, label = TRUE, week_start = 1)) %>%
  summarise(Consumption = mean(Consumption)) %>%
  rename(Day = 1) %>%
  ungroup()

ggplot(day_of_week, aes(x=day_of_week$Day, y=day_of_week$Consumption)) +
  theme_minimal() +
  geom_line(aes(group = 1), color='red', alpha=0.5) +
  geom_point(color='red') +
  labs(x = 'Days', 
       y = 'Consumption (MWh)', 
       title = 'Average Consumption by Days of a Week')


#####################################
#      Time Series Decomposition    #
#####################################

# hourly data converted to ts object
consumption <- msts(df$Consumption, start = c(2016,1), seasonal.periods = c(24,168,8760))
consumption %>%
  autoplot() +
  xlab('Date') +
  ylab('') +
  ggtitle("Turkey's Electricity Consumption")

# decomposing the data
components <- mstl(consumption, lambda = 'auto')
autoplot(components)

# seasonal adjustment
cons_sa <- seasadj(components)

# removing the trend
seas_diff = diff(diff(diff(diff(log(cons_sa), 8760), 168), 24),1)

# plotting the final version
autoplot(seas_diff)

# AR model
fit_AR <- Arima(consumption, order = c(8, 0, 0))

cbind(consumption, fitted(fit_AR)) %>%
  autoplot(alpha=0.5)

# MA model
fit_MA <- Arima(consumption, order = c(8, 0, 0))

cbind(consumption, fitted(fit_MA)) %>%
  autoplot(alpha=0.5)

# forecast
forecast(fit_AR, level = c(95), h = 24) %>%
  autoplot(include=168)

