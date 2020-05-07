# setting the working directory
setwd("/Users/bardi/Documents/GitHub/spring20-barandogru/HW1")

# including the required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(xts)

# The data that will be used consists of the following:
# 1. Total credit/debit card expenditure amount (Thousand TRY)
# 2. Total credit/debit card number of transactions ()
# 3. USD/TRY exchange rate ()
# 4. Consumer price index of Turkey ()

# The main goal is to examine the behaviour of the total credit/debit card expenditure amount.
# To do that we will be be using number of transactions, usd currency and consumer price index.
# The questions that we are looking to find the answers for are as follows:
# 1. Is there a positive correlation between the amount spent and number of transactions?
# 2. How does amount spent per a transaction changed over time?
# 3. What is the relationship between currency rate and total amount spent? Correlation?
# 4. Is there a relationship between consumer price index and total amount spent converted to USD?


# reading the data (the data used belongs to Central Bank of the Republic of Turkey)
total_expenditure <- read_xlsx('totalcardexpenditure.xlsx')
num_transactions <- read_xlsx('totalnumoftransactions.xlsx')
usd_try <- read_xlsx('usdcurrency.xlsx')
turkey_cpi <- read_xlsx('turkeycpi.xlsx')

# tidying up the data respectively
total_expenditure <- total_expenditure %>%
                      rename(Date = 'Tarih', Amount = 'TP KKHARTUT KT1') %>%
                      filter(!is.na(Date) & !is.na(Amount))
total_expenditure$Date <- as.Date(total_expenditure$Date, format='%d-%m-%Y')

num_transactions <- num_transactions %>%
                      rename(Date = 'Tarih', Number = 'TP KKISLADE KA1') %>%
                      select(Date, Number) %>%
                      filter(!is.na(Date) & !is.na(Number)) %>%
                      slice(1:319)
num_transactions$Date <- as.Date(num_transactions$Date, format='%d-%m-%Y')
num_transactions$Number <- as.numeric(num_transactions$Number)

usd_try <- usd_try %>%
            rename(Date = 'Tarih', Currency = 'TP DK USD A YTL') %>%
            select(Date, Currency) %>%
            slice(2:2299) %>%
            mutate(USDTRY = na.locf(Currency)) %>%
            select(Date, USDTRY)
usd_try$Date <- as.Date(usd_try$Date, format='%d-%m-%Y')

turkey_cpi <- turkey_cpi %>%
            rename(Date = 'Tarih', CPI = 'TP FG J0') %>%
            select(Date, CPI) %>%
            slice(1:75)
turkey_cpi$Date <- as.yearmon(turkey_cpi$Date, format='%Y-%m')
turkey_cpi$CPI <- as.numeric(turkey_cpi$CPI)

# # creating xts objects respectively
# expenditure_xts <- xts(select(total_expenditure, Amount), order.by = total_expenditure$Date)
# transaction_xts <- xts(select(num_transactions, Number), order.by = num_transactions$Date)
# cpi_xts <- xts(select(turkey_cpi, CPI), order.by = turkey_cpi$Date)
# usdtry_xts <- xts(select(usd_try, USDTRY), order.by = usd_try$Date)

# Number of transactions vs total flow amount
amount_with_transactions <- merge(total_expenditure, num_transactions, by = 'Date')
amount_with_transactions <- amount_with_transactions %>%
                        mutate(Amount_Per_Transaction = Amount * 1000 / Number)
amount_vs_transactions <- amount_with_transactions %>%
                 gather(key = 'Type', value = 'Transaction', c(Amount, Number)) %>%
                 select(-Amount_Per_Transaction) %>%
                 arrange(Date)

amount_transaction_xts <- xts(select(amount_with_transactions, -c(Date)),
                              order.by = amount_with_transactions$Date)

ggplot(amount_transaction_xts, aes(x = index(amount_transaction_xts))) +
  theme_minimal() +
  geom_line(aes(y = Amount, color='Amount (1000 TL)')) +
  geom_line(aes(y = Number, color='Count')) +
  scale_x_date(breaks = '6 months', date_labels = '%b %Y') +
  theme(axis.text.x = element_text(angle=45), legend.position = 'bottom') +
  labs(x = 'Date', y = 'Total Expenditure',
       title = 'Total Expenditure Amount vs # of Transactions') +
  scale_color_manual(name = "Transaction",
                     values = c("Amount (1000 TL)" = "lightblue", "Count" = "darkred"))

ggplot(amount_transaction_xts, aes(x = index(amount_transaction_xts))) +
  theme_minimal() +
  geom_line(aes(y = Amount_Per_Transaction)) +
  scale_x_date(breaks = '6 months', date_labels = '%b %Y') +
  theme(axis.text.x = element_text(angle=45)) +
  labs(x = 'Date', y = 'Expenditure (TL)',
       title = 'Average Expenditure Amount per Transaction')

cor(amount_transaction_xts$Amount, amount_transaction_xts$Number)


# Total amount spent vs cpi
total_expenditure_monthly <- total_expenditure %>%
                              mutate(Month = month(Date), Year = year(Date)) %>%
                              group_by(Year, Month) %>%
                              summarise(Avg_Expen = mean(Amount)) %>%
                              ungroup()
total_expenditure_monthly <- total_expenditure_monthly %>%
                              mutate(Date = as.yearmon(paste(total_expenditure_monthly$Year, '-',
                                                             total_expenditure_monthly$Month),
                                                       format='%Y-%m')) %>%
                              select(c(Date, Avg_Expen))

expenditure_xts <- xts(select(total_expenditure, Amount), order.by = total_expenditure$Date)
usdtry_xts <- xts(select(usd_try, USDTRY), order.by = usd_try$Date)
index(expenditure_xts) <- index(expenditure_xts) + 2
usdtry_xts <- usdtry_xts['2014-03-07/2020-04-10']
ep <- endpoints(usdtry_xts, on='weeks')
usdtry_weekly_xts <- period.apply(usdtry_xts, INDEX = ep, FUN = mean)
amount_with_exchange <- merge(expenditure_xts, usdtry_weekly_xts, join = 'inner')

cor(amount_with_exchange$Amount, amount_with_exchange$USDTRY)

expenditure_monthly <- apply.monthly(expenditure_xts, FUN = mean, indexAt='yearmon')
index(expenditure_monthly) <- as.yearmon(index(expenditure_monthly))
cpi_xts <- xts(select(turkey_cpi, CPI), order.by = turkey_cpi$Date)

expenditure_monthly <- expenditure_monthly['2014-03-01/2020-03-31']
cpi_xts <- cpi_xts['2014-03-01/2020-03-31']
  
amount_with_cpi <- merge(expenditure_monthly, cpi_xts)

cor(amount_with_cpi$Amount, amount_with_cpi$CPI)

