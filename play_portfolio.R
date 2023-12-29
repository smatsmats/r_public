

library('tidyverse')
library('tidyquant')
library('googlesheets4')
library('lubridate')

pp_cols <- list(
  Name = col_character(),
  Symbol = col_character(),
  Type = col_character(),
  Date = col_character(),
  Withdrawl = col_number(),
  Deposit = col_number(),
  `#` = col_double(),
  `Div Out` = col_number(),
  `Div Rein` = col_number(),
  `Interest / Misc` = col_number(),
  `Buy price` = col_number(),
  `Sell price` = col_number(),
  Verify = col_character(),
  `Net Buy` = col_number(),
  `Net Sell` = col_number(),
  `Net Div` = col_number(),
  Transaction = col_number(),
  `Is Pending` = col_logical(),
  `Cash Balance` = col_number(),
  `RH Balance` = col_number()
)

rm_nas <- function(input) {
  input[is.na(input)] <- 0
  return(input)
}

my_cost <- function(symbol) {
  price <- sum(pp[pp$Symbol == symbol & pp$Type == 'Buy',]$`Net Buy`)
  return(price)
}

my_sale <- function(symbol) {
  price <- sum(pp[pp$Symbol == symbol & pp$Type == 'Sell',]$`Net Sell`)
  return(price)
}

numb_shares <- function(symbol) {
  b <- sum(pp[pp$Symbol == symbol & pp$Type == 'Buy',]$count)
  s <- sum(pp[pp$Symbol == symbol & pp$Type == 'Sell',]$count)
  r <- sum(pp[pp$Symbol == symbol & pp$Type == 'Dividend (reinvested)',]$`Div Rein`)
  fs <- sum(pp[pp$Symbol == symbol & pp$Type == 'Forward Split',]$count)
  rs <- sum(pp[pp$Symbol == symbol & pp$Type == 'Reverse Split',]$count)
  return(round(b - s + r + fs + rs, 7))
}

setwd('/Users/willey/Desktop')
today <- Sys.Date()

pp_sheet <- read_sheet('https://docs.google.com/spreadsheets/d/1X_zjs811JESFbzLvw-p8HKmRhvPldJ40_BaojPxLBWo/edit#gid=2059880044')

# clean out non-transation lines
pp <- subset(pp_sheet, ! is.na(Name))

#fix formats and na's
pp$Date <- as.Date.POSIXct(as.numeric(pp$Date))
pp$Deposit <- rm_nas(pp$Deposit)
pp$Withdrawl <- rm_nas(pp$Withdrawl)
pp$`Interest / Misc` <- rm_nas(pp$`Interest / Misc`)
pp$`Buy price` <- rm_nas(pp$`Buy price`)
pp$`Sell price` <- rm_nas(pp$`Sell price`)
pp$`Div Out` <- rm_nas(pp$`Div Out`)
pp$count <- rm_nas(pp$`#`)

sum(pp$Deposit)
sum(pp$Withdrawl)
sum(pp$`Interest / Misc`)
sum(pp$`Div Out`)

max(pp$Date)
last_bidn_day <- Sys.Date()
last_bidn_day <- "2023-11-22"

# this can be slow
#getSymbols(unique(pp$Symbol), from = min(pp$Date),
#           to = max(pp$Date),
#           to = last_bidn_day,
#           warnings = FALSE,
#           auto.assign = TRUE)




symbols <- unique(pp$Symbol)
symbols <- symbols[ !symbols == 'BREW']
symbols <- symbols[ !symbols == 'PEGI']
symbols <- symbols[ !symbols == 'SCTY']
symbols <- symbols[ !symbols == 'Cash']
symbols <- symbols[ !symbols == 'WCG']

symbols <- symbols[ !symbols == 'AJRD']
symbols <- symbols[ !symbols == 'BRK.B']
symbols <- symbols[ !symbols == 'WCG']

symbols <- symbols[ !symbols == 'XLNX']
symbols <- symbols[ !symbols == 'WORK']
symbols <- symbols[ !symbols == 'DOGE']
symbols <- symbols[ !symbols == 'WSKY']


current_values <- data.frame(symbols)
counts <- c()
prices <- c()
costs <- c()
avrg_costs <- c()
sells <- c()
for (s in symbols) {
  print(s)
  counts <- c(counts, numb_shares(s))
#  the_stock <- get(s)
  stock <- tq_get(s, get = "stock.prices", from = min(pp$Date), to=today)
  prices <- c(prices, as.numeric(stock[nrow(stock)-1,'close']))
  costs <- c(costs, my_cost(s))
  if ( numb_shares(s) > 0 ) {
    avrg_costs <- c(avrg_costs, my_cost(s) / numb_shares(s))    
  } else {
    avrg_costs <- c(avrg_costs, 0) 
  }
  sells <- c(sells, my_sale(s))
}
current_values$n <- counts
current_values$price <- prices
current_values$cost <- costs
current_values$sell <- sells
current_values$avrg_cost <- avrg_costs

current_values$position <- current_values$price * current_values$n

sum(current_values$cost)
sum(current_values$sell)
sum(current_values$cost) - sum(current_values$sell)
sum(current_values$position)


holdings <- filter(current_values, n > 0)
holdings$gain_loss <- holdings$position - holdings$cost
holdings$pct <- holdings$gain_loss / holdings$cost * 100

sum(holdings$gain_loss)
sum(holdings$gain_loss) / sum(holdings$cost) * 100

closed <- filter(current_values, n == 0)
closed$gain_loss <- closed$sell - closed$cost
closed$pct <- closed$gain_loss / closed$cost * 100

sum(closed$gain_loss)
sum(closed$gain_loss) / sum(closed$cost) * 100



invested <- sum(pp$Deposit) + sum(pp$`Interest / Misc`) + sum(pp$`Div Out`) - sum(pp$Withdrawl)
sum(pp$`Interest / Misc`)
sum(pp$`Div Out`) 
invested <- sum(pp$Deposit) - sum(pp$Withdrawl)

cat("Invested:", invested, "\n")

cash_balance <- sum(pp$Deposit) - sum(pp$Withdrawl) - sum(current_values$cost) + sum(current_values$sell)
cash_balance

