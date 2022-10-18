

library('tidyverse')
library('tidyquant')

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
  Interest = col_number(),
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

numb_shares <- function(symbol) {
  b <- sum(pp[pp$Symbol == symbol & pp$Type == 'Buy',]$count)
  s <- sum(pp[pp$Symbol == symbol & pp$Type == 'Sell',]$count)
  fs <- sum(pp[pp$Symbol == symbol & pp$Type == 'Forward Split',]$count)
  return(b - s + fs)
}

numb_shares("F")

setwd('/Users/willey/Desktop')
today <- Sys.Date()

pp_in <- read_csv('Play Portfolio - Main.csv', col_types = pp_cols)
# clean out non-transation lines
pp <- subset(pp_in, ! is.na(Name))

#fix formats and na's
pp$Date <- as.Date(pp$Date, format = "%m/%d/%Y")
pp$Deposit <- rm_nas(pp$Deposit)
pp$Withdrawl <- rm_nas(pp$Withdrawl)
pp$Interest <- rm_nas(pp$Interest)
pp$`Buy price` <- rm_nas(pp$`Buy price`)
pp$`Sell price` <- rm_nas(pp$`Sell price`)
pp$`Div Out` <- rm_nas(pp$`Div Out`)
pp$count <- rm_nas(pp$`#`)

sum(pp$Deposit)
sum(pp$Withdrawl)
sum(pp$Interest)
sum(pp$`Div Out`)

summary(pp$`Div Out`)

invested <- sum(pp$Deposit) + sum(pp$Interest) + sum(pp$`Div Out`) - sum(pp$Withdrawl)

cat("Invested:", invested, "\n")

max(pp$Date)
getSymbols(unique(pp$Symbol), from = min(pp$Date),
           to = max(pp$Date),warnings = FALSE,
           auto.assign = TRUE)


getSymbols("TSLA", from = min(pp$Date),
           to = today, warnings = FALSE,
           auto.assign = TRUE)

summary(pp$count)


