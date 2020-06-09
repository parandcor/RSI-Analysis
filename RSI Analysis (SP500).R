# Libraries
library(quantmod)
library(lubridate)
library(TTR)
library(ggplot2)
library(reshape)
library(dplyr)
library(tidyquant)
library(rvest)
library(purrr)
library(tidyr)
library(gridExtra)

# Variables
history         <- 5  # Years of historical data
overboughtLevel <- 70 # Level where you think we will have an overbought situation
oversoldLevel   <- 30 # Level where you think we will have an oversold situation

wd <- "C:/Users/Administrator/Desktop/Github/RSI-Analysis"

# Access to the database with all the tickets in SP500
SP500 <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies" %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table() %>%
  pluck(1)

# Fixing some tickets to be downloaded from yahoo finance
SP500[SP500$Symbol == "BRK.B", "Symbol"] <- "BRK-B"
SP500[SP500$Symbol == "BF.B", "Symbol"]  <- "BF-B"

# Download historical price data ("X" days) 
mult_stocks <- tq_get(SP500$Symbol,
                      get  = "stock.prices",
                      from = Sys.Date() - years(history),
                      to   = Sys.Date(),
                      complete_cases = TRUE) %>%
  select(symbol, date, adjusted) %>%
  spread(key = symbol, value = adjusted) %>%
  as.data.frame()

# Remove the columns with NAs
mult_stocks <- mult_stocks %>% select_if(~ !any(is.na(.)))

# Moving Average Calculation
RSI14_OS <- data.frame(matrix(data = 0, nrow = nrow(mult_stocks), ncol = ncol(mult_stocks) - 1))
RSI14_OB <- data.frame(matrix(data = 0, nrow = nrow(mult_stocks), ncol = ncol(mult_stocks) - 1))

for(i in 2:ncol(mult_stocks)){ # i <- 1
  
  RSI14_OB[,i] <- ifelse(RSI(mult_stocks[,i], n = 14) > overboughtLevel, 1, 0)
  RSI14_OS[,i] <- ifelse(RSI(mult_stocks[,i], n = 14) < oversoldLevel, 1, 0)
  
  print(paste("Progress... ", round(i/(ncol(mult_stocks) - 1)*100, digits = 2), "%", sep = ""))
}

# Results
RSI14_OB <- rowSums(RSI14_OB)/ncol(RSI14_OB)
RSI14_OS <- rowSums(RSI14_OS)/ncol(RSI14_OS)

# Create the final Data Base with all the info
finalDDBB_OB <- data.frame(RSI14_OB) %>%
  mutate(Dates = mult_stocks$date %>% as.Date(format = "%Y-%m-%d")) %>%
  na.omit() %>%
  `colnames<-`(c("Overbought", "Dates")) %>%
  mutate(Value = round(Overbought*100, digits = 2))

finalDDBB_OS <- data.frame(RSI14_OS) %>%
  mutate(Dates = mult_stocks$date %>% as.Date(format = "%Y-%m-%d")) %>%
  na.omit() %>%
  `colnames<-`(c("Oversold", "Dates")) %>%
  mutate(Value = round(Oversold*100, digits = 2))

# Plot (Overbought)
OB_Analysis <- ggplot(data = finalDDBB_OB, aes(x = Dates, y = Value/100)) +
  geom_line(size = 1) +
  scale_color_viridis_d(end = 0.9)+
  theme_minimal() +
  labs(title = "SP500 - Overbought and Oversold Analysis (Percentage of companies)",
       subtitle = paste("RSI Levels: ", overboughtLevel, " - ", oversoldLevel, sep = ""),
       caption = "",
       y = "Overbought Condition",
       x = "Dates") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "bottom")

# Plot (Oversold)
OS_Analysis <- ggplot(data = finalDDBB_OS, aes(x = Dates, y = Value/100)) +
  geom_line(size = 1) +
  scale_color_viridis_d(end = 0.9)+
  theme_minimal() +
  labs(title = "",
       subtitle = "",
       caption = "By: Carlos Jimenez (@cjimenezdiaz)\nSource: Yahoo Finance",
       y = "Oversold Condition",
       x = "Dates") +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position="bottom")

# Saving the results
ggsave(file = paste(wd, "/OB_and_OS_Analysis.png", sep = ""), plot = grid.arrange(OB_Analysis, OS_Analysis, nrow = 2, ncol = 1), dpi = 300, width = 8, height = 8)
