library(tidyverse)
library(lubridate)
library(forecast)
library(gridExtra)
library(ggplot2)

#---------------------------------------------------------------------------
data = monthly_averages

# Převod datumu na správný datový typ Date
data <- data %>%
  mutate(Date = as.Date(Date, format="%Y-%m-%d"))
data <- data %>%
  arrange(Date) # seřazení datumu

data

# Date - datum den
# Open - Cena otevření
# High - Nejvyšší cena dne
# Low - Nejnižší cena dne
# Close - Cena, kdy se trh uzavíral
# Adj. Close - Zpřesněná uzavírací cena
# Volume - Suma, za kolik se tradovalo v daný den

# Převedu data do objektu časové řady
ts_data <- ts(data, start = c(2012, 1), frequency = 12)  # roční pro všechny hodnoty Mety
ts_data

ts_data <- ts(data$Close, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12) # ročí - pro hodnotu Close
ts_data_quarterly <- aggregate(ts_data, nfrequency=4, FUN=mean) # kvartál - mean(ni -> ni+3)

# výstupy
ts_data
ts_data_quarterly

# dekompozice časové řady
decomposed <- decompose(ts_data)

# vizualizace
plot(decomposed)

#---------------------------------------------------------------------------
# Budeme porovnávat - náhled do dat
# 1. vytvoříme Time Series pro dané proměnné
ts_close <- ts(data$Close, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12) # ročí - pro hodnotu Close
ts_volume <- ts(data$Volume, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12) # ročí - pro hodnotu Volume
ts_open <- ts(data$Open, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12) # ročí - pro hodnotu Volume

plot(ts_close)
plot(ts_volume)
plot(ts_open)

# grafy jedotlivých proměnných
p1 <- ggplot(data, aes(x = Date, y = Close)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 0.3) +
  ggtitle("Close")

p2 <- ggplot(data, aes(x = Date, y = Volume)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 0.3) +
  ggtitle("Volume")

p3 <- ggplot(data, aes(x = Date, y = Open)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 0.3) +
  ggtitle("Open")

# zobrazení
grid.arrange(p1, p2, p3, ncol = 3)

#---------------------------------------------------------------------------
# 2. Dekompozice
decomposed_close <- decompose(ts_close)
decomposed_volume <- decompose(ts_volume)

# plotneme pro vizualizaci trendu, sezony....
plot(decomposed_close)
plot(decomposed_volume)

# 3. Analýza


