data = Meta.Dataset.Cleaned

# Date - datum den
# Open - Cena otevření
# High - Nejvyšší cena dne
# Low - Nejnižší cena dne
# Close - Cena, kdy se trh uzavíral
# Adj. Close - Zpřesněná uzavírací cena
# Volume - Suma, za kolik se tradovalo v daný den

# Převedu data do objektu časové řady

ts_data <- ts(data, start = c(2012, 1), frequency = 12)  # roční
ts_data

#ts_data2 <- ts(data, start = c(2013, 1), frequency = 12)  # roční
#ts_data2

plot(ts_data)
library(ggplot2)
ggplot(data = ts_data, aes(x = Date, y = Open)) +
  geom_line() +
  labs(x = "Datum", y = "Hodnota", title = "Časová řada")
