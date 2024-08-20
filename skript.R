library(tidyverse)
library(lubridate)
library(forecast)
library(gridExtra)
library(ggplot2)
library(dynlm)
#---------------------------------------------------------------------------
data <- read.csv("./monthly_averages.csv")

data <- monthly_averages

# Převod datumu na správný datový typ Date
data <- data %>%
  mutate(Date = as.Date(Date, format="%Y-%m-%d"))
data <- data %>%
  arrange(Date) # seřazení datumu

data <- data %>%
  mutate(Quarter = paste(year(Date), quarter(Date), sep = " Q"))

data

# Date - datum den
# Open - Cena otevření
# High - Nejvyšší cena dne
# Low - Nejnižší cena dne
# Close - Cena, kdy se trh uzavíral
# Adj. Close - Zpřesněná uzavírací cena
# Volume - Suma, za kolik se tradovalo v daný den

# Převedu data do objektu časové řady
ts_data <- ts(data, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12)
ts_data_quarterly <- aggregate(ts_data, nfrequency=4, FUN=mean) # kvartál - mean(ni -> ni+3)

ts_data_close <- ts(data$Close, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12)
ts_data_quarterly_close <- ts(data$Close, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 4) # kvartál - mean(ni -> ni+3)
ts_data_open <- ts(data$Open, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12)
ts_data_quarterly_open <- ts(data$Open, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 4) # kvartál - mean(ni -> ni+3)
ts_data_vol <- ts(data$Volume, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12)
ts_data_quarterly_vol <- ts(data$Volume, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 4) # kvartál - mean(ni -> ni+3)

# výstupy
ts_data
ts_data_quarterly

#plot(ts_data_quarterly)
#plot(ts_data)

# dekompozice časové řady
decomposed <- decompose(ts_data)
decomposed

decomp_close <- decompose(ts_data_close)
decomp_open <- decompose(ts_data_open)
decomp_vol <- decompose(ts_data_vol)

decomp_qclose <- decompose(ts_data_quarterly_close)
decomp_qopen <- decompose(ts_data_quarterly_open)
decomp_qvol <- decompose(ts_data_quarterly_vol)

# vizualizace
plot(decomp_close)
plot(decomp_open)
plot(decomp_vol)
plot(decomp_qclose)
plot(decomp_qopen)
plot(decomp_qvol)
#---------------------------------------------------------------------------
# Budeme porovnávat - náhled do dat
# 1. vytvoříme Time Series pro dané proměnné
ts_close <- ts(data$Close, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12) # ročí - pro hodnotu Close
ts_volume <- ts(data$Volume, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12) # ročí - pro hodnotu Volume
ts_open <- ts(data$Open, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12) # ročí - pro hodnotu Open

plot(ts_close)
plot(ts_volume)
plot(ts_open)

# grafy jedotlivých proměnných s vyhlazením
p1 <- ggplot(data, aes(x = Date, y = Close)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 0.5) +
  ggtitle("Close")

p2 <- ggplot(data, aes(x = Date, y = Volume)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 0.5) +
  ggtitle("Volume")

p3 <- ggplot(data, aes(x = Date, y = Open)) +
  geom_line(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, size = 0.5) +
  ggtitle("Open")

# zobrazení
grid.arrange(p1, p2, p3, ncol = 1)

#---------------------------------------------------------------------------
# 2. Dekompozice
# => na quarterly
decomposed_close <- decompose(ts_close, type = "multiplicative")
decomposed_volume <- decompose(ts_volume, type = "multiplicative")
decomposed_open <- decompose(ts_open, type = "multiplicative")
# plotneme pro vizualizaci trendu, sezony....


# Vizualizace dekompozice
plot(decomposed_close)
plot(decomposed_volume)
plot(decomposed_open)

#---------------------------------------------------------------------------
# 3. Analýza
# => převod na quartaly - kvůli season

# Autokorelační funkce
residual_close <- decomposed_close$random
residual_close <- na.omit(residual_close) #na.omit() -> vamže případne NA hodnoty
acf(residual_close, main="ACF pro close")


residual_open <- decomp_qopen$random
residual_open <- na.omit(residual_open)
acf(residual_open, main="ACF pro open")
# Open a close vykazují téměř stejné hodnoty autokorelace, téměř se nedostaneme 
# nad prahovou hodnotu


residual_volume <- decomposed_volume$random
residual_volume <- na.omit(residual_volume)
acf(residual_volume, main="ACF pro volume")
# U složky volume můžeme pozorovat významnou autokorelaci, která vyznačuje
# přítomnost krátkodobých závislostí.
# V dlouhodobém trendu se již nacházíme pod prahovou hodnotou 

#---------------------------------------------------------------------------
# Jednoduché exponenciální vyrovnání
ses_close <- ses(data$Close, h = 12)
ses_open <- ses(data$Open, h = 12)
ses_volume <- ses(data$Volume, h = 12)

# Nahrání výsledků k půlvodnímu datasetu
data <- data %>%
  mutate(SES_Close = fitted(ses_close),
         SES_Open = fitted(ses_open),
         SES_Volume = fitted(ses_volume))

# Vizualizace
p4 <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Close), color = "blue") +
  geom_line(aes(y = SES_Close), color = "purple", size = 1) +
  labs(title = "Close s vyhazením Simple Exponential Smoothing", x = "Date", y = "Close Price")

p5 <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Volume), color = "blue") +
  geom_line(aes(y = SES_Volume), color = "purple", size = 1) +
  labs(title = "Volume s vyhlazením Simple Exponential Smoothing", x = "Date", y = "Volume")

p6 <- ggplot(data, aes(x = Date)) +
  geom_line(aes(y = Open), color = "blue") +
  geom_line(aes(y = SES_Open), color = "purple", size = 1) +
  labs(title = "Open Price s vyhlazením Simple Exponential Smoothing", x = "Date", y = "Open Price")

# Zobrazení grafů vedle sebe
grid.arrange(p4, p5, p6, ncol = 1)

#---------------------------------------------------------------------------
# Vytvoření modelu sezónnosti pomocí tslm

# Model sezónnosti pro Close
model_close <- tslm(ts_close ~ trend + season)
summary(model_close)

# Model sezónnosti pro Open
model_open <- tslm(ts_open ~ trend + season)
summary(model_open)

# Model sezónnosti pro Volume
model_volume <- tslm(ts_volume ~ trend + season)
summary(model_volume)

#---------------------------------------------------------------------------
# Předpověď pomocí modelu sezónnosti
fc_close <- forecast(model_close, h = 12)
fc_open <- forecast(model_open, h = 12)
fc_volume <- forecast(model_volume, h = 12)

#---------------------------------------------------------------------------
# Vizualizace výsledků modelu a předpovědi
p7 <- autoplot(fc_close) +
  labs(title = "Předpověď pro Close Price", x = "Date", y = "Close Price")

p8 <- autoplot(fc_open) +
  labs(title = "Předpověď pro Open Price", x = "Date", y = "Open Price")

p9 <- autoplot(fc_volume) +
  labs(title = "Předpověď pro Volume", x = "Date", y = "Volume")

# Zobrazení grafů vedle sebe
grid.arrange(p7, p8, p9, ncol = 1)

#---------------------------------------------------------------------------
# SARIMA
arima <- auto.arima(ts_data, seasonal=TRUE)

fitted_values_index <- fitted(arima)

autoplot(ts_data, series="Data") +
  autolayer(fitted_values_index, series="Fitted") +
  labs(title = "Model SARIMA - Index", x = "Čas", y = "Index") +
  theme_minimal() +
  scale_colour_manual(values=c("Data"="blue","Fitted"="red"))

#---------------------------------------------------------------------------
# Optimální modely - podle AIC kritéria

# sarima model
sarima_model <- auto.arima(ts_data[,'Close'], seasonal=TRUE)

# linear model s trendem a sezonosti
linear_model <- tslm(ts_data[,'Close'] ~ trend + season)

aic_lm <- AIC(linear_model)
aic_sarima <- AIC(sarima_model)

res <- data.frame(
  Model = c('Linear model','SARIMA'),
  AIC = c(aic_lm,aic_sarima)
)
res

#---------------------------------------------------------------------------
# TO:DO 
# Kroskorelační funkce - pro mesíční srovnání - freq = 12
# -jak spolu ostatní řady souvisí a ovluvňují se navzájem?
# -jelikoz mame frekvenci ts<-12, tak jeden lag je jeden měsíc
# -pro lag=3 ::::> (<3)<--0-->(3>)

Close = ts_data[,'Close']
Open = ts_data[, 'Open']
High = ts_data[, 'High']
Low = ts_data[, 'Low']

lag <- 4
par(mfrow=c(3,1))
ccf(Close, Open,na.action =na.pass, lag=lag)
ccf(Close, High,na.action =na.pass, lag=lag)
ccf(Close, Low,na.action =na.pass, lag=lag)
par(mfrow=c(1,1))

# => přepsat...
# s posunem  3 měsíce vidíme stále velkou korelaci, ta však postupně klesá
# všechny hodnoty jsou nad modrou čárou, takže korelace je statisticky významná

#---------------------------------------------------------------------------
# dynamické modely
# linearni dynamický model
dynlm <- dynlm(ts_data_quarterly_close[,'Close']l ~)

#---------------------------------------------------------------------------
# opt model pro tslm - + splnění předpokladů pomocí analýzy residuí příslušných modelů, úprava při nesplnění
# k predikci - s intervaly spolehlivosti vykreslete a výsledky komentujte
# porovnání jednotlivých modelů
#---------------------------------------------------------------------------

