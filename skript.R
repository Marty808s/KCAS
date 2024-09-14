library(tidyverse)
library(lubridate)
library(forecast)
library(gridExtra)
library(ggplot2)
library(dynlm)
library(TTR)
#---------------------------------------------------------------------------

data <- read.csv("./monthly_averages.csv")

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
ts_data_quarterly_close <- aggregate(ts_data_close, nfrequency=4, FUN=mean)

ts_data_open <- ts(data$Open, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12)
ts_data_quarterly_open <- aggregate(ts_data_open, nfrequency=4, FUN=mean)

ts_data_vol <- ts(data$Volume, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12)
ts_data_quarterly_vol <- aggregate(ts_data_vol, nfrequency=4, FUN=mean)

# výstupy
ts_data
ts_data_quarterly


plot(ts_data_quarterly)
plot(ts_data)

autoplot(ts_data_quarterly)
autoplot(ts_data)

# dekompozice časové řady
decomposed <- decompose(ts_data)
decomposed

autoplot(decomposed)

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

# EXPONCENCIONÁLNÍ VYROVNÁNÍ - HoltWinters()
#exp_smooth_index <- HoltWinters(ts_data_index, seasonal = "multiplicative")

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
decomposed_close <- decompose(ts_data_quarterly_close, type = "multiplicative")
decomposed_volume <- decompose(ts_data_quarterly_vol, type = "multiplicative")
decomposed_open <- decompose(ts_data_quarterly_open, type = "multiplicative")
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
pacf(residual_close)

residual_open <- decomposed_open$random # TADY BYL KVARTÁL..
residual_open <- na.omit(residual_open)
acf(residual_open, main="ACF pro open")
pacf(residual_open)
# Open a close vykazují téměř stejné hodnoty autokorelace => nenvýznamná, téměř se nedostaneme 
# nad prahovou hodnotu


residual_volume <- decomposed_volume$random
residual_volume <- na.omit(residual_volume)
acf(residual_volume, main="ACF pro volume")
pacf(residual_volume)
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
model_close$coefficients

#Y_t = − 2.126 + 2.170*trend + 3.462*season2 + 1.366*season3 + 4.074*season4 + 10.242*season5 + 1.243*season6
#   + 7.046*season7 + 6.559*season8 + 3.180*season9 −2.516*season10 − 3.606*season11 − 3.690*season12
#===> výstup: největšá vliv má sezoní složka 5. měsíce - 10.242* na jednotku
#   druhý: s koeficientem 7.046* na jednotku  - 7. měsíc
#   třetí: 6.559*season8 - 8. měsíc


#TO:DO - dodělat check residuí přes ACF A PACF - ale ne na kvartály, ale na měsíce viz předpovědi!

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

#sarima <- auto.arima(ts_data, seasonal=TRUE) <= nefungovalo
sarima <- auto.arima(ts_close, seasonal=TRUE)
fitted_values_close <- fitted(sarima)
#fitted_values_index <- fitted(sarima) <= původní

autoplot(ts_data_quarterly_close, series="Vstupní data")+
  autolayer(fitted_values_close, series="Fitted data")

#---------------------------------------------------------------------------
# Modely

# TO:DO SMA model!

# Sarima model - optimální model
sarima_model <- auto.arima(ts_data_quarterly_close, seasonal=TRUE)

# Vypočítané hodnoty modelem
fitted_values_arima <- fitted(sarima_model)
fitted_values_arima

# Vizualizace
autoplot(ts_data_quarterly_close, series="Vstupní data")+
autolayer(fitted_values_arima, series="Fitted data")

# Linear model s trendem a sezonosti
linear_model <- tslm(ts_data_quarterly_close ~ trend + season)

# Lineární model - vizualizace
autoplot(ts_data_quarterly_close, series="Vstupní data")+
  autolayer(linear_model$fitted.values, series="Fitted data")

# ETS
# => kvůli podmínkám - klouzavý průměr -> vyrovnání dat pro ETS
sma_data <- SMA(ts_data_quarterly_close, n=1)
sma_data
summary(sma_data)

# kvůli ETS - změna 0
sma_data[1] <- 0.0000001

ets_model <- ets(sma_data, model='MMM')
ets_fitted <-fitted(ets_model)

# ETS - vizualizace
autoplot(ts_data_quarterly_close, series="Vstupní data")+
  autolayer(ets_fitted, series="Fitted data")

#!SMA predikce

#---------------------------------------------------------------------------
# TO:DO 
# Kroskorelační funkce - pro mesíční srovnání - freq = 12
# -jak spolu ostatní řady souvisí a ovluvňují se navzájem?
# -jelikoz mame frekvenci ts<-12, tak jeden lag je jeden měsíc
# -pro lag=3 ::::> (<3)<--0-->(3>)

Close = ts_data_quarterly[,'Close']
Open = ts_data_quarterly[, 'Open']
High = ts_data_quarterly[, 'High']
Low = ts_data_quarterly[, 'Low']

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
# !dynamické modely - proč, co jak?...
# linearni dynamický model

m1 <- dynlm(ts_data_quarterly_close ~ decomposed_close$trend + decomposed_close$seasonal)
summary(m1)
#=> rozepsat výstupy podle Est, p-val...

# porovnání závislostí ostatních proměnných v řadě
m2 <- dynlm(ts_data_quarterly[,'Close'] ~ ts_data_quarterly[,'Open'] + ts_data_quarterly[,'Low'] + ts_data_quarterly[,'High'])
summary(m2)

#---------------------------------------------------------------------------
# kontrola předpokladů - analýza residui - úprava při nesplnění
#H0: autokorelace je nulová
#H1: autokorelace není nulová

# residua odhadnutého modelu by mela být nezávislá -> Ljung-Box neboli Box-Pierce

# SARIMA
checkresiduals(ets_fitted)
# p-value < 2.2e-16 => ZAMÍTÁME H0

# LM
checkresiduals(linear_model$fitted.values)
# p-value < 2.2e-16 => ZAMÍTÁME H0

# EST
checkresiduals(fitted_values_arima)
# p-value < 2.2e-16 => ZAMÍTÁME H0

#V modelech jsou přítomné závislosti zbytku - Autokorelace

#---------------------------------------------------------------------------
# Predikce modelů - 12 pozorování
# est, predikce, fit arima, s intervaly spolehlivosti vykreslete a výsledky komentujte

# TO:DO - dopsat asi ze summary?;

#EST
pred_est <- forecast(ets_model, h=12)
autoplot(pred_est)

summary(pred_est)

"""
Error measures:
     ME     RMSE      MAE        MPE      MAPE      MASE     ACF1
7.220935 48.58932 32.38335 -209271066 209271086 0.5523114 0.655942

"""

#LM s Trend a Season
pred_tslm<- forecast(linear_model, h=12)
autoplot(pred_tslm)

summary(pred_tslm)

"""
Error measures:
        ME     RMSE      MAE    MPE MAPE    MASE     ACF1
3.710184e-17 47.81943 27.63394 -Inf  Inf 0.4713083 0.704184

Coefficients:
(Intercept)        trend      season2      season3      season4  
      4.072        6.241       -5.165        3.986       -4.610  

"""

#Sarima
pred_sarima <- forecast(sarima_model, h=12)
autoplot(pred_sarima)

summary(pred_sarima)

"""
Error measures:
        ME     RMSE      MAE  MPE   MAPE    MASE      ACF1
-0.08169654 26.95386 16.43565 -Inf  Inf 0.2803167 0.1103005
"""
#------------------------------------------------------------------------

#Porovnání jednotlivých modelů

# AIC kritérium
aic_lm <- AIC(linear_model)
aic_sarima <- AIC(sarima_model)
aic_ets <- AIC(ets_model)

# Vyhodnocení modelů
res <- data.frame(
  Model = c('Linear model','SARIMA', 'ETS'),
  AIC = c(aic_lm,aic_sarima, aic_ets)
)

res

#---------------------------------------------------------------------------

