library(tidyverse)
library(lubridate)
library(forecast)
library(gridExtra)
library(ggplot2)
library(dynlm)
library(TTR)
#---------------------------------------------------------------------------

# 1. Manipulace s daty
#   informace o datasetu Meta

# Date - datum den
# Open - Cena otevření
# High - Nejvyšší cena dne
# Low - Nejnižší cena dne
# Close - Cena, kdy se trh uzavíral
# Adj. Close - Zpřesněná uzavírací cena
# Volume - Suma, za kolik se tradovalo v daný den

data <- read.csv("./monthly_averages.csv")

# 1.1 Převod datumu na správný datový typ Date
data <- data %>%
  mutate(Date = as.Date(Date, format="%Y-%m-%d"))

# 1.2 Seřazení podle datumu
data <- data %>%
  arrange(Date) 

# 1.3 Agregace pro kvartály
data <- data %>%
  mutate(Quarter = paste(year(Date), quarter(Date), sep = " Q"))

data


# 1.4 Převedení dat do objektů časových řad

# Měsíc
ts_data <- ts(data, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12)

# Kvartál
ts_data_quarterly <- aggregate(ts_data, nfrequency=4, FUN=mean) # kvartál - mean(ni -> ni+3)

# Složka close - kvartál, rok
ts_data_close <- ts(data$Close, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12)
ts_data_quarterly_close <- aggregate(ts_data_close, nfrequency=4, FUN=mean)

# Složka open - kvartál, rok
ts_data_open <- ts(data$Open, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12)
ts_data_quarterly_open <- aggregate(ts_data_open, nfrequency=4, FUN=mean)

# Složka vol - kvartál, rok
ts_data_vol <- ts(data$Volume, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12)
ts_data_quarterly_vol <- aggregate(ts_data_vol, nfrequency=4, FUN=mean)


# Tabulka do markdownu
ts_data
ts_data_quarterly


plot(ts_data_quarterly)
plot(ts_data)

# 1.5 Popis řady - dát nahoru v markdownu
plot(ts_data_quarterly)
plot(ts_data)

# => podle pohledu aditivní model!

# dekompozice časové řady
decomposed <- decompose(ts_data)

decomp_close <- decompose(ts_data_close)
decomp_open <- decompose(ts_data_open)
decomp_vol <- decompose(ts_data_vol)

decomp_qclose <- decompose(ts_data_quarterly_close, type = "multiplicative")
decomp_qopen <- decompose(ts_data_quarterly_open, type = "multiplicative")
decomp_qvol <- decompose(ts_data_quarterly_vol, type = "multiplicative")


# Vizualizace - dekompozice časové řady
# roční
plot(decomp_close)
plot(decomp_open)
plot(decomp_vol)

# kvartální
plot(decomp_qclose)
plot(decomp_qopen)
plot(decomp_qvol)

# => podle pohledu aditivní model, ale při porovnání s multiplikativním 
# nebyl spatřen významný rozdíl

# => sezoní složka vypadá pravidelně
# => trend je klesající - dlouhodobě

#---------------------------------------------------------------------------

# 1.5 Vyhlazení dat

ts_close <- ts(data$Close, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12) # ročí - pro hodnotu Close
ts_volume <- ts(data$Volume, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12) # ročí - pro hodnotu Volume
ts_open <- ts(data$Open, start = c(year(min(data$Date)), month(min(data$Date))), frequency = 12) # ročí - pro hodnotu Open

plot(ts_close)
plot(ts_volume)
plot(ts_open)

# Grafy jedotlivých proměnných s vyhlazením - loess
# možná TO:DO - exponenciální vyrovnání do druhého sloupce (HoltWinters())
#https://www.youtube.com/watch?v=Vf7oJ6z2LCc&ab_channel=StatQuestwithJoshStarmer

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

# Zobrazení
grid.arrange(p1, p2, p3, ncol = 1)




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
# 2. Analýza
# => TO:DO - definovat bod 0 a lagy - mít přehled
# => i pro kvartály - tam je korelace menší
# PACF - co nám k tomu řekne?

# Autokorelační funkce
residual_close <- decomp_close$random
residual_close <- na.omit(residual_close) #na.omit() -> vamže případne NA hodnoty
acf(residual_close, main="ACF pro close")
pacf(residual_close)

residual_open <- decomp_open$random
residual_open <- na.omit(residual_open)
acf(residual_open, main="ACF pro open")
pacf(residual_open)

residual_volume <- decomp_vol$random
residual_volume <- na.omit(residual_volume)
acf(residual_volume, main="ACF pro volume")
pacf(residual_volume)

# U složky volume můžeme pozorovat významnou autokorelaci, která vyznačuje
# přítomnost krátkodobých závislostí.
# V dlouhodobém trendu se již nacházíme pod prahovou hodnotou - zvětšit lag!
# TO:DO kvartály


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
#3. Predikce
# 3.1 Lineární model se sezoní složkou

# Model sezónnosti pro Close
model_close <- tslm(ts_close ~ trend + season)
summary(model_close)
model_close$coefficients

# TOHLE DOPSAT
#Y_t = − 2.126 + 2.170*trend + 3.462*season2 + 1.366*season3 + 4.074*season4 + 10.242*season5 + 1.243*season6
#   + 7.046*season7 + 6.559*season8 + 3.180*season9 −2.516*season10 − 3.606*season11 − 3.690*season12
#===> výstup: největšá vliv má sezoní složka 5. měsíce - 10.242* na jednotku
#   druhý: s koeficientem 7.046* na jednotku  - 7. měsíc
#   třetí: 6.559*season8 - 8. měsíc
# podle p-value není ani jedna sezona význmaná

checkresiduals(model_close) # Dopsat výstup co to je?

# Model sezónnosti pro Volume
model_volume <- tslm(ts_volume ~ trend + season)
summary(model_volume)

checkresiduals(model_volume)

# Model sezónnosti pro Open
model_open <- tslm(ts_open ~ trend + season)
summary(model_open)

checkresiduals(model_open)
# Přidat výstupy z residuí

fc_close <- forecast(model_close, h = 12)
fc_open <- forecast(model_open, h = 12)
fc_volume <- forecast(model_volume, h = 12)

# Vizualizace výsledků modelu a předpovědi
p7 <- autoplot(fc_close) +
  labs(title = "Předpověď pro Close Price", x = "Date", y = "Close Price")

p8 <- autoplot(fc_open) +
  labs(title = "Předpověď pro Open Price", x = "Date", y = "Open Price")

p9 <- autoplot(fc_volume) +
  labs(title = "Předpověď pro Volume", x = "Date", y = "Volume")

# Zobrazení grafů vedle sebe
grid.arrange(p7, p8, p9, ncol = 1)


# Linear model s trendem a sezonosti
linear_model <- tslm(ts_data_quarterly_close ~ trend + season)

# Lineární model - vizualizace
autoplot(ts_data_quarterly_close, series="Vstupní data")+
  autolayer(forecast(linear_model,h=4), series="Fitted data")

#---------------------------------------------------------------------------
# 3.2 SARIMA
# TO:DO - udělat predikci roční ts -> 3 grafy -close, open, volume - viz. linearka
# TO:DO - residua, summary na model
#TO:DO optimální - vypsat koeficienty a pochopit co to je... jak to funguje

sarima <- auto.arima(ts_close, seasonal=TRUE)
fitted_values_close <- fitted(sarima)
#fitted_values_index <- fitted(sarima) <= původní

predict_sarima <- forecast(fitted_values_arima, h=12)

autoplot(ts_close, series="Vstupní data")+
  autolayer(predict_sarima, series="Predikce SARIMA")

# SARIMA kvartály
sarima_model <- auto.arima(ts_data_quarterly_close, seasonal=TRUE)

#Review modelu - dopsat slovně a porovnat
summary(sarima_model) 

# Vypočítané hodnoty modelem
fitted_values_arima <- fitted(sarima_model)
pred_q_sarima <- forecast(sarima_model, h=4)

# Vizualizace
autoplot(ts_data_quarterly_close, series="Vstupní data")+
autolayer(pred_q_sarima, series="Fitted data")

#---------------------------------------------------------------------------
# 3.3 ETS
# TO:DO - residua, summary na model
# TO:DO - udělat predikci roční ts -> 3 grafy -close, open, volume - viz. linearka
# Co je MMM
# => kvůli podmínkám - klouzavý průměr -> vyrovnání dat pro ETS

sma_data <- SMA(ts_data_quarterly_close, n=1)
sma_data
summary(sma_data)

# kvůli ETS - změna 0
sma_data[1] <- 0.0000001

ets_model <- ets(sma_data, model='MMM')
ets_fitted <-fitted(ets_model)

ets_qpred <- forecast(ets_fitted, h=4)

# ETS - vizualizace
autoplot(ts_data_quarterly_close, series="Vstupní data") +
  autolayer(ets_qpred, series="Fitted data")

#---------------------------------------------------------------------------
# 3.4 Dynamické modely - proč, co jak?...
# linearni dynamický model

m1 <- dynlm(ts_data_quarterly_close ~ decomposed_close$trend + decomposed_close$seasonal)
summary(m1)
#=> rozepsat výstupy podle Est, p-val...

# porovnání závislostí ostatních proměnných v řadě
m2 <- dynlm(ts_data_quarterly[,'Close'] ~ ts_data_quarterly[,'Open'] + ts_data_quarterly[,'Low'] + ts_data_quarterly[,'High'])
summary(m2)

#---------------------------------------------------------------------------
# 4. Porovnání jednotlivých modelů
# TO:DO navázat

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

# 5. Závěr
#---------------------------------------------------------------------------

