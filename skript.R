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
ts_data_quarterly <- aggregate(ts_data, nfrequency=4, FUN=mean) # kvartál

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

decomp_qclose <- decompose(ts_data_quarterly_close, type = "additive")
decomp_qopen <- decompose(ts_data_quarterly_open, type = "additive")
decomp_qvol <- decompose(ts_data_quarterly_vol, type = "additive")


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


#SMA vyrovnání
posun <- 5
sma_close <- SMA(ts_close,n=posun)
sma_volume <- SMA(ts_volume,n=posun)
sma_open <- SMA(ts_open,n=posun)

ggplot(data,aes(x = Date, y = Close))+
  geom_line(color="blue") +
  geom_line(aes(x=Date,y=sma_close, color ="red"))

ggplot(data,aes(x = Date, y = Volume))+
  geom_line(color="blue") +
  geom_line(aes(x=Date,y=sma_volume, color ="red"))

ggplot(data,aes(x = Date, y = Open))+
  geom_line(color="blue") +
  geom_line(aes(x=Date,y=sma_open, color ="red"))

# Jednoduché exponenciální vyrovnání
ses_close <- ses(data$Close)
ses_open <- ses(data$Open)
ses_volume <- ses(data$Volume)

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
# Bod 0 => korelace časové řady sama se sebou, lag 1 = korelace lag 0 s lag 1, který značí posun o 12 měsíců zpět
# v lagu 2 se tedy projeví i nepřímé efekty z lagu 1
# Na rozdíl od toho PACF bere vždy pouze určitý lag v korelaci s časovou řadou (nižší lagy jsou odstraněny, není tam tento nepřímý vliv)
# ACF použiji pro identifikaci trendů a sezónnosti, dále pro modelování ARIMA, kdy mě zajímá jaké zpožděné hodnoty mají vliv
# na hodnoty časové řady
# PACF použiji pro identifikaci vztahů mezi jednotlivými lagy, dále když mají lagy z ACF dlouhý dosah a není tedy čitelné,
# které z nich jsou důležité
# => i pro kvartály - tam je korelace menší

# Autokorelační funkce
lag_max <- 60 #-> 5 let
qlag_max <- 20 #-> 5 let
# ACF a PACF pro 'close'
residual_close <- decomp_close$random
residual_close <- na.omit(residual_close) # Odstranění NA hodnot
acf(residual_close, main="ACF pro close", lag.max=lag_max)
#=> jsou prahové korelace - slabá korelace - v dlouhodobém posunu klesá
pacf(residual_close, lag.max=lag_max)
#=> jsou prahové korelace - slabá korelace - v dlouhodobém posunu klesá

# kvartály
residual_qclose <- decomp_qclose$random
residual_qclose <- na.omit(residual_qclose) # Odstranění NA hodnot
acf(residual_qclose, main="ACF pro close v Q", lag.max=lag_max)
#=> NEjsou prahové korelace - slabá korelace - v dlouhodobém posunu klesá
pacf(residual_qclose, lag.max=qlag_max)
#=> jsou prahové korelace - slabá korelace - v dlouhodobém posunu klesá

# ACF a PACF pro 'open'
residual_open <- decomp_open$random
residual_open <- na.omit(residual_open)
acf(residual_open, main="ACF pro open", lag.max=lag_max)
#=> jsou prahové korelace - slabá korelace, ale vyskytuje se i ve 3. lagu
#- nějaký milník nebo pád
# - v dlouhodobém posunu klesá
pacf(residual_open, lag.max=lag_max)
#=> jsou prahové korelace - slabá korelace - v dlouhodobém posunu klesá

# kvartály
residual_qopen <- decomp_qopen$random
residual_qopen <- na.omit(residual_qopen)
acf(residual_qopen, main="ACF pro open v Q", lag.max=lag_max)
#=> NEjsou prahové korelace - slabá korelace - v dlouhodobém posunu klesá
pacf(residual_qopen, lag.max=qlag_max)
#=> NEjsou prahové korelace - slabá korelace - v dlouhodobém posunu klesá

# ACF a PACF pro 'volume'
residual_volume <- decomp_vol$random
residual_volume <- na.omit(residual_volume)
acf(residual_volume, main="ACF pro volume", lag.max=lag_max)
#=> NEjsou prahové korelace - slabá korelace - v dlouhodobém posunu klesá
pacf(residual_volume, lag.max=lag_max)
#=> NEjsou prahové korelace - slabá korelace - v dlouhodobém posunu klesá

# kvartály
residual_qvol <- decomp_qvol$random
residual_qvol <- na.omit(residual_qvol)
acf(residual_qvol, main="ACF pro volume v Q", lag.max=lag_max)
#=> NEjsou prahové korelace - slabá korelace - v dlouhodobém posunu klesá
pacf(residual_qvol, lag.max=qlag_max)
#=> jsou prahové korelace - 6. kvartál vůči 1. - slabá korelace - v dlouhodobém posunu klesá


# Kroskorelační funkce - pro kvartální srovnání - freq = 12
# -jak spolu ostatní řady souvisí a ovluvňují se navzájem?
# -jelikoz mame frekvenci ts<-12, tak jeden lag je jeden měsíc
# -pro lag=3 ::::> (<3)<--0-->(3>)

Close = ts_data_quarterly[,'Close']
Open = ts_data_quarterly[, 'Open']
High = ts_data_quarterly[, 'High']
Low = ts_data_quarterly[, 'Low']
Volume = ts_data_quarterly[, 'Volume']

lag <- 20 #-> 5 let
par(mfrow=c(4,1))
ccf(Close, Open,na.action =na.pass, lag=lag)
ccf(Close, High,na.action =na.pass, lag=lag)
ccf(Close, Low,na.action =na.pass, lag=lag)
ccf(Volume, Open,na.action =na.pass, lag=lag)

par(mfrow=c(1,1))

#Close&Open,Close&High, Close&Low
# - v krátkodobém lagu existuje vzájemná korelace, v dlouhodobém hledisku však ne

#Volume&Open
# - Na počátku byla vysoká negativí korelace - následně v dlouhodobém 
# horizontu se závislost zmenšuje

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

checkresiduals(model_close)
#upozornit: první dva roky byla významná korelace - střední, 
# residua mají přibližně normálí rozdělení

# Model sezónnosti pro Volume
model_volume <- tslm(ts_volume ~ trend + season)
summary(model_volume)

checkresiduals(model_volume)
#upozornit: první dva roky byla významná korelace - střední, 
# residua mají přibližně normálí rozdělení

# Model sezónnosti pro Open
model_open <- tslm(ts_open ~ trend + season)
summary(model_open)

checkresiduals(model_open)
#upozornit: první dva roky byla významná korelace - střední, 
# residua mají přibližně normálí rozdělení

model_qclose <- tslm(ts_data_quarterly_close ~ trend + season)
summary(model_qclose)
checkresiduals(model_qclose)
# Přidat výstupy z residuí

# Predikce na rok
fc_close <- forecast(model_close, h = 12)
fc_open <- forecast(model_open, h = 12)
fc_volume <- forecast(model_volume, h = 12)
fc_qclose <- forecast(model_qclose, h=4)

# Vizualizace výsledků modelu a předpovědi
p7 <- autoplot(fc_close) +
  labs(title = "TSLM Předpověď pro Close Price", x = "Date", y = "Close Price")

p8 <- autoplot(fc_open) +
  labs(title = "TSLM Předpověď pro Open Price", x = "Date", y = "Open Price")

p9 <- autoplot(fc_volume) +
  labs(title = "TSLM Předpověď pro Volume", x = "Date", y = "Volume")

p10 <- autoplot(fc_qclose)+
    labs(title = "TSLM Předpověď pro Q Close", x = "Date", y = "Close Price")

# Zobrazení grafů vedle sebe
grid.arrange(p7, p8, p9, p10, ncol = 1)

#---------------------------------------------------------------------------
# 3.2 SARIMA

# Roční close
sarima_close <- auto.arima(ts_close, seasonal=TRUE)
fitted_values_close <- fitted(sarima_close)

summary(sarima_close)
# Optimali ARIMA model: ARIMA(2,1,2)
# => AR - využíváme 2 předchozí hodnoty časové řady k predikci současné hodnoty
# => I - Diference: data byly jednou diferencovaná  kvůli stacionaritě
# => MA - zohledňuje dvě předchozí chyby (rezidua) k predikci současné hodnoty
# => Drift - trend v modelu - konstanta

# ar - autoregresní koefeicienty
# ma - koeficienty klouzavého průměru


predict_sarima_close <- forecast(fitted_values_close, h=12)

ps1 <- autoplot(ts_close, series="Vstupní data")+
  autolayer(predict_sarima_close , series="Predikce SARIMA - close")+
  labs(title = "SARIMA - Předpověď pro Close", x = "Date", y = "Close")


# Roční open
sarima_open <- auto.arima(ts_open, seasonal=TRUE)
fitted_values_open <- fitted(sarima_open)

summary(sarima_open)
# ARIMA(3,1,3) with drift
checkresiduals(sarima_open) # test zda je korelace nulová v residuí
predict_sarima_open <- forecast(fitted_values_open, h=12)

ps2 <- autoplot(ts_open, series="Vstupní data")+
  autolayer(predict_sarima_open , series="Predikce SARIMA - open")+
  labs(title = "SARIMA - Předpověď pro Open", x = "Date", y = "Open")


# Roční volume
sarima_volume <- auto.arima(ts_volume, seasonal=TRUE)
fitted_values_volume <- fitted(sarima_volume)
summary(sarima_volume)
# ARIMA(0,1,1) 

predict_sarima_volume <- forecast(fitted_values_volume, h=12)

ps3 <- autoplot(ts_volume, series="Vstupní data")+
  autolayer(predict_sarima_volume , series="Predikce SARIMA - volume")+
  labs(title = "SARIMA - Předpověď pro Volume", x = "Date", y = "Volume")


# SARIMA kvartál close
sarima_qclose <- auto.arima(ts_data_quarterly_close, seasonal=TRUE)

# Vypočítané hodnoty modelem
fitted_values_qclose <- fitted(sarima_qclose)
pred_q_sarima_close <- forecast(sarima_model, h=4)

# Vizualizace
ps4 <- autoplot(ts_data_quarterly_close, series="Vstupní data")+
  autolayer(pred_q_sarima_close, series="Predikce SARIMA - Q close")+
  labs(title = "SARIMA - Předpověď pro Q Close", x = "Date", y = "Close")

grid.arrange(ps1, ps2, ps3, ps4, ncol = 1)

#---------------------------------------------------------------------------
# 3.3 ETS

# Roční Close
ets_model_close <- ets(ts_close)
ets_fitted_close <- fitted(ets_model_close)
ets_pred_close <- forecast(ets_fitted_close,h=12)

pe1 <- autoplot(ts_close, series="Vstupní data") +
  autolayer(ets_pred_close  , series="Predikce Close")+
  labs(title = "ETS - Předpověď pro Close", x = "Date", y = "Close")

# Roční Open
ets_model_open <- ets(ts_open, model="AAA")
ets_fitted_open <- fitted(ets_model_open)
ets_pred_open <- forecast(ets_fitted_open,h=12)

pe2 <- autoplot(ts_open, series="Vstupní data") +
  autolayer(ets_pred_open  , series="Predikce Open") +
  labs(title = "ETS - Předpověď pro Open", x = "Date", y = "Open")


# Roční Volume
ets_model_volume <- ets(ts_volume, model="AAA")
ets_fitted_volume <- fitted(ets_model_volume)
ets_pred_volume <- forecast(ets_fitted_volume,h=12)

pe3 <- autoplot(ts_volume, series="Vstupní data") +
  autolayer(ets_pred_volume  , series="Predikce Volume") + 
  labs(title = "ETS - Předpověď pro Volume", x = "Date", y = "Volume")


# Close Q

ets_model_qclose <- ets(ts_data_quarterly_close, model="AAA") # změna na automatickou volbu modelu
ets_fitted_qclose <-fitted(ets_model_qclose)
ets_pred_qclose <- forecast(ets_fitted_qclose, h=4)

# ETS - vizualizace
pe4 <- autoplot(ts_data_quarterly_close, series="Vstupní data") +
  autolayer(ets_pred_qclose , series="Fitted data") +
  labs(title = "ETS - Předpověď pro Q Close", x = "Date", y = "Q Close")

grid.arrange(pe1, pe2, pe3, pe4, ncol = 1)

#---------------------------------------------------------------------------
# 4. Porovnání jednotlivých modelů

# Vyhodnocení modelů
res <- data.frame(
  Model = c('Linear model','SARIMA', 'ETS'),
  'AIC Close' = c(AIC(model_close),AIC(sarima_close),AIC(ets_model_close)),
  'AIC Open' = c(AIC(model_open),AIC(sarima_open),AIC(ets_model_open)),
  'AIC Volume' = c(AIC(model_volume),AIC(sarima_volume),AIC(ets_model_volume)),
  'AIC Q_Close' = c(AIC(model_qclose),AIC(sarima_qclose),AIC(ets_model_qclose))
)

res

res_matrix <- t(as.matrix(res[, -1]))

# Vytvoření barplotu
barplot(res_matrix, beside = TRUE, 
        names.arg = res$Model, 
        col = c("orange", "blue", "green", "red"),
        legend.text = colnames(res)[2:5],
        args.legend = list(x = "topleft"),
        main = "AIC Comparison Across Different Models", 
        ylab = "AIC Values",)


# 5. Závěr
#ETS model má vyšší hodnoty AIC ve srovnání s SARIMA modelem, což naznačuje, že není tak přesný. 
#Zvláště u proměnných Volume a kvartální Close má velmi vysoké hodnoty AIC, což znamená, 
#že se méně hodí k modelování těchto časových řad. Pro proměnné Close a Open je ETS horší než SARIMA, 
#ale lepší než lineární model. ETS model může mít problém dobře zachytit strukturu proměnných ve tvých datech, 
#proto, že nezachycuje nelineární změny tak efektivně jako SARIMA.

#---------------------------------------------------------------------------

