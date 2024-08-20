############################
## Dekompozice casovych rad - hledani trendu a sezonni slozky
# nacteni knihoven, ktere pracuji s casovymi radami
library(TTR)
library(zoo)
library(forecast)
library(randtests)
# dalsi uzitecne funkce obsazene primo v knihovne stats

########################
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births
# mesicni pocty narozenych v New Yorku od ledna 1946 do prosince 1959
births.ts <- ts(births, frequency=12, start=c(1946,1))
# prevedeni na casovou radu
plot(births.ts)
# vykresleni rady

# chceme-li data vyhladit
births.exp<-HoltWinters(births.ts, gamma=FALSE)
# zobecnene dvojite exponencialni vyrovnani
plot(births.exp)
# vykresleni odhadnuteho trendu
births.exp
# odhadnute hodnoty parametru alfa a beta nam rikaji, ze odhady jsou zalozene 
#   predevsim na nedavnych hodnotach (zavislost nejde daleko do minulosti)
# chceme-li hladsi prubeh (vetsi vyhlazeni) volime male alfa
births.exp2<-HoltWinters(births.ts, alpha=0.1, gamma=FALSE)
plot(births.exp2)
# prubeh je hladsi, ale nesedi mi na data (je posunuty)
# pro mala alpha jsem na zacatku rady "mimo"

# nechci-li vyhlazovat, ale odhadovat (presneji se trefit),
#   je dobre pouzit prosezonni data Holt-Wintersovu metodu
births.hw<-HoltWinters(births.ts)
# podobna myslenka jako u dvojiteho exponencialniho vyhlazeni, 
#   ale pridam sezonnost
plot(births.hw)
# vykresleni odhadnuteho trendu
# na data sedi mnohem lepe 
births.hw
# vidim i vykyvy pro jenotlive mesice
# hladsi odhady nedostanu ani pro jine volby parametru
births.hw2<-HoltWinters(births.ts,alpha=0.1)
plot(births.hw2)
births.hw3<-HoltWinters(births.ts,gamma=0.1)
plot(births.hw3)

# porovnani, jak modely sedi na data
births.exp$SSE
births.hw$SSE
# Soucet druhych mocnin chyb odhadu

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
# mesicni data o prodeji suvenyru v Queendslandu v Australii od ledna 1987 do prosince 1993
souvenir.ts <- ts(souvenir, frequency=12, start=c(1987,1))	
# prevedeni na casovou radu
plot(souvenir.ts)
# vykresleni rady
# vykyvy se zvetsuji -> multplikativni sezonni rada

# Vyzkousim bezny HoltWintersuv model
souvenir.hw<-HoltWinters(souvenir.ts)
plot(souvenir.hw)
# vykresleni odhadnuteho trendu
souvenir.hw$SSE
# nic moc

# Ale existuje i Holt-Wintersova metoda pro multiplikativni rady
souvenir.hwm<-HoltWinters(souvenir.ts,seasonal = "multiplicative")
plot(souvenir.hwm)
# vykresleni odhadnuteho trendu
souvenir.hwm$SSE
# vyrazne lepsi

# druha moznost je logaritmovat
ln.souvenir.ts <- log(souvenir.ts)
# logaritmem casto prevedu multiplikativni model na aditivni
# oduvodneni: logaritmus soucinu rovna se soucet logaritmus
plot(ln.souvenir.ts)

ln.souvenir.exp<- HoltWinters(ln.souvenir.ts)
ln.souvenir.exp
# nizka hodnota koeficientu alpha ukazuje, ze uroven se meni v zavislosti na 
#   nedavnych i davnych datech
# beta = 0 rika, ze sklon trendu se v case nemeni
# vysoka hodnota gamma rika, ze sezonnost je zalozena na nedavnych/poslednich datech
plot(ln.souvenir.exp)
# odhad je velmi dobry

########################
## Obecna dekompozice rady se sezonni slozkou 
births.dec<-decompose(births.ts)
plot(births.dec)
# rada byla rozlozena na trend, sezonni slozku a nahodnou chybu
births.dec$seasonal
# sezonni slozka
births.dec$trend
# je mozne odhadnout krivkou logistickeho trendu
(ns<-births.dec$random)
# nahodna slozka

## test o nevyznamnosti korelaci 
Box.test(ns, lag=20, type="Ljung-Box")
# jsou vsechny autokorelace az do casu 20 nevyznamne?
acf(ns,na.action=na.omit)
# vykresleni autokorelacni funkce s mezemi -> viz priste

#######################
## Testy nahodnosti pro radu typu bily sum
plot(discoveries)

# Je tato rada nahodna?
difference.sign.test(discoveries)
# test zalozeny na znamenkach diferenci
diff(discoveries,1)
# takto vypadaji diference
sign(diff(discoveries,1))
# takto vypadaji znamenka diferenci
table(sign(diff(discoveries,1)))
# a tady vidime pocty kladnych a zapornych diferenci
# podle tohoto testu je rada nahodna

# pro testy zalozene na korelacnich koeficientech potrebuji poradove cislo hodnot
index<-1:length(discoveries)
cor.test(discoveries,index,method="kendall")
# test zalozeny na Kandallove korelacnim koeficientu
# korelacni koeficient pro usporadane promenne
cor.test(discoveries,index,method="spearman")
# test zalozeny na Spearmanove korelacnim koeficientu
# test pro spojite promenne zalozeny na poradich
# oba tyto testy ukazuji mirnou zapornou korelaci (rada klesa),
#   ktera je na hladine vyznamnosti 5% vyznamna

# dalsi testy
runs.test(discoveries)
# Wald-Wolfowitz Runs Test
discoveries-median(discoveries)
# rada odchylek od medianu
sign(discoveries-median(discoveries))
# znamenka odchylek
(zn<-sign(discoveries-median(discoveries))[sign(discoveries-median(discoveries))!=0])
# znamenka s vynechanymi nulami
abs(diff(zn))
# body, kde se mi meni znamenko
sum(abs(diff(zn)))/2
# pocet useku se stejnymi znamenky
# rada se tvari nahodne

# pro zajemce
cox.stuart.test(discoveries)
# znamenkovy test porovnavajici dve poloviny casove rady proti sobe
bartels.rank.test(discoveries)
# poradova verze Neumannova podiloveho testu

###############################
### Samostatne

# popiste + odhadnete radu AirPassengers
# Monthly Airline Passenger Numbers 1949-1960
# popiste + odhadnete radu fdeaths
# Monthly Deaths from Lung Diseases in the UK

# je rada LakeHuron nahodna?
# hladina Huronskeho jezera
# je rada lh nahodna?
# Luteinizing Hormone in Blood Samples
# je rada nhtemp nahodna?
# Average Yearly Temperatures in New Haven