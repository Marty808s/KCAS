# OPORA

### ZDROJE

* Modely TS: https://www.youtube.com/watch?v=0ar9extHObg&t=44s
* Autokorelace, korelace, parciální korelace
    * https://www.youtube.com/watch?v=ZjaBn93YPWo&t=585s
    * 

## 1. Kdy používáme pro data aditivní dekompozici a kdy multiplikativní? Která Vám připadá lepší ve Vašem případě a proč?

Po dekompozici jednotlivých proměnných v různých frekvencích časové řady - kvartály/měsíčný jsme neviděli znaky multiplikativního modelu. Při růstu trendu se nezvětšuje sezoní a náhodná složka (nevznikají větší amplitudy s roustoucím trendem) -> tím pádem nedochází k multiplikaci jednotlivých složek časové řady. Proto jsme využili aditivní model

$$Y_{t} = T_{t} + S_{t} + E_{t}$$

![Additive vs Multiplicative](https://www.bounteous.com/sites/default/files/additive-vs-multiplicative.png "Additive vs Multiplicative")


## 2. U autokorelační funkce mi nesedí komentáře, které máte uvedené pod grafy - nejsou prohozené? Kromě autokorelační funkce jsme brali ještě parciální autokorelační funkci (PACF). Má smysl ji počítat i tady? A co vše Vám může spolu s ACF říct?

### Autokorelačí funkce
Jedná se o korelaci jedné proměnné na indexu časové řady $$t$$ vůčí hodnotě na indexu $$t+shift$$ Pro časové řadě definujeme posun (shift) a korelujeme vždy příslušnou sekvenci s příslušnou vzdáleností.

Výpočet pro jeden korelačí koeficient dvou hodnot (pro představu):
$$temps = [68.2, 65.7, ...]$$
$$res = corcoef(temps[0],temps[1])$$

### Výstup autokorelační funkce
Jak na zobě závisí hodnota na indexu $$t; (t+shift)$$ pomocí korelačního koeficientu.

**Po dekompozici časové řady:** Pokud po dekompozici zkontrolujeme ACF reziduí, chceme vidět hodnoty blízké nule pro všechny lagy. To by znamenalo, že v reziduích neexistuje žádná struktura a že model zachytil vše důležité.

**Identifikace sezónnosti a trendu:** ACF také pomáhá identifikovat sezónní a cyklické vzory v datech. Pokud má ACF výrazné hodnoty na určitých lagách (např. každý 12. lag u měsíčních dat), může to ukazovat na sezónnost, kterou model možná ještě nezachytil.

**Lepší výběr modelu:** Po zhodnocení ACF reziduí můžeš vybrat vhodnější model. Například pokud ACF ukazuje, že rezidua jsou autokorelovaná, může být potřeba použít ARIMA model, který tuto závislost zohledňuje.

## Parciální autokorelace
