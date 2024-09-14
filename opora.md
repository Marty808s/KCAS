# OPORA

### ZDROJE

* Modely TS: https://www.youtube.com/watch?v=0ar9extHObg&t=44s
* Autokorelace, korelace, parciální korelace / autokorelace
    * https://www.youtube.com/watch?v=ZjaBn93YPWo&t=585s
    * https://www.youtube.com/watch?v=LF0WAVBIhNA
    * https://www.youtube.com/watch?v=DeORzP0go5I
    
    

## 1. Kdy používáme pro data aditivní dekompozici a kdy multiplikativní? Která Vám připadá lepší ve Vašem případě a proč?

Po dekompozici jednotlivých proměnných v různých frekvencích časové řady - kvartály/měsíčný jsme neviděli znaky multiplikativního modelu. Při růstu trendu se nezvětšuje sezoní a náhodná složka (nevznikají větší amplitudy s roustoucím trendem) -> tím pádem nedochází k multiplikaci jednotlivých složek časové řady. Proto jsme využili aditivní model

$$Y_{t} = T_{t} + S_{t} + E_{t}$$

![Additive vs Multiplicative](https://www.bounteous.com/sites/default/files/additive-vs-multiplicative.png "Additive vs Multiplicative")


## 2. U autokorelační funkce mi nesedí komentáře, které máte uvedené pod grafy - nejsou prohozené? Kromě autokorelační funkce jsme brali ještě parciální autokorelační funkci (PACF). Má smysl ji počítat i tady? A co vše Vám může spolu s ACF říct?

### Autokorelační funkce
Jedná se o korelaci jedné proměnné na indexu časové řady \( t \) vůči hodnotě na indexu \( t+shift \). Pro časové řady definujeme posun (shift) a korelujeme vždy příslušnou sekvenci s odpovídající vzdáleností.

Výpočet pro jeden korelační koeficient dvou hodnot (pro představu):
$$ temps = [68.2, 65.7, \dots] $$
$$ res = cor(temps[0], temps[1]) $$

Pokud přistupujeme k autokorelaci na časové řadě pro nějaký shift:

### Příklad
Potřebuji zjistit autokorelaci náhodné složky pro následující 3 lagy. Dle frekvence časové řady pro kvartály platí: \( lag = (\text{kvartál} \Rightarrow 3 \text{ měsíce}) \).

Tím pádem získáme autokorelaci jak cílového kvartálu, tak i jednotlivých měsíců, které kvartál obsahuje.

$$
list[ACF_{result}] = \{ cor(X_{t}, X_{t-1}), cor(X_{t}, X_{t-2}), \dots, cor(X_{t}, X_{t-k}) \}
$$

### Výstup autokorelační funkce
Ukazuje, jak na sobě závisí hodnota na indexu \( t \) a \( t+shift \) pomocí korelačního koeficientu.

**Po dekompozici časové řady:** Pokud po dekompozici zkontrolujeme ACF reziduí, chceme vidět hodnoty blízké nule pro všechny lagy. To by znamenalo, že v reziduích neexistuje žádná struktura a že model zachytil vše důležité.

**Identifikace sezónnosti a trendu:** ACF také pomáhá identifikovat sezónní a cyklické vzory v datech. Pokud má ACF výrazné hodnoty na určitých lagách (např. každý 12. lag u měsíčních dat), může to ukazovat na sezónnost, kterou model možná ještě nezachytil.

**Lepší výběr modelu:** Po zhodnocení ACF reziduí můžeš vybrat vhodnější model. Například pokud ACF ukazuje, že rezidua jsou autokorelovaná, může být potřeba použít ARIMA model, který tuto závislost zohledňuje.

---

## Parciální korelace - prolog k PACF
Parciální korelace měří vztah mezi dvěma proměnnými, přičemž eliminuje vliv ostatních proměnných. Na rozdíl od běžné korelace, která zachycuje celkový vztah mezi dvěma proměnnými, parciální korelace ukazuje, jak silný je jejich vztah, pokud kontrolujeme (nebo udržíme konstantní) vliv dalších proměnných.

Např. vztah mezi \( \text{weight} \), \( \text{chol} \), \( \text{gender} \).

---

## Parciální autokorelace PACF
U parciální autokorelace nevypočítáváme od vektoru všechny členy příslušného lagu iterativně, ale přímo k indexu \( t_{t-shift} \) pomocí lineární regrese => jejich koeficientů příslušných lagů.

PACF odstraňuje vliv mezilehlých lagů a ukazuje, jak silný je vztah mezi \(t\) a \(t_{t-k}\) 

$$
S_{t} = \phi_{21} S_{t-1} + \phi_{22} S_{t-2} + \varepsilon_{t}
$$

**Legenda:**
- \( S_{t} \): hodnota časové řady v čase \( t \),
- \( \phi_{21}, \phi_{22} \): koeficienty autoregrese (vztahující se k předchozím hodnotám \( S \)),
- \( S_{t-1}, S_{t-2} \): hodnoty časové řady posunuté o 1 a 2 lagy,
- \( \varepsilon_{t} \): náhodná složka (šum) v čase \( t \).

### Výpočet PACF pro kvartál pomocí regresního modelu

1. **Vytvoření lineárního modelu**:
   Vytvořím lineární regresní model, který predikuje hodnotu \( S_t \) (např. za měsíc) na základě lagů (předchozích měsíců) až do zadaného lag \( k \). Pokud kvartál obsahuje 3 měsíce, použijeme 3 hodnoty \( S_{t-1}, S_{t-2}, S_{t-3} \) jako prediktory:
   
   $$ S_t = \beta_1 S_{t-1} + \beta_2 S_{t-2} + \beta_3 S_{t-3} $$

2. **Výpočet predikovaných hodnot**:
   Pro každou hodnotu \( S_t \) predikuji její hodnotu pomocí výše uvedeného modelu. Výsledkem je predikovaná hodnota \( \hat{S_t} \).

3. **Výpočet reziduí**:
   Vypočítám rezidua, což jsou rozdíly mezi skutečnými a predikovanými hodnotami:
   
   $$ \text{Rezidua} = S_t - \hat{S_t} $$

4. **Výpočet PACF**:
   Následně vypočítám korelaci mezi skutečnými hodnotami \( S_t \) a rezidui, abych získal parciální autokorelaci pro lag \( k \):
   
   $$ \text{PACF}_{k} = cor(S_t, \text{Rezidua}) $$

## 3. Uměli byste zapsat rovnice odhadnutých modelů TSLM? Co Vám tyto modely o řadách říkají? A jsou tyto modely dobré ve smyslu, že se residua chovají tak, jak mají?



