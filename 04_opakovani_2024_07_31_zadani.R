
# Načtení balíčků ---------------------------------------------------------
library(tidyverse)


# TODO načti si další potřebné balíčky
# pokud bys některý neměl nainstalovaný musíš první instalovat pomoci install.packages("nazev_balicku")


# Nápověda: Vybrané užitečné klávesové zkratky -------------------------------------------------------

# zobrazí klávesové zkratky                     Alt + Shift + k 
# zobrazí nápovědu u funkcí                     F1 
# ukládá                                        Ctrl + S
# vyhledávání                                   Ctrl + F
# spustí kód, ve kterém jsme                    Ctrl + Enter   
# spustí kód od začátku k řádku, v němž jsme    Ctrl + Alt + B    
# spustí kód od  řádku, v němž jsme do konce    Ctrl + Alt + E    
# spustí kód v celém skriptu                    Ctrl + Alt + R 
# pipe %>%                                      Shift + Ctrl + M
# operátor "nebo" |                             AltGr + W
# operátor "a" &                                AltGr + C
# zpětné lomítko \                              AltGr + Q
# přiřazení <-                                  Alt + -
# názvy sekcí v kódu                            Ctrl + Shift + R
# křížek pro komentování                        AltGr + X
# zakomentování a odkomentování řádků           Ctrl + Shift + C
# hranaté závorky []                            AltGr + F / AltGr + G
# složené závorky []                            AltGr + B / AltGr + N


# https://petrbouchal.xyz/eval2020/ klávesové zkratky





# úkoly -------------------------------------------------------------------

# 0. Načti data ze souboru "2024_04_pocty_ua_zaku_uprchliku_upraveno.xlsx" na úrovni ORP - soubor je ve složce data => input



# Nápověda (úkol 0) ----------------------------------------------------------------
# Jsi v projektu workshopy_R? 
# Máš zadanou celou cestu k souboru? (umístění, typ souboru)
# Volný řádek v záhlaví je potřeba přeskočit



# Kontrolní otázka --------------------------------------------------------
# Máš ve svém datasetu 227 řádků? Pokud ne, potřebuješ zřejmě načíst jiný list souboru nebo přeskočit prázdný první řádek


# 1. zbav se diakritiky v názvu proměnných
d <- d %>% 
  # xxx()

# 2. Prozkoumej chybějící hodnoty a pokud někde jsou, tak dané ORP odfiltruj pryč 


# 3. Když použiješ str() nebo glimpse () nebo obdobné funkce pro průzkum dat, sedí jednotlivé datové typy, nebo je potřeba něco změnit? (stačí 1 oprava)



# 4. přejmenuj proměnnou KON na konzervator  


# 5. Najdi 5 ORP a největším a nejmenším počtem dětí v MŠ (stačí k tomu mít kód, který je najde)

# 6. V ORP s největším počtem dětí v MŠ se vloudila chyba, číslo má mít o 2 nuly méně, oprav (např. pomocí mutate + if_else), ukázka kódu

# Nápověda (úkol 6)----------------------------------------------------------------
# Ukázka kódu k úpravě vybrané hodnoty, pokud se rovná nějaké hodnotě - kód níže sjednocoval různý zápis dané obce 
# v 1 datasetu se měnil do stejného formátu, jaký byl v druhém 
# Jak kód níže funguje? pokud si nejsi jistý, podívej se do nápovědy if_else nebo si nech  vysvětlit od chatgpt 

# název datasetu <- nazev datasetu %>% 
#     mutate(obec_mc = ifelse(obec_mc == "Praha-Újezd u Průhonic", "Praha-Újezd", obec_mc)) %>% 
 




# další úkoly -------------------------------------------------------------

# 7. Budeme chtít přidat proměnnou kraj - budeme potřebovat soubor, kde jsou kraje a orp propojené, např. zde

prevodnik <- read.xlsx("data/input/prevodnik.xlsx")

# 8 uprav převodník, tak, aby nejnižší jednotka byla ORP a uloz to do datasetu orp - měl/a bys získat dataset se 206 řádky (resp. 205 protože 1 jsme odfiltrovali)




# Nápověda (úkol 8) ----------------------------------------------------------------
# V tomhle případě nemáme žádné hodnoty, které bychom potřebovali sčítat, proto nepotřebuejeme group_by() a summarise()
# To, co chceme je, aby každý orp byl v datasetu 1x, chceme odstranit duplicitní ORP
# Pouzij funkci distinct - je potřeba ale pracovat jen s proměnnými, které nás zajímají - odstranit proměnne tykající se obci (nebo vybrat pouze ty ktere chceme)




# další tasks -------------------------------------------------------------

# 9. propoj nyní oba datsety pomocí funkce left_join (můžeš vyzkoušet dát na první místo dataset orp i d - v čem je rozdíl)
# podle které proměnné je propojujeme?




# 10. seskupme prahu v datasetu d Praha 1 až Praha 22 do orp s názvem Praha. 
# Můžeme rozdělit do 2 kroků - pokud proměnná ORP obsahuje nebo začíná na slovo Praha (xx) přepíšeme na "Praha" podobně jako v bodu 6, 
# spolu s funkcí str_detect nebo starts_with, poté pouzijeme group_by a summarise




# 11. opakuj nyní krok z bodu 9 - spojení datasetů 


# 12. podívej se na kraje, kde je u kraje chybějící hodnota, zde je nějaký problém. Jaký? Případně mrkni do nápovědy


# Nápověda (úkol 12) ----------------------------------------------------------------
# názvy orp se v obou datasetech liší - uprav je v datech tak, aby odpovídaly převodníku a poté opakuj spojení datasetů 

d_fin <- left_join(d_fin, orp, by = "orp")




# Pokračování -------------------------------------------------------------

# 13. vytvoř nový dataframe s nazvem kraje, kde budou součty dětí v ZŠ a  prumerne a medianove pocty dětí v ZŠ  v za jednotlivé kraje (reálně jde o průměry a mediány za ORP v kraji). 
# Zaokrouhli na 1 desetinné místo a seřaď sestupně podle průměrného počtu v ZŠ (od největšího po nejmenší)


  

# vizualizace - vycházej ze skriptů z minulého workshopu - vyber si graf, nebo mapu -------------------------------------------------------------


# # 13. GRAF: udělej sloupcový graf s počty dětí v ZŠ v jednotlivých krajích --------


# jestli bude horizontální nebo vertiální je na tobě
# vyber si barvu, která se ti líbí 
# uprav theme, jak se ti líbí 
# uprav název grafu, popisky os 
# uprav velikost a font písem, jak se ti líbí 



# # 14. MAPA: udělej mapu s počty dětí v MŠ v jednotlivých ORP pomocí balíčku tmap --------

# Poznámka: u propojování datasetů u tmap (např. pomocí left_join) je důležité, aby dataset s geospaciálními daty byl při spojení první - abychom připojovali k němu 

## Načti polygony (hranice) ORP --------------------------------------------

# vyber si paletu, která se ti líbí - bud z pouzivanych, nebo si definuj vlastni 
# styl který se ti líbí - např pretty, ordered, fixed aj. (u fixed si můžeš definovat vlastní hranice intevralů - tzv. breaks) aj. 
# uprav název mapy


# zobrazení všech barevných palet 
# display.brewer.all()

# Zobrazení konkrétní palety - zde "Blues" se zadnaým počtem barev - zde v příkladu 5, uložení dané palety
# display.brewer.pal(n = 5, name = "Blues")
# blues7 <- get_brewer_pal("Blues", n = 7)
