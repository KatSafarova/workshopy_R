
# Načtení balíčků ---------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(tmap)
library(janitor)


# TODO načti si další potřebné balíčky
# pokud bys některý neměl nainstaovaný musíš první instalovat pomoci install.packages("nazev_balicku")


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

d <- read.xlsx("data/input/2024_04_pocty_ua_zaku_uprchliku_cvicny_dataset.xlsx", startRow = 2, sheet = "orp")

# poznámka: můžeš použít i balík readxl - ten je o něco rychlejší,
# data vrací ve formátu tibble (je to varianta data.frame, lépe se zobrazuje v konzoli)
# a balík se o něco snadněji instaluje.
# Na ukládání dat do excelu má brášku/ségru s názvel writexl.


# Nápověda (úkol 0) ----------------------------------------------------------------
# Jsi v projektu workshopy_R? 
# Máš zadanou celou cestu k souboru? (umístění, typ souboru)
# Volný řádek v záhlaví je potřeba přeskočit



# Kontrolní otázka --------------------------------------------------------
# Máš ve svém datasetu 227 řádků? Pokud ne, potřebuješ zřejmě načíst jiný list souboru nebo přeskočit prázdný první řádek


# 1. zbav se diakritiky v názvu proměnných
d <- d %>% clean_names()

# 2. Prozkoumej chybějící hodnoty a pokud někde jsou, tak dané ORP odfiltruj pryč 

skimr::skim(d) # viz sloupec n_missing a complete_rate

d |> 
  filter(is.na(ms) | is.na(zs) | is.na(ss))

d <- d %>% 
  drop_na(ms)

# 3. Když použiješ str() nebo glimpse () nebo obdobné funkce pro průzkum dat, sedí jednotlivé datové typy, nebo je potřeba něco změnit? (stačí 1 oprava)

d <- d %>% 
  mutate(zs = as.numeric(zs))

# 4. přejmenuj proměnnou KON na konzervator  

d <- d %>% 
  rename(konzervator = kon)

# 5. Najdi 5 ORP a největším a nejmenším počtem dětí v MŠ (stačí k tomu mít kód, který je najde)

# 6. V ORP s největším počtem dětí v MŠ se vloudila chyba, číslo má mít o 2 nuly méně, oprav (např. pomocí mutate + if_else), ukázka kódu

# Nápověda (úkol 6)----------------------------------------------------------------
# Ukázka kódu k úpravě vybrané hodnoty, pokud se rovná nějaké hodnotě - kód níže sjednocoval různý zápis dané obce 
# Jak kód níže funguje? pokud si nejsi jistý, podívej se do nápovědy if_else nebo si nech  vysvětlit od chatgpt 

# název datasetu <- nazev datasetu %>% 
#     mutate(obec_mc = ifelse(obec_mc == "Praha-Újezd u Průhonic", "Praha-Újezd", obec_mc)) %>% 
 




# další úkoly -------------------------------------------------------------

# 7. Budeme chtít přidat proměnnou kraj - budeme potřebovat soubor, kde jsou kraje a orp propojené, např. zde

prevodnik <- read.xlsx("data/input/prevodnik.xlsx")

# 8 uprav převodník, tak, aby nejnižší jednotka byla ORP a uloz to do datasetu orp - měl/a bys získat dataset se 206 řádky (resp. 205 protože 1 jsme odfiltrovali)

orp <- prevodnik %>% 
  select(orp, kraj) %>% 
  distinct()


# Nápověda (úkol 8) ----------------------------------------------------------------
# V tomhle případě nemáme žádné hodnoty, které bychom potřebovali sčítat, proto nepotřebuejeme group_by() a summarise()
# To, co chceme je, aby každý orp byl v datasetu 1x, chceme odstranit duplicitní ORP
# Pouzij funkci distinct - je potřeba ale pracovat jen s proměnnými, které nás zajímají - odstranit proměnne tykající se obci (nebo vybrat pouze ty ktere chceme)




# další tasks -------------------------------------------------------------

# 9. propoj nyní oba datsety pomocí funkce left_join (můžeš vyzkoušet dát na první místo dataset orp i d - v čem je rozdíl)
# podle které proměnné je propojujeme?


d <- left_join(d, orp, by = "orp")


# 10. seskupme prahu v datasetu d Praha 1 až Praha 22 do orp s názvem Praha. 
# Můžeme rozdělit do 2 kroků - pokud proměnná ORP obsahuje slovo Praha (xx) přepíšeme na "Praha" podobně jako v bodu 6, 
# spolu s funkcí str_detect nebo starts_with, poté pouzijeme group_by a summarise

d <- d %>%
  mutate(orp = ifelse(str_detect(orp, "Praha"), "Praha", orp))

d_fin <- d %>% 
  group_by(orp) %>%
  summarise(
    ms = sum(ms, na.rm = TRUE),
    zs = sum(zs, na.rm = TRUE),
    ss = sum(ss, na.rm = TRUE),
    konzervator = sum(konzervator, na.rm = TRUE),
    .groups = 'drop'
  )

# alternativně se dají sečíst všechny numerické hodnoty
# d_fin <- d %>%
#   group_by(orp) %>%
#   summarise(across(where(is.numeric), sum, na.rm = TRUE))


# 11. opakuj nyní krok z bodu 9 - spojení datasetů 

d_fin <- left_join(d_fin, orp, by = "orp")

# 12. podívej se na kraje, kde je u kraje chybějící hodnota, zde je nějaký problém. Jaký? Případně mrkni do nápovědy

kraj_na <- d_fin %>% 
  filter(is.na(kraj))


# Nápověda (úkol 12) ----------------------------------------------------------------
# názvy orp se v obou datasetech liší - v 1 je změň podle druhého datasetu (je jedno v jakém je budeš měnit) a poté opakuj spojení datasetů 

d_fin <- d_fin %>%
  select(-kraj) %>% 
  mutate(orp = if_else(orp == "JAROMĚŘ", "Jaroměř", orp),
         orp = if_else(orp == "Brandýs n.L.-St.Boleslav", "Brandýs nad Labem-Stará Boleslav", orp))

d_fin <- left_join(d_fin, orp, by = "orp")




# Pokračování -------------------------------------------------------------

# 13. vytvoř nový dataframe s nazvem kraje, kde budou součty dětí v ZŠ a  prumerne a medianove pocty dětí v ZŠ  v za jednotlivé kraje (reálně jde o průměry a mediány za ORP v kraji). 
# Zaokrouhli na 1 desetinné místo a seřaď sestupně podle průměrného počtu v ZŠ (od největšího po nejmenší)

kraje <- d_fin %>%
  group_by(kraj) %>%
  summarise(
    suma_zs = sum(zs, na.rm = TRUE),
    prumer_zs = round(mean(zs, na.rm = TRUE), digits = 1),
    median_zs = round(median(zs, na.rm = TRUE), digits = 1)
  ) %>% 
  arrange(desc(prumer_zs))


  

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

orp <- RCzechia::orp_polygony()

# vyber si paletu, která se ti líbí - bud z pouzivanych, nebo si definuj vlastni 
# styl který se ti líbí - např pretty, ordered, fixed aj. (u fixed si můžeš definovat vlastní hranice intevralů - tzv. breaks) aj. 
# uprav název mapy


# zobrazení všech barevných palet 
# display.brewer.all()

# Zobrazení konkrétní palety - zde "Blues" se zadnaým počtem barev - zde v příkladu 5, uložení dané palety
# display.brewer.pal(n = 5, name = "Blues")
# blues7 <- get_brewer_pal("Blues", n = 7)
