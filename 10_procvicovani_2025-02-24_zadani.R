
# načtení balíčků ---------------------------------------------------------

library(readr) # načítání csv a dalších tabulkových dat, funkce read_csv, read_csv2

library(dplyr) # Umožňuje efektivní filtrování, seskupování, tvorbu nových proměnných a další datové transformace. Funkce  filter(), mutate(), arrange(), group_by() a summarise() 

library(tidyr) # Pomáhá s úpravou a čištěním dat tak, aby odpovídala principům "tidy data". # Funkce jako pivot_longer(), pivot_wider(), separate() a unite() slouží ke změně struktury tabulek.

library(lubridate) # Zjednodušuje manipulaci s datumy a časy.Hlavní funkce jako ymd(), mdy(), hms() a floor_date() usnadňují konverzi a výpočty s datem a časem.

library(stringr) # práce s textem. Zjednodušuje manipulaci s textem oproti base R, který má nejednotné funkce pro textové operace. Hlavní funkce jako str_detect(), str_replace(), str_extract() a str_split() umožňují vyhledávání, náhradu a rozdělování textu.

library(forcats) #  Práce s faktory. Usnadňuje manipulaci s faktory, které jsou v R často komplikované a neintuitivní. Hlavní funkce jako fct_relevel(), fct_lump(), fct_reorder() a fct_collapse() umožňují efektivní třídění, seskupování a přeuspořádání úrovní faktorů.

library(ggplot2) # grafy 

library(janitor) # pro čištění a přípravu dat, nepatří do tidyverse. Funcke clean_names(), get_dupes(), tabyl(), remove_empty()


# dále můžeme přidat obecná nastavení, např týkající se zobrazení čísel 
options(scipen = 999) # ne vědecké zobrazení čísel


# vybrané užitečné klávesové zkratky -------------------------------------------------------

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
# složené závorky {}                            AltGr + B / AltGr + N


# načtení dat -------------------------------------------------------------

# Data z registru ekonomických subjektů (RES) odsud Seznam ekonomických subjektů 
# https://csu.gov.cz/statistika/registr-ekonomickych-subjektu-otevrena-data-dokumentace - pro workshop náhodně vybraných 10 000 řádkl 

d1 <- read.csv("data/input/rejstrikova_data_ekonomicke_subjekty/res_data_vzorek_10000_zaznamu.csv")

d2 <- read.csv("data/input/rejstrikova_data_ekonomicke_subjekty/res_data_vzorek_10000_zaznamu.csv", colClasses = c("ICO" = "character"))


guess_encoding("data/input/rejstrikova_data_ekonomicke_subjekty/res_data_vzorek_10000_zaznamu.csv")

d3 <- read_csv("data/input/rejstrikova_data_ekonomicke_subjekty/res_data_vzorek_10000_zaznamu.csv") 


# vybrané číselníky k rejstříkovým datům
c_okresy <- read.csv("data/input/rejstrikova_data_ekonomicke_subjekty/CIS0109_CS.csv")
c_zpusob_zaniku <- read.csv("data/input/rejstrikova_data_ekonomicke_subjekty/CIS0572_CS.csv")
c_pocet_prac <- read.csv("data/input/rejstrikova_data_ekonomicke_subjekty/CIS0579_CS.csv")


# průzkum dat -------------------------------------------------------------
str(d1)
str(d3)

# glimpse(d1)

d <- d3 |> 
  clean_names()

# ÚKOL: odstraň dataframy d1, d2, d3



colnames(d)
head(d)

# když bychom chtěli u head() vidět všechny proměnné, můžeme přenastavit výchozí zobrazení
# options(tibble.width = Inf)
# head(d)  # Zobrazí prvních 10 řádků a všechny sloupce
# options(tibble.width = NULL) # následně se zase můžeme vrátit do výchozího


# kolik máme unikátních okresů?
n_distinct(d$okreslau)
length(unique(d$okreslau)) # alternativně 


# zobraz si unikátní hodnoty ve sloupci forma, zjisti následně jaká je to datová třída a převeď to na třídu factor pomocí as.factor()




# FILTER ------------------------------------------------------------------

# Najdi a oprav chyby, chceme vyfiltrovat, to, co je v poznámce u textu, všímej si tříd proměnných, čárek, závorek, uvozovek, operátorů...
d |> 
  filter("forma" = "101") |>  # právnická forma 101
  filter(is.na(okreslau))  # bez chybějích hodnot u okreslau 


d |> 
filter(!obec_t = "Praha")  # všechny obce mimo Prahu

d |> 
  filter(obec_text %in% c("Praha" "Brno", "Ostrava"))  # konkrétní obce Brno, Praha, Ostrava

d |> 
  filter(str_starts(nace, "4") # vyber subjekty co mají nace kód začínající na 4 
 
 d |> 
   filter(str_detect(nazev_firmy, "Martin")) # vyfiltruj firmy co mají v názvu "Martin"
 
d %>% select(nchar(as.character(kodadm)) > 5) # vyfiltruj případy, kdy má proměnná kodadm (kód adresního místa) víc než 5 znaků
 
 
# Udělej sám/a
# Vyfiltruj firmu s názvem  Ilona Bažíková
# Vyfiltruj řádky s chybějící hodnotou v ddatzan (datum zániku)
# Vyfiltruj řádky, co nemají chybějí hodnotu u názvu obce 
# Vyfiltruj obce začínající na "St"
# Vyfiltruj PSČ menší než 14200 
 









# SELECT ------------------------------------------------------------------
# Najdi a oprav chyby 
d |> 
  select(ico, okres_lau) # vyber sloupce ico a okreslau
 
 d |> 
   select(priznak) # odstraň sloupec priznak (ostatní ponechej)
 
d |> 
  select(starts_with(f)) # vyber sloupce začínající na písmeno f

d |> 
  select(-katpo, ciss2010) # odstraň tyto dva sloupce

d |> 
  select(where(is.numeric) # vyber všechny číselné sloupce
         
# Udělej sám/a
# Odstraň sloupce forma a rosforma
# Vyber sloupce psc, ulice_text  
# Vyber všechny sloupce co končí na _text 






# RENAME a RENAME_WITH ----------------------------------------------------

# Najdi a oprav chyby,
d <- d |> 
  rename(okreslau = okres_kod) # přejmenuj okreslau na okres_kod

# Udelej sama: 
# Přejmenuj ddatvzn na datum_vzniku 
# Přejmenuj katpo na pocet_zamestnancu






# ukázka přejmenování více (zde všech) proměnných s rename_with()
d_kapitalky <- d |> 
  rename_with(toupper) # není li specifikováno vyberou se všechny sloupce


# složitější výběr všech proměnných končících na _text a jejich přejmenování  aby končily jen na _t

# d <- d |>
#   rename_with(str_replace, ends_with("_text"), "_text$", "_t")
# # kombinace funkce z balíčku stringru, znak $ označuje konec textového řetězce
# 
# d <- d |>
#   rename_with(function(x) str_replace(x, "_text$", "_t"), ends_with("_text"))
# # použití anonymní funkce

# PROPOJENÍ DAT S LEFT_JOIN ------------------------------------------------

## vyber z číelníků pro zjednodušení jen hodnoty, které tě zajímá - hodnotu která je v původních datech (kód) a význam - nahrad proměnné x, pokud chceš tak si je i přejmenuj  ----------------------------------

c_okresy <- c_okresy %>%
  select(x1,
         x2)

c_pocet_prac <- c_pocet_prac %>%
  select(x1,
         xě)

c_zpusob_zaniku <- c_zpusob_zaniku %>%
  select(x1,
         x2)


## UKÁZKA propojení datasetu a číselníků   -----------------------------------------

d <- d |> 
  left_join(c_okresy, by = c("okreslau" = "okres_kod")) # pokud je klíč k propojení datasetů v obou pod ruznými návzy dáváme je do toho by do c( s =, první je název v datasetu, kte kterému připojujeme)

# alternativně
d <- left_join(d, c_okresy, by = c("okreslau" = "okres_kod"))

# Udělej sám/sama 
# Připoj podobně ostatní dva číselníky 
# pokud je pOTřeba sjednotit class můžeš použít kód ve stylu
# d <- d |> 
#   mutate(katpo = as.numeric(katpo))







# MUTATE ------------------------------------------------------------------

# vytvoř proměnnou rok vzniku podle promenne ddatvzn (datum_vzniku)
# Rok můžeš extrahovat pomocí funkce z balíčku lubridate




# UKÁZKA změny (rekodování) hodnoty Neuvedeno v počtu zaměstnanců na 0, nejdřív pro jaký případ to chceme změnit (podmínka), jak to změnit, když to platí a co se má stát když to neplatí 
d <- d |> 
  mutate(pocet_zam = if_else(pocet_zam == "Neuvedeno", "0", pocet_zam))

# Udělej sám/sama 
# Vytvoř pocet_zam_kat, která spojí skupiny 1-5 a 6-9
# a 20 - 24 a 25 až 49, ostatní nehct tak jak jsou 
# pouzij na to mutate a case_when 
# převed to následně na class faktor a definuj levely, aby to šlo vzestupně (to se hodí pak např. v grafech)


















# ÚKOL: zjisti kolik ekonomických subjektů vzniklo v roce 2024 










# GROUP_BY + SUMMARISE ----------------------------------------------------

# UKÁZKA agregace, ze ktere zjistíme, kolik subjektů vzniklo v jednotlivých letech, seřadíme sestupně podle počtu

pocty_vzniku_roky <- d %>%
  group_by(rok_vzniku) |> 
  summarise(pocet = n()) |> 
  arrange(desc(pocet)) 

# Další úkol k tomu Odfiltruj subjekty které mají NA v proměnné datum vzniku 



# Udělej sám/a
# Agreguj data na úrovni kódu a názvu okresu a spočítej kolik je v jednotlivých okresech jedinečných ičo a seřaď to sestupně 



