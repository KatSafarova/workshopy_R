library(dplyr)
library(stringr)
library(openxlsx)
library(readxl) # obsahuje excel_sheets()
library(writexl)
library(janitor)
library(lubridate)
library(gt)
# library(gtExtras)
library(glue)
library(purrr)

options(scipen = 999) # ne vědecké zobrazení čísel

# cvičný dataset, k cestování do vybraných měst
d_raw <- read.xlsx("data/input/cestovani.xlsx")

# TODO ÚKOLY 
# 1. pročistit soubor 
#  1.1 odstranit z proměnných diakritiku
#  1.2 odstranit kompletní duplikáty
#  1.3 ošetřit textové hodnoty - odstranit přebytečné mezery, sjednotit velikost písmen

# 2. dopočítat nové proměnné 
#  2.1 cenu pro auto podle spotřeby 7l/100 km a ceny benzínu 36,40
#  2.2 vypočíst sloupce cena_1_osoba a cena_3_osoby podle počtu cestujících (u auta je to jinak než u ostatních)
#  2.3 připočíst do cas_fin zpoždění 10 % pro cestu autem do Brna a do Jihlavy, ostatní časy přenést

# 3. vyjádřit čas v minutách 
# 4. sjednotit data (datumy)
# 5. vyfiltrovat jen cesty, které trvají pod 150 minut a seřadit vzestupně dle doby cestování 
# 6. seřadit podle ceny pro 1 osobu a pro 3 osoby (vytvořit 2 datasety seřazené podle ceny pro 1 a pro 3 osoby)
# 7. další finální úpravy - odstranit sloupce, které ve finálních datech nechceme mít apod.

# 8. využít podmíněné formátování tabulky s balíčkem gt, barevně ji podbarvit podle dopravních prostředků (řádky s různými dopravními prostředky jinak barevně, celkem potřeba 3 barvy), upravit titulek, a přidat poznámku, že jde o data z vyhledávače idos.cz k 10. 10. 2024
# 9. uložit cesty vlakem, autem, busem do 1 excelu který bude mít 3 sheety (1 dopravní prostředek 1 sheet)
# 10 získat a uložit pomocí cyklu for i pomoci map() z 1 společného dataframu 3 excel soubory (pro kazdy dopravní prostředek 1)


# Nápověda funkce ---------------------------------------------------------
# možné funkce k použití (výběr)
# clean_names()
# unique()
# mutate()
# where(is.character)
# as.numeric()
# as.character()
# dmy()
# as.Date()
# select()
# across()
# class()
# distinct()
# filter()
# str_to_title()
# trimws()
# arrange()
# ifelse()
# case_when()
# excel_numeric_to_date()
# read.xlsx()
# map()
# map_chr()
# walk()



# čištění názvů proměnných a odstranění duplikátů  ------------------------

# ošetření textových proměnných -------------------------------------------------------
# ostranění přebytečných mezer na začátku a na konci 


# sjednocení velikosti písmen v hodnotách, kde se vyskytuje více variant


# dopočtení nových sloupců ------------------------------------------------
# cena za cestu autem dle spotřeby a ceny benzinu 
benzin <- 36.4
spotreba <- 7

# delší čas cesty o 10 % při jízdě autem do Brna a do Jihlavy kvůli zácpám 
# cena pro 1 cestujícího a pro 3


# převod času na minuty ---------------------------------------------------
d$cas
# máme data v tomto zvláštním formátu

# Je to způsobený tím, že Excel ukládá časy jako zlomek dne, což znamená, že 2:35 (2 hodiny a 35 minut) je uložený jako podíl z 24 hodin. V R je načten jako 0.1076388888888889, což je asi 10,76 % z celkového dne.

# zároveň je tam i chyba, čísla nad 1 nejsou podíl z 24 hodin, ale minuty


# Chceme převést čas na minuty. V této části je vhodné také do nove promenne cas_fin připočíst zpoždění pro cestu autem do Jihlavy a Brna a dále pracovat s cas_fin()

# Pokud d$cas * 24 je 12.75, znamená to 12 hodin a 0.75 hodiny, což odpovídá 45 minutám (0.75 * 60 = 45).



# sjednocení dat (datumů) -------------------------------------------------

unique(d$datum) # jakých unikátních hodnot nabývá datum 

# Nahradíme názvy měsíce číslem pro snadnější úpravy
d$datum <- gsub("října", "10.", d$datum)

# je potřeba hodnoty sjednoti, možné použít funkci excel_numeric_to_date() z balíčku janitor 


# Vysvětlení k Excel číslům:
#   Excel používá číselný formát dat, kde je každý den reprezentován jako pořadové číslo počínaje 1. lednem 1900. Čísla reprezentují počet dní od tohoto data. Například:
# 1. leden 1900 je v Excelu reprezentován jako číslo 1.
# 1. leden 1970 je reprezentován jako číslo 25569, což odpovídá 25 569 dní po 1. lednu 1900.
# 15. říjen 2024 je v Excelu reprezentován jako číslo 45580, což je 45 580 dní po 1. lednu 1900.


# seřazení datasetů dle ceny pro 1 a pro 3 -----------------------------------------------------


# final úpravy dataframů pro hezkou formátovanou tabulku ---------

# odstranění přebytečných proměnných, seřazení, odstranění přebytečných desetinných míst apod.

# gt tabulka s formátováním --------------------------------------------------

# zápis všech dopravních prostředků do jednotlivých listů 1 excelu (co prostředek to sheet) ------------------------------------------

# Načtení všechn listů z excelu výše do 1 dataframu 


# uložit každý dopravní prostředek do samostatného excel souboru pomocí for/map/walk----------


