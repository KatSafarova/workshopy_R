
# načtení balíčků ---------------------------------------------------------
library(dplyr) # data transformation https://nyu-cdsc.github.io/learningr/assets/data-transformation.pdf, https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
library(openxlsx) # práce s xlsx soubory, read.xlsx ()
library(tidyr) # transformace dat do tidy formátu https://github.com/rstudio/cheatsheets/blob/main/tidyr.pdf , drop_na()
library(lubridate) # úprava datumů https://rstudio.github.io/cheatsheets/lubridate.pdf 
library(janitor) # čištění dat, clean_names() 
library(skimr) # souhrnné statistiky  
library(stats)
library(tidyverse)


# Krok 1, pokud s R začínáš ------------------------------------------------
# Projdi si znovu podklady z 1. workshopu - skript s názvem 01_import_cisteni_zakl_transformace.R", ppt prezentaci a nasdílený záznam z workshopu (je na 2 části)

# Konkrétní procvičovací úkoly
# 
# Import dat --------------------------------------------------------------
# Nahraj do objektu d1 pomocí soubor v textovém formátu countries.txt ze slozky data/input (pomocí read.csv nebo read.csv2)
## S tímhle datasetem v procvičování pracujeme, dataset je více popsaný zde https://sociology-fa-cu.github.io/uvod-do-r-kniha/data.html 

# nezapoměň přistupovat ke skriptům přes projekt (soubor, který má u typu uvedený "R projekt")
d1 <- read.csv("data/input/countries.txt")
d1 <- read.csv2("data/input/countries.txt", sep = ",")

# Nahraj do objektu d2 soubor v Rkovém formátu .rds kraje_pocty_projektu_cvicna_data.rds ze slozky data/processed 


d2 <- readRDS("data/processed/kraje_pocty_projektu_cvicna_data.rds")


# Klávesové kratky -------------------------------------------------------
# Napiš %>% (budeme příště více používat)

# ctrl + shift + M %>% 

# Odkomentuj tuto část kódu
 # ctr + shift + c pro odkomentování nebo zakomentování 


# Proměnné a datové typy --------------------------------------------------
# Zjisti jaký datový typ je proměnná eu_member v nahraném datasetu countries

class(d1$eu_member)

# Kolik hodnot má proměnná maj_belief? 

table(d1$maj_belief)

pocet_hodnot <- length(unique(d1$maj_belief))

print(pocet_hodnot)


# čištění dat a identifikace chybějících hodnot-------------------------------------------

# Ulož do nového objektu s názvem d3 stávající dataframe bez řádků s chybějícími hodnotami v proměnné gdp, která označuje HDP dané země
d3 <- d1 %>% 
  filter(!is.na(gdp))


# rm(d1, d2)

# Zakomentuj rm(d) výše a spusť skript znovu, můžeš zkusit zkratku Ctrl + Alt + B   




# další operace ------------------------------------------------------------

# vyfiltruj pouze země s vyšším poverty risk než 0,3, ulož to do nového dataframu d4

d4 <- d3 %>% 
  filter(poverty_risk > 0.3)

# Přejmenuj proměnnou country na zeme 

d5 <- d3 %>% 
  rename(zeme = country)

d5$life_exp <- as.numeric(d5$life_exp)

# Spočítej průměr průměrnou naději dožití (life_exp) pro postsovětské a nepostsovětské země, pojmenuj ji life_exp_mean (ideální použít group_by), poraď se případně s chtagpt
d6 <- d5 %>% 
  group_by(postsoviet) %>% 
  summarise(life_exp_mean = mean(life_exp, na.rm = TRUE)) %>% 
  ungroup()


# seřaď dataset sestupně a vzestupně podle hodnoty proměnné gdp (můžeš se podívat do nápovědy funkce arrange přes ?arrange)
d7 <- d5 %>% 
  arrange(desc(gdp)) 

# Vytvoř novou proměnnou s názvem hustota_zal, ve které budeš dělit population proměnnou area - najdi nejnižší a nejvyšší hodnotu 

d8 <- d7 %>% 
  mutate(hustota_zal = population/area) 

# možnost přidat funkci round pro zaoukrouhlení, digits udává počet desetinných míst, relocate pro přesun proměnné
d8 <- d8 %>% 
  mutate(hustota_zal = round(population/area, digits = 2)) %>%
  relocate(hustota_zal, .after = area)

# Vypočítej jaký je rozdíl v letech mezi foundation date a 2024 - ulož číslo do nové proměnné stari_statu

d9 <- d8 %>% 
  mutate(stari_statu = 2024 - year(foundation_date))

# ulož si výsledek do datasetu s nazvem procvicovani_1, zkus si ho ulozit jako csv, xlsx i rds soubor 

saveRDS(d9, "data/processed/procvicovani_01.rds")
write.xlsx(d9, "data/processed/procvicovani_01.xlsx")


# Studium zhruba na hodinku-----------------------------------------------------------------
# Projdi si kapitoly 2-9 zde https://sociology-fa-cu.github.io/uvod-do-r-kniha/rstudio.html 


# další setkání je 12. 6. od 14 hodin opět v MPSV Na poříčním právu, doděláme základní transformace ze skriptu 01 (skončili jsme okolo řádku 290) a pak půjdeme na grafy a mapy 
