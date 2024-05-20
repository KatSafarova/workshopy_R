library(dplyr) # data transformation https://nyu-cdsc.github.io/learningr/assets/data-transformation.pdf, https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
library(openxlsx) # práce s xlsx soubory, read.xlsx ()
library(tidyr) # transformace dat do tidy formátu https://github.com/rstudio/cheatsheets/blob/main/tidyr.pdf , drop_na()
library(lubridate) # úprava datumů https://rstudio.github.io/cheatsheets/lubridate.pdf 
library(janitor) # čištění dat, clean_names() 
library(RCzechia) # využívá se hlavně pro tvrobu map, obsahuje szenam obcí, orp, krajů 
library(skimr) # souhrnné statistiky  
library(stats)
# library(stringr) # práce s textovýmni proměnnými
# library(reschola)

# při načtení projektů se nám ve výchozím nastavení soubory načítají a ukládají do složky s projektem, můžeme ověřit pomocí 

getwd()


# import dat  -------------------------------------------------------------

# načítám dataset ve formát csv

d <- read.csv2("data/input/cvicny_dataset.csv", header = TRUE, sep = ";", na.strings = c("", "NA"), encoding = "UTF-8")
# d <- read.csv2 ("C:/Users/katerina.safarova/Documents/GitHub/workshopy_R/data/input/cvicny_dataset.csv")
# další alternativy read.csv(), read.table()


# načítám dataset ve formtá xlsx - funkce z balíčku openxls
d2 <- read.xlsx("data/input/cvicny_dataset.xlsx", na.strings = c("", "NA")) # pokud máme více listů načítá se automaticky 1., pokud chceme jiný potřeba definovat 
d2 <- read.xlsx("data/input/cvicny_dataset.xlsx", sheet = "vsichni", na.strings = c("", "NA"))

d3 <- read.xlsx("data/input/cvicny_dataset_priklad.xlsx")

# pokud potřebujeme přeskočit řádky, lze použít argument startrow 
d3 <- read.xlsx("data/input/cvicny_dataset_priklad.xlsx", startRow = 3)


# odstraníme nepotřebné datasety z global environment
rm(d2, d3)



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



# průzkum dat -------------------------------------------------------------

# View(d)

head(d)

tail(d)

glimpse(d)

str(d)

summary(d)

skim(d)



# čištění dat a identifikace chybějících hodnot-------------------------------------------

# úprava názvů proměnných 
d <- d %>% 
  clean_names()

table(d$pohlavi)


# chybějící hodnoty
# přeskočí všechny řádky, kde je nějaké NA, stejně funguje funkce complete.cases()
d1 <- d %>% 
  na.omit()

d2 <- d %>% filter(complete.cases(d))

# přeskočí řádky s NA v dané proměnné 
d3 <- d %>% 
  drop_na(pohlavi)

# filter + is.na vyfiltruje pouze řádky s chybějícími hodnotami, opak (řádky mimo chybějící hodnoty) by byl !is.na
d4 <- d %>% 
  filter(is.na(datum_narozeni))

d5 <- d %>% 
  filter(!is.na(datum_narozeni))


# ÚKOLY 
# 1. zjisti, kolik chybějících hodnot je v proměnné pohlavi v tomto datasetu
# 2. Načti si do R  cvičně další csv a xlsx soubor



# počty chybějících hodnot v proměnných 
# ve sloupcích
na_counts <- colSums(is.na(d))

# Vytvoření dataframe s názvy proměnných a počty NA hodnot
na_counts_df <- data.frame(
  promenna = names(na_counts),
  na_pocet = na_counts
)

# v řádcích 
d <- d %>% 
  mutate(na_resp = rowSums(is.na(d)))

resp_s_na <- d %>% 
  filter(na_resp > 0)
  
d <- d %>% 
  filter(na_resp < 2)

# duplikáty podle ID
duplikaty <- d %>% 
  get_dupes(id) %>% 
  distinct(id)

duplikaty2 <- d %>%
  group_by(id) %>%
  filter(n() > 1)


# Odstranění úplných duplikátů 
d <- distinct(d, .keep_all = TRUE)

# Odstranění opakujících se hodnot podle proměnné orp
d_jedinecne_orp <- distinct(d, orp, .keep_all = TRUE)

rm(d1, d2, d3, d4, d5, na_counts_df, resp_s_na, duplikaty, duplikaty2, d_jedinecne_orp)


# ÚKOLY 
# 1. Zjisti, která orp se v datasetu opakují 



# úprava stávajících proměnných a tvorba nových s mutaate -----------------------------------------------------------

table(d$pohlavi)

# změna kódů na hodnoty u pohlaví
d <- d %>%
  mutate(pohlavi = factor(pohlavi, levels = c(1, 2)))

class(d$pohlavi)
levels(d$pohlavi)


d <- d %>%
  mutate(pohlavi = factor(pohlavi, levels = c(1, 2), labels = c("ženy", "muži")))

levels(d$pohlavi)


# změna character proměnných u vzdělání na faktor
class(d$vzdelani)
table(d$vzdelani)

d <- d %>%
  mutate(vzdelani_f = factor(vzdelani, levels = c("ZŠ", "SŠ", "VŠ")))

d$vzdelani

# vidíme, že v hodnotách jsou mezery, potřebujeme je odstranit, 
# odtraníme rovnou počáteční a konečné z celého datasetu
d <- d %>%
  mutate_all(trimws)


d <- d %>%
  mutate(vzdelani_f = factor(vzdelani, levels = c("ZŠ", "SŠ", "VŠ")))

# podle faktorů můžeme řadit 
d_serazeny <- d %>%
  arrange(vzdelani_f)

rm(d_serazeny)


# nová proměnná rok_narození pro výpočet věku, funkce z balíčku lubridate
d$rok_narozeni <- year(dmy(d$datum_narozeni))
d$mesic_narozeni <- month(dmy(d$datum_narozeni))
d$den_narozeni <- day(dmy(d$datum_narozeni))


d <- d %>% 
  mutate(vek = 2024 - rok_narozeni)

# d$rok1 <- as.numeric(substr(d$datum_narozeni, start = 7, stop = 10))


# ÚKOLY
# 1. vytvoř novou proměnnou vzdelani_f2 která bude mít hodnoty v tomto pořadí SŠ, ZŠ, VŠ
# 2. vytvoř novou proměnno, která bude mít hodnoty u pohlaví žena a muž 
# 3. zjisti jakou třídu (class) má proměnná id 



# přejmenování a rozdělování a spojování buněk ----------------------------

d <- d %>% 
  rename(jmenoprijmeni = jmeno)

# rozdělení jména a příjmení do dvou buněk 
d <- separate(d, jmenoprijmeni, into = c("jmeno", "prijmeni"), sep = " ",  remove = FALSE)


# spojení 
d <- unite(d, jmeno_prijmeni, jmeno, prijmeni, sep = " ", remove = FALSE)


# přejmenování 
d <- d %>% 
  rename(vzdelani_3kat = vzdelani_f)

# nová proměnná na zákaldě hodnot jiné proměnné
d <- d %>% 
  mutate(vzdelani_vs = ifelse(vzdelani_3kat == "VŠ", "Ano", "Ne"))


# upráva délku spánku 
class(d$obvykla_delka_spanku)

# potřebujeme převést na číselný formát
d$obvykla_delka_spanku <- as.numeric(gsub(",", ".", d$obvykla_delka_spanku))


# ÚKOLy
# 1. přejmenuj proměnnou id na id_respondenta
# 2. udělej dichotomickou proměnnou vzdelani_ss s hodnotami 0 a 1 (1 pokud má SŠ vzdělání)


# funkce filter a select --------------------------------------------------

# filter pro výběr řádků s danými parametry
# x rovná se
zeny <- d %>% 
  filter(pohlavi == "ženy")

# nerovná se, všechny s ostatní hodnotou
ne_muzi <- d %>% 
  filter(pohlavi != "muži")

d$id <- as.numeric(d$id)

# čísla nejsou v uvozovkách
vybrane_id <- d %>% 
  filter(id == 10 )

# více čísel
vybrana_id <- d %>% 
  filter(id %in% c(5:8))

# mimo těchto více čísel
vybrana_id2 <- d %>% 
  filter(!id %in%c(5:8))


# kombiance parametrů
vybrana_id3 <- d %>% 
  filter(id !=10 & vzdelani_3kat %in% c("ZŠ", "SŠ"))

# kombiance parametrů
vybrana_id4 <- d %>%
  filter(vek %in% c(30:40) & pohlavi == "ženy")

# vybrana_id5 <- d %>%
#   filter(vek <= 40 & vek >= 30 & pohlavi == "ženy")

# select pro výběr proměnných
d_vyber <- d %>% 
  select(id, obvykla_delka_spanku)

d_vyber2 <- d %>%
  select(where(is.character)) 

# proměnné začínající na, opakem by bylo končící na "ends_with()"
d_vyber3 <- d %>%
  select(starts_with("p")) 

colnames(d)

# odstranění proměnných s funkcí select
d <- d %>% 
  select(-c(vzdelani, jmenoprijmeni, jmeno_prijmeni, na_resp))

# d <- d %>%
#   select(-vzdelani, -jmenoprijmeni, -jmeno_prijmeni, -na_resp)

rm(d_vyber, d_vyber2, d_vyber3, zeny, ne_muzi, vybrane_id, vybrana_id, vybrana_id2)


# ÚKOLY 
# 1. vyber z datasetu proměnné začínající na písmeno o
# 2. vyfiltruj respondenta s id 12 
# 3 vyber z datasetu jen proměnné které jsou číselné (is.numeric)



# souhrnné a číselné statistiky -------------------------------------------

summary(d$obvykla_delka_spanku)

min(d$obvykla_delka_spanku, na.rm = TRUE)
max(d$obvykla_delka_spanku, na.rm = TRUE)
mean(d$obvykla_delka_spanku, na.rm = TRUE)
median(d$obvykla_delka_spanku, na.rm = TRUE)
sum(d$obvykla_delka_spanku, na.rm = TRUE)


spanek <- d %>%
  group_by(vzdelani_3kat) %>%
  summarize(mean_spanek = round(mean(obvykla_delka_spanku, na.rm = TRUE), digits = 2))

# ÚKOLY 
# 1. jaký je nejstarší a nejmladší účastník? Kolik je jim let? 
# 2. jaký je medián hodin spánku pro ženy? 



# seřazení datasetu podle hodnot proměnné ------------------------------------------------------

# seřazení 
d <- d %>% 
  arrange(desc(vek))

# arrange mělo problém s českou diakritikou ale funkce sort() funguje 
d$prijmeni <- sort(d$prijmeni, decreasing = FALSE)


# ÚKOLY 
# 1. Seřaď respondenty podle hodin spánku od nejméně po nejvíce
# 2. Seřaď datset abecedně podle ORP od konce abecedy


# změna pořadí proměnných v dataframu-------------------------------------------------

d <- d %>%
  relocate(vek, .after = datum_narozeni) %>%
  relocate(orp, .before = last_col())


# ÚKOLY 
# 1. smaž dataset spánek z global environment
# 2. Umísti proměnnou vzdělání vš za proměnnou vzdelani_3kat 
# 3. Seřaď responendty podle dne narozeni vzestupně 



# ukládání dat ------------------------------------------------------------

saveRDS(d, "data/processed/dataset_clean.rds")

write.xlsx(d, "data/processed/dataset_clean.xlsx")




