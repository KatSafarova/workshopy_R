library(tidyverse)
# ggplot2: pro vizualizaci dat
# dplyr: pro manipulaci s daty
# tidyr: pro úpravu dat (např. reshaping)
# readr: pro čtení dat (např. CSV)
# purrr: pro funkcionální programování
# tibble: pro moderní data.frame
# stringr: pro práci s textovými řetězci #https://www.youtube.com/watch?v=Rj5YFbXGENY 
# forcats: pro práci s faktory
# lubridate: pro manipulaci s daty a časy

# Načtení dalších potřebných balíčků
library(janitor) # video ukazující funkce k tabulkám z janitoru, funkce tabyl() # https://www.youtube.com/watch?v=3D_BSQlBIr0 janitor
library(openxlsx) # import a export xlsx souborů 
library(readxl)
library(stringi)

# Nastavení lokalizace pro český jazyk (Windows)
Sys.setlocale("LC_ALL", "Czech")

# Pro Linux nebo Mac může být nastavení trochu jiné
# Sys.setlocale("LC_TIME", "cs_CZ.UTF-8")


# Nepoužívat exponenciální zobrazení čísel 
options(scipen = 99)



# načtení dat - cvičné datasety  -------------------------------------------------------------

# cíl provést v R stejné operace jako v excelovém souboru cviceni_excel_ws_rijen_2024
orp <- read_excel("data/input/cviceni_excel_ws_rijen_2024.xlsx", sheet = "orp_zdroj") # readr
telefony <- read.xlsx("data/input/cviceni_excel_ws_rijen_2024.xlsx", sheet = "telefony_zdroj") # openxlsx
studenti <- read.xlsx("data/input/cviceni_excel_ws_rijen_2024.xlsx", sheet = "studenti_zdroj", detectDates = TRUE) # openxlsx


# čištění názvů proměnných  -----------------------------------------------
orp <- orp %>%
  clean_names()

telefony <- telefony %>%
  clean_names()

studenti <- studenti %>%
  clean_names()

# cleaned <- map(list(orp, telefony, studenti), clean_names)
# orp <- cleaned[[1]]
# telefony <- cleaned[[2]]
# studenti <- cleaned[[3]]

# rm(cleaned)


# ORP - 1. dataset  ---------------------------------------------------------------------

# extrakce hodnot/slov nebo podle podmínek
# napojení NUTS 2 regionů podle tabulky krajů a NUTS2
# hledání hodnot podle pozice v tabulce

# stringr::word()

orp <- orp %>% 
  as_tibble() %>% 
  mutate(across(where(is.character), trimws), 
         kraj_zkracene_1_slovo = word(kraj, 1),
         kraj_spravne_1_slovo = if_else(str_starts(kraj, "Kraj"), word(kraj, 2), word(kraj, 1)),
         nuts2 = case_when(
           kraj == "Hlavní město Praha" ~ "Praha",
           kraj == "Středočeský kraj" ~ "Střední Čechy",
           kraj == "Jihočeský kraj" ~ "Jihozápad",
           kraj == "Plzeňský kraj" ~ "Jihozápad",
           kraj == "Karlovarský kraj" ~ "Severozápad",
           kraj == "Ústecký kraj" ~ "Severozápad",
           kraj == "Liberecký kraj" ~ "Severovýchod",
           kraj == "Královéhradecký kraj" ~ "Severovýchod",
           kraj == "Pardubický kraj" ~ "Severovýchod",
           kraj == "Kraj Vysočina" ~ "Jihovýchod",
           kraj == "Jihomoravský kraj" ~ "Jihovýchod",
           kraj == "Olomoucký kraj" ~ "Střední Morava",
           kraj == "Zlínský kraj" ~ "Střední Morava",
           kraj == "Moravskoslezský kraj" ~ "Moravskoslezsko",
           TRUE ~ NA_character_  # pro ostatní případy
         )
  )

orp %>% 
  filter(kraj == "Kraj Vysočina") %>% 
  select(kraj, kraj_zkracene_1_slovo, kraj_spravne_1_slovo, nuts2) %>% 
  distinct(kraj, .keep_all = TRUE) %>% 
  print()

# tabyl z balíčku janitor 
tabyl(orp$kraj_spravne_1_slovo)

# sloupce pro spojení NUTS2 a krajů je lepší uložit zvlášť a odmazat z původní tabulky 
klic_orp_nuts_zvlast <- orp %>% 
  select(nuts_2_klic, kraj_klic) %>% 
  remove_empty("rows")

orp <- orp %>% 
  select(-nuts_2_klic, -kraj_klic) 


# počty všech obcí s "nad" oddělenými mezerami
orp %>%
  filter(str_detect(obec, " nad ")) %>%
  select(obec) %>%
  pull()

# počet jedinečných ORP začínajících na "Kr"
orp %>%
  filter(str_starts(orp, "Kr")) %>%
  distinct(orp) %>%
  count()

# počty všech unikátních obcí končících na á 
orp %>%
  filter(grepl("á$", obec)) %>% #regular expressions (regex)
  distinct(obec) %>%
  count()

# TODO upravit pro českou diakritiku
orp %>%
  filter(str_ends("á", obec)) %>%
  distinct(obec) %>%
  count()

orp %>%
  filter(str_ends(stri_trans_general(obec, "Latin-ASCII"), "a")) %>% # Tato úprava by měla zajistit, že porovnání bude fungovat správně bez ohledu na diakritiku.
  distinct(obec) %>%
  count()


# ÚKOL 1 --------------------------------------------------------------------
# TODO spočítej, kolik jedinečných orp končí na na "ov"


# pokračování cvičení po ÚKOLU 1 ------------------------------------------

# poslední záznam u obce
orp %>%
  select(obec) %>% 
  slice_tail(n = 1)

# hodnota ve sloupci 4, na 8. řádku 
orp[8, 4]

# nebo 
orp %>%
  slice(8) %>%
  pull(4)
 
# slice_head(n = x): Vybere prvních x řádků.
# slice_tail(n = x): Vybere posledních x řádků.
# slice_sample(n = x): Náhodně vybere x řádků.
# slice_min(order_by = col, n = x): Vybere řádky s minimálními hodnotami ve sloupci col.
# slice_max(order_by = col, n = x): Vybere řádky s maximálními hodnotami ve sloupci col.

# název 6. sloupce
orp %>%
  select(6) %>%
  colnames()

# můžeme vybrat více sloupců podle pozice, ale přesnější je vybírat podle názvů 
orp %>%
  select(6:8) %>%
  colnames()

# Doplnění názvu číslo orp a název podle hodnoty, příklad "obec == Kozlov"
orp %>%
  filter(obec == "Kozlov") %>%
  select(cisorp, orp) %>% 
  # slice(1) %>% #se slice(1) první shoda	Kozlov, jinak všechny
  print()


# ÚKOL 2 --------------------------------------------------------------------
# TODO Zjisti, z jakého okresu nebo okresů a kraje/ů je obec Píš


# pokračování cvičení po ÚKOLU 2 ------------------------------------------


# TELEFONNÍ ČÍSLA - 2. dataset ---------------------------------------------------------
# odstranění mezer uprostřed hodnot
# odstranění všech vybraných hodnot
# zjištění počtu znaků 
# doplnění konkrétní hodnoty do dosažení zadaného počtu znaků 
# doplnění hodnot na základě jiné hodnoty

t <- telefony %>% 
  as_tibble() 

rm(telefony)

t <- t %>% 
  mutate(ico_bez_mezer = str_replace_all(ico_s_mezerami, " ", ""), # podobně funguje funkce gsub()
         ico_bez_nul = str_remove(ico_bez_mezer, "^0+"), #regular expressions (regex)
         pocet_znaku_v_ico = nchar(ico_bez_mezer), 
         ico_s_doplnenymi_nulami = str_pad(ico_bez_nul, width = 8, side = "left", pad = "0")  # Doplnění nul
)

# width = 8: Cílová délka výsledného řetězce.
# side = "left": Určuje, že chceme přidat znaky na začátek (doleva).
# pad = "0": Znak, který chceme použít pro doplnění (v tomto případě nula).

00420 765 876 543

# sjendocení telefonních čísel 
t <- t %>% 
  mutate(
    telefon_upraveny = str_replace_all(telefon, " ", ""),  # Odstranění všech mezer
    telefon_upraveny = str_replace(telefon_upraveny, "^\\+420", "00420"),  # Standardizace +420 na 00420
    telefon_upraveny = str_replace(telefon_upraveny, "^\\+421", "00421"),  # Standardizace +421 na 00421
    telefon_upraveny = str_replace(telefon_upraveny, "^\\+48", "0048"),    # Standardizace +48 na 0048
    telefon_upraveny = str_replace(telefon_upraveny, "^0048", "0048 "),     # Přidání mezery po 0048
    telefon_upraveny = str_replace(telefon_upraveny, "^00420", "00420 "),   # Přidání mezery po 00420
    telefon_upraveny = str_replace(telefon_upraveny, "^00421", "00421 "),   # Přidání mezery po 00421
    telefon_upraveny = str_replace(telefon_upraveny, "(004\\d{1,2}\\s\\d{3})(\\d{3})(\\d{3})$", "\\1 \\2 \\3")  # Formátování na předvolbu s následnými trojicemi čísel
  )

t$telefon_upraveny

#Rozbor regulárního výrazu dle chatgpt (004\\d{1,2}\\s\\d{3})(\\d{3})(\\d{3})$
# Tento regulární výraz hledá telefonní číslo ve specifickém formátu a rozděluje ho na tři skupiny:
#   
# Část (004\\d{1,2}\\s\\d{3})
# 004: Hledá telefonní číslo, které začíná na "004".
# \\d{1,2}: Hledá 1 nebo 2 číslice (to pokrývá různé délky předvolby, například 00420 nebo 0048).
# \\s: Hledá mezeru za předvolbou (to je ta mezera, kterou jsme již přidali dříve).
# \\d{3}: Hledá tři číslice (to je první trojice číslic za předvolbou).
# Celá tato část je uzavřená v závorkách (), takže se jedná o první zachycenou skupinu.
# 
# Část (\\d{3})
# \\d{3}: Hledá další tři číslice (druhá trojice číslic za první trojicí).
# Tato část je také uzavřena v závorkách, takže je to druhá zachycená skupina.
# Část (\\d{3})$
#   \\d{3}: Hledá poslední tři číslice (třetí trojice číslic).
# $: Znak $ značí konec řetězce, takže tento vzor musí být na konci telefonního čísla.
# Tato část je také v závorkách, takže je to třetí zachycená skupina.
# 3. Nahrazovací řetězec \\1 \\2 \\3
# Tento nahrazovací řetězec říká, jak chceme výsledné telefonní číslo upravit:
#   
#   \\1: Odkazuje na první zachycenou skupinu (předvolba a první trojice čísel).
# \\2: Odkazuje na druhou zachycenou skupinu (druhá trojice čísel).
# \\3: Odkazuje na třetí zachycenou skupinu (třetí trojice čísel).
# Vkládáme mezi ně mezery ( ), aby telefonní číslo vypadalo takto: předvolba 3 3 3.
# 
# Výsledkem tedy je:
#   Tento regulární výraz vezme telefonní číslo, které má předvolbu a za ní devět číslic (rozdělených do tří trojic), a zajistí, že mezi každou trojicí bude mezera, přičemž formát zůstane ve tvaru:
#   
#   "00420 123 456 789"
# "0048 123 456 789"
# Shrnuto: Tento řádek hledá vzor začínající předvolbou a rozdělí zbytek telefonního čísla do trojic, přičemž mezi trojicemi vloží mezery.


# ÚKOL 3 ------------------------------------------------------------------
# TODO Vytvoř v datasetu nový sloupec pocet_znaku_telefon, který spočítá všechny znaky v proměnné telefon_upraveny


# pokračování cvičení po ÚKOLU 3 ------------------------------------------

t %>% 
  select(telefon_upraveny, n_znaku_tel, n_znaku_tel_bm1, n_znaku_tel_bm2)


# Přidání předčíslí 000 před upravené číslo
t <- t %>% 
  mutate(telefon_upraveny_s_novym_predcislim = paste("000", telefon_upraveny))  

# přidání proměnné stát podle předvolby
t <- t %>% 
  mutate(stat = case_when(str_starts(telefon_upraveny, "00421") ~ "Slovensko",
                          str_starts(telefon_upraveny, "00420") ~ "ČR",
                          str_starts(telefon_upraveny, "0048") ~ "Polsko"))

# alternativne dát do tabulky zvlášť klíč pro propojení a připojit přes left_join 
klic_tel_predvolby <- t %>% 
  select(stat_2, predvolba_2) %>% 
  drop_na() %>% 
  print()

t <- t %>%
  select(- c(stat_2, predvolba_2, predvolba_1)) %>% 
  mutate(predvolba = case_when(
    str_detect(telefon_upraveny, "^00420") ~ "00420",
    str_detect(telefon_upraveny, "^00421") ~ "00421",
    str_detect(telefon_upraveny, "^0048") ~ "0048",
    TRUE ~ NA_character_
  )) %>%
  left_join(klic_tel_predvolby, by = c("predvolba" = "predvolba_2")) %>% 
  select(-predvolba)

# souhrnne statistiky k telefonátům za země 
souhrnne_telefonaty <- t %>% 
  drop_na(stat) %>% 
  group_by(stat) %>% 
  summarize(
    prumer = round(mean(pocet_telefonatu), digits = 1),  # Zaokrouhlení po výpočtu průměru
    celkem = sum(pocet_telefonatu),
    median = median(pocet_telefonatu),
    max = max(pocet_telefonatu),
    min = min(pocet_telefonatu),
    sd = round(sd(pocet_telefonatu), digits = 1)  # Zaokrouhlení směrodatné odchylky na 1 desetinné místo
  ) 

# Vypočítáme souhrn pro celý dataset a přidáme jej jako další řádek
celkovy_souhrn <- t %>%
  summarize(
    stat = "Celkem",
    prumer = round(mean(pocet_telefonatu), digits = 1),
    celkem = sum(pocet_telefonatu),
    median = median(pocet_telefonatu),
    max = max(pocet_telefonatu),
    min = min(pocet_telefonatu),
    sd = round(sd(pocet_telefonatu), digits = 1)
  )

# Přidání řádku s celkovými hodnotami do přehledu
souhrnne_telefonaty <- bind_rows(souhrnne_telefonaty, celkovy_souhrn)

rm(celkovy_souhrn)



# další ----------------------------------------------------
# počet řádků a sloupců v tabulce
nrow(t)		
ncol(t)		

# počet platných hodnot u proměnné stát (bez NA)
print(sum(!is.na(t$stat)))

# alternativne
t %>%
  summarise(pocet_hodnot_stat = sum(!is.na(stat))) %>% 
  print()

t %>%
  filter(!is.na(stat)) %>%
  summarise(pocet_hodnot_stat = n()) %>%
  print()


pocet_numeric_sloupcu <- print(sum(sapply(t, is.numeric)))

# Získání názvů numerických sloupců 
nazvy_numeric_sloupcu <- t %>%
  select_if(is.numeric) %>%
  names() %>% 
  print()



# STUDENTI 3. dataset ----------------------------------------------------------------
# rozdělování jména příjmení
# změna velikosti písmen (lower x upper)
# práce s daty

s <- studenti %>% 
  as_tibble()

rm(studenti)

colnames(s)

# spojování 
s <- s %>%
  unite(jmeno_prijmeni, jmeno, prijmeni, sep = " ", remove = FALSE)

class(s$datum_narozeni)

s <- s %>%
  mutate(
    jen_jmeno = word(jmeno_prijmeni, 1),  # První slovo (jméno)
    prostredni_jmeno = ifelse(str_count(jmeno_prijmeni, "\\s+") > 1, word(jmeno_prijmeni, 2, -2), NA),  # Prostřední slovo/slova, pokud jsou
    jen_prijmeni = word(jmeno_prijmeni, -1),  # Poslední slovo (příjmení)
    jmeno_malym = tolower(jmeno),
    jmeno_velkym = toupper(jmeno),
    jmeno_1_pismeno_velkym = str_to_title(jmeno_malym),
    den_narozeni_cislo = day(datum_narozeni),
    mesic_narozeni_cislo = month(datum_narozeni),
    kalendarni_tyden_narozeni_cislo = week(datum_narozeni),
    den_narozeni_den_v_tydnu = weekdays(datum_narozeni),
    den_narozeni_den_v_tydnu_zkratka = substr(den_narozeni_den_v_tydnu, 1, 2) # nebo str_sub
  ) %>% 
  relocate(prostredni_jmeno, .after = jen_prijmeni) %>% 
  relocate(den_narozeni_den_v_tydnu_zkratka, .after = den_narozeni_den_v_tydnu) 
  

# Zobrazit více řádků a sloupců pomocí print()
print(s, n = Inf, width = Inf)
glimpse(s)


# přiřazování znamení zvěrokruhu  ------------------------------------------

# opět je vhodné klíč uložit zvlášť
znameni_propojeni <- s %>% 
  select(znameni_klic, datum_znameni) %>% 
  remove_empty("rows")

s <- s %>% 
  select(-znameni_klic, -datum_znameni) 


znameni_propojeni <- znameni_propojeni %>% 
  separate(datum_znameni, into = c("start", "end"), sep = " – ", remove = FALSE) %>%
  mutate(
    # Nahrazení teček a oddělení dnů a měsíců
    start = str_replace_all(start, "\\. ", "-"),
    end = str_replace_all(end, "\\. ", "-")
  ) %>%
  separate(start, into = c("start_den", "start_mesic"), sep = "-") %>%
  separate(end, into = c("end_den", "end_mesic"), sep = "-") %>%
  mutate(
    # Převod na numerické hodnoty
    start_den = as.integer(start_den),
    start_mesic = as.integer(start_mesic),
    end_den = as.integer(end_den),
    end_mesic = as.integer(end_mesic)
  )

znameni_propojeni

colnames(s)


# Funkce pro přiřazení znamení na základě klíče
prirad_znameni <- function(den, mesic) {
  znamen <- znameni_propojeni %>%
    filter(
      # Podmínka pro běžný případ: datum narození je mezi startem a koncem znamení
      (mesic == start_mesic & den >= start_den) |
        (mesic == end_mesic & den <= end_den) |
        # Přidat podmínku pro znamení, která přecházejí do nového roku (Kozoroh)
        (start_mesic > end_mesic & (mesic == start_mesic | mesic == end_mesic))
    )
  
  # Kontrola, zda bylo nalezeno nějaké znamení
  if (nrow(znamen) == 0) {
    return(NA)  # Vrátí NA, pokud nebylo nalezeno žádné znamení
  } else if (nrow(znamen) > 1) {
    return(paste(znamen$znameni_klic, collapse = ", "))  # Vrátí seznam znamení
  }
  
  # Jinak vrátí první nalezené znamení
  return(pull(znamen, znameni_klic))  
}


# Naplníme existující sloupec znameni_r a zkontrolujeme výstupy
s <- s %>%
  rowwise() %>%
  mutate(znameni_r = prirad_znameni(den_narozeni_cislo, mesic_narozeni_cislo)) %>%
  ungroup() 

# porovnání rozdílných znamení vygenerovaných chatgpt a přiřazených v R
s %>% 
  select(datum_narozeni, znameni_chatgpt, znameni_r) %>%
  filter(znameni_chatgpt != znameni_r) %>% 
  print() 
# když se podíváme do klíče, vidíme že náš výpočet je správný 


# výpočet roků z data narození  -------------------------------------------

s <- s %>%
  mutate(
    dnes = Sys.Date(),
    vek_r = year(dnes) - year(datum_narozeni), # celkové roky
    vek_r2 = as.integer(difftime(dnes, datum_narozeni, units = "weeks") / 52.25)     # Věk 2: Vypočítání přesného věku v letech, zaokrouhleno na celé číslo
  ) %>% 
  relocate(vek_r2, .after = vek_r)


# porovnání rozdílných věků
s %>% 
  select(datum_narozeni, dnes, vek_r, vek_r2) %>%
  filter(vek_r != vek_r2) %>% 
  print() 

# porovnání, zda se vypočtené hodnoty shodují
s %>% 
  mutate(porovnani_znameni_chatgpt_r = znameni_chatgpt == znameni_r,
         porovnani_veku_chatgpt_r = vek_chatgpt == vek_r) %>% 
  select(-hendikepy_kategorie, -hendikepy_zvyhodneni) %>% 
  select(porovnani_znameni_chatgpt_r, znameni_chatgpt, znameni_r) %>% 
  print()


colnames(s)

class(s$hendikep_stupen)

# kolik je v datasetu duplikátů
duplikaty <- get_dupes(s, -id)  # Vynechání sloupce id z kontroly

# Vytvoření vektoru s ID duplikátů
id_duplikaty <- unique(duplikaty$id)  # Získání unikátních ID

s <- s %>% 
  mutate(across(where(is.character), trimws),  # Aplikace trimws ořezávající mezery na konci a na začátku na všechny character sloupce
    final_skore_celkem = case_when(
      hendikep_stupen == "ne" ~ skore_celkem, 
      hendikep_stupen == "1" ~ skore_celkem + 0.5, 
      hendikep_stupen == "2" ~ skore_celkem + 1, 
      hendikep_stupen == "3" ~ skore_celkem + 2,
      TRUE ~ skore_celkem
    ),
    skore_kat = cut(
          final_skore_celkem,
          breaks = c(-Inf, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, Inf),  # Definice intervalů
          labels = c("0-5", "5,1-10", "10,1-15", "15,1-20", "20,1-25,1", "25,1 -30", "30,1-35", "35,1-40", "40,1-45", "45,1-50", "Mimo rozsah"),  # Názvy kategorií
          right = TRUE  # Interval je otevřený na pravé straně
        ),
    ma_vyssi_skore_nez_30 = if_else(final_skore_celkem > 30, "Ano", "Ne"),
    percentily = percent_rank(final_skore_celkem)*100,
    poradi = round(rank(final_skore_celkem, ties.method = "min"), digits = 0), # další average
    je_totalni_duplikat = if_else(id %in% id_duplikaty, "Ano", "Ne")
  ) %>%
  group_by(id) %>% 
  mutate(kolik_ma_respondent_na = sum(is.na(final_skore_celkem))) %>% 
  ungroup()

# Druhý nejvyšší počet bodů
s %>%
  arrange(desc(final_skore_celkem)) %>%  # Seřazení od nejvyššího k nejnižšímu
  slice(2) %>%   # Vyber druhý řádek (druhý nejvyšší)
  select(id, jmeno, prijmeni, final_skore_celkem) %>% 
  print()

# Druhý nejnižší počet bodů
s %>%
  arrange(final_skore_celkem) %>%  # Seřazení od nejnižšího k nejvyššímu
  slice(2) %>%   # Vyber druhý řádek (druhý nejnižší)
  select(id, jmeno, prijmeni, final_skore_celkem) %>% 
  print()

# ÚKOL 4 --------------------------------------------------------------------
# který student má u skóre třetí nejvyšší percentil


# ukládání dat ------------------------------------------------------------
# rkový formát
saveRDS(s, "data/intermediate/studenti_upraveny_dataset_ws_6.rds")
saveRDS(orp, "data/intermediate/orp_upraveny_dataset_ws_6.rds")
saveRDS(t, "data/intermediate/telefony_upraveny_dataset_ws_6.rds")

# jako xlsx nebo csv
write.xlsx(s, "data/intermediate/studenti_upraveny_dataset_ws_6.xlsx")
write.csv2(s, "data/intermediate/studenti_upraveny_dataset_ws_6.csv")


# EXTRA TÉMA-------------------------------------------------------------------
# zaokrouhlování ----------------------------------------------------------

# Funkce round() v R zaokrouhluje hodnoty podle pravidla zaokrouhlování k nejbližšímu sudému číslu (tzv. "round half to even"). Tento způsob zaokrouhlování se někdy nazývá bankéřské zaokrouhlování. 

# pokud chci zaokrouhlovat končící na 0.5 nahoru, mohu požít následující funkci

round_half_up <- function(x, digits = 0) {
  multiplier <- 10^digits
  adjusted_x <- x * multiplier
  
  # Kontrola, zda je desetinná část přesně 0.5
  if (adjusted_x %% 1 == 0.5) {
    return((floor(adjusted_x) + 1) / multiplier)
  } else {
    return(round(x, digits = digits))
  }
}

# Příklady použití
round(0.5) # výchozí zaokrouhlování, výsledek 0
round_half_up(0.5)     # funkce, Výsledek: 1
round_half_up(1.5)     # Výsledek: 2
round(1.5)
round_half_up(2.5)     # Výsledek: 3
round(2.5)
round_half_up(2.25, 1) # Výsledek: 2.3
round(2.25, digits = 1)
round_half_up(2.75, 1) # Výsledek: 2.8
round(2.75, digits = 1)
round_half_up(2.755, 2) # Výsledek: 2.76


# Vysvětlení funkce krok po kroku:
#   Definice parametru digits:
#   
#   Funkce bere vstupní hodnotu x a volitelný argument digits (kolik desetinných míst chceme zachovat). Výchozí hodnota je 0, což znamená, že pokud neuvedeš počet desetinných míst, funkce bude zaokrouhlovat na celé číslo.
# Násobení pomocí multiplier:
#   
#   multiplier <- 10^digits: Pokud chceš zaokrouhlit například na 1 desetinné místo, násobíš číslo 10. Pro 2 desetinná místa násobíš číslo 100, a tak dále.
# Příklad: Pro x = 2.755 a digits = 1 se multiplier stane 10 a adjusted_x se spočítá jako 2.755 * 10 = 27.55.
# Kontrola, zda je desetinná část přesně .5:
#   
#   adjusted_x %% 1 == 0.5: Tento krok zjistí, zda desetinná část výsledku po násobení končí na .5. To se provádí pomocí operátoru modulo (%%), který zjistí zbytek po dělení 1.
# Příklad: Pokud máme 27.55 %% 1, výsledek je 0.55, což není .5, takže tato podmínka není splněna a jdeme do větve else.
# Zaokrouhlení nahoru:
#   
#   Pokud je desetinná část přesně .5, použije se následující pravidlo:
#   floor(adjusted_x) + 1: floor() zaokrouhlí hodnotu směrem dolů na nejbližší celé číslo a pak se přičte 1, což zajistí zaokrouhlení nahoru.
# Příklad: Pokud by adjusted_x bylo například 27.5, floor(27.5) dá 27, a přidáním 1 dostaneme 28.
# Poté dělíme výsledné číslo zpět multiplierem, aby se desetinná čárka vrátila na správné místo.
# Příklad: 28 / 10 = 2.8.
# Standardní zaokrouhlování:
#   
#   Pokud není desetinná část .5, použije funkce standardní zaokrouhlení pomocí round(), což je defaultní zaokrouhlovací funkce R.
# Příklad: round(2.755, 1) by v tomto případě zaokrouhlilo na 2.8, protože .55 je blíže k .6.

