library(czso) # balíček pro práci s daty ČSÚ
library(ggplot2) # grafy
library(tidyr)
library(dplyr)
library(forcats) # práce s faktory
library(stringr) # práce s textovými proměnnými
library(RColorBrewer) # barevná paleta

# library(ggiraph)
# library(nanoparquet)
# library(statnipokladna)
# library(tmap) # mapy


# nastavení ---------------------------------------------------------------

# Kam se ukládají stažené soubory, pokud bychom je stahovali 
options(czso.dest_dir = "data-input/czso")


# načtení dat -------------------------------------------------------------

# zobrazení katalogu ČSÚ
catalogue <- czso_get_catalogue()

# hledám podle klíčového slova
vysledky <- catalogue %>%
  filter(str_detect(title, "Uchazeči")) %>%
  select(dataset_id, start, end, title, description)

czso_get_table("250169r22") # stahuje mi zazipované, odzipovala jsem 

# načítám si dataset
d23 <- read.csv("data/input/czso/250169r23/OD_NEZ01_2024011010464438.CSV")
d22 <- read.csv("data/input/czso/250169r22/OD_NEZ01_2023011008094073.CSV")


# schéma proměnných
czso_get_table_schema("250169r23")

# dívám se na názvy proměnných, vypadají shodně, pro ověření úplné shody můžeme použít funkci identical
vars23 <- colnames(d23)
vars22 <- colnames(d22)

identical(vars23, vars22)



# napojení dat o území a propojení datasetů -------------------------------

# struktura území - jen vybrané
struktura_uzemi <- czso_get_table("struktura_uzemi_cr") %>%
  select(obec_kod, obec_typ, orp_text, okres_text, kraj_text)

# spojuju datasety
d <- bind_rows(d22, d23)

# sjednocuju class pro spojení
d <- d %>%
  mutate(uzemi_kod = as.character(uzemi_kod))

# připoju info o typu obce, orp, okresu a kraji k datasetu
d <- d %>%
  left_join(struktura_uzemi, by = c("uzemi_kod" = "obec_kod"))

# dívám se na unikátní kategorie (a počty)
table(d$vuk_text)
unique_values <- unique(d$vuk_text) %>% print()


# otázky -------------------------------------------------------------------

# řekněme si, že nás zajímají podíly nezaměstnaných,
# jaká ORP měla v roce 2013 nejnižší a nejvyšší nezaměstnanost
# V jakých ORP došlo k největšímu poklesu a nárůstu nezaměstnanosti mezi roky 2022 a 2023?
# jak se nezaměstnanost liší podle obec_typ (velikost obce),
# jak se celková nezaměstnanst a nezaměstnanost žen vyvíjela v jednotlivých měsících mezi roky 2022 a 2023, 



# pro přehlednost výběr jen vybraných proměnných, přejmenování hodnot i vybraných proměnných
d <- d %>%
  filter(vuk_text == "Podíl nezaměstnaných osob  - ženy (%)" | vuk_text == "Podíl nezaměstnaných osob (%)") %>%
  mutate(vuk_text = recode(vuk_text,
                           "Podíl nezaměstnaných osob  - ženy (%)" = "Nezaměstnanost žen",
                           "Podíl nezaměstnaných osob (%)" = "Nezaměstnanost celkem")) %>%
  select(obec_txt = uzemi_txt, kod_obce = uzemi_kod, vuk_text, obec_typ, orp_text, kraj_text, mesic, rok, podil = hodnota)

# nejnižší jednotka obec_typ
d_souhrn <- d %>%
  group_by(vuk_text, obec_typ, orp_text, kraj_text, mesic, rok) %>%
  summarize(prumer = round(mean(podil, na.rm = TRUE), digits = 1), .groups = 'drop')

d_souhrn_celkova <- d_souhrn %>%
 filter(vuk_text == "Nezaměstnanost celkem")

# 1) ve kterých 5 ORP byla v roce 2023 nejnižší a nejvyšší celková nezaměstnanost?
# nejnižší 
d_min <- d_souhrn_celkova %>%
  group_by(orp_text, kraj_text, rok) %>%
  summarize(prumer = round(mean(prumer, na.rm = TRUE), digits = 1), .groups = 'drop') %>%
  filter(rok == 2023) %>%
  select(orp_text, kraj_text, prumer) %>%
  arrange(prumer) %>%
  slice_head(n=5) %>%
  print()

# nejvyšší
d_max <- d_souhrn_celkova %>%
  group_by(orp_text, kraj_text, rok) %>%
  summarize(prumer = round(mean(prumer, na.rm = TRUE), digits = 1), .groups = 'drop') %>%
  filter(rok == 2023) %>%
  select(orp_text, kraj_text, prumer) %>%
  arrange(desc(prumer)) %>%
  slice_head(n=5) %>%
  print()


# 2) V jakých ORP došlo k největšímu poklesu a nárůstu nezaměstnanosti mezi roky 2022 a 2023?

# Agregace na úrovni orp_text a rok
d_orp_zmena <- d_souhrn_celkova %>%
  group_by(orp_text, kraj_text, rok) %>%
  summarize(prumer = mean(prumer, na.rm = TRUE), .groups = 'drop')

# Pivot data do širokého formátu: jeden sloupec pro každý rok
d_wide <- d_orp_zmena %>%
  pivot_wider(names_from = rok, values_from = prumer, names_prefix = "rok_")

# Spočítej změnu mezi roky 2022 a 2023
d_zmena <- d_wide %>%
  mutate(zmena = rok_2023 - rok_2022) %>%
  select(orp_text, kraj_text, zmena, rok_2023, rok_2022)

# Najdi ORP s největším nárůstem a poklesem
nejvetsi_narust <- d_zmena %>%
  arrange(desc(zmena)) %>%
  slice_head(n=5) %>%
  print()

nejvetsi_pokles <- d_change %>%
  arrange(zmena) %>%
  slice_head(n=5) %>%
  print()


# 3) porovnání obcí, měst, hlavního města a statutátrních měst - celková nezaměstnanost v letech 2022 a 2023, vyřaďme Vojenský újezd


palette <- brewer.pal(5, "Set2")

# Agregace na úrovni obec_typ a rok
d_obec_typ <- d_souhrn_celkova %>%
  filter(obec_typ != "Vojenský újezd") %>%
  group_by(obec_typ, rok) %>%
  summarize(prumer = round(mean(prumer, na.rm = TRUE), digits =1), .groups = 'drop')

# faktor z proměnné v obec_typ řazený podle hodnoty prumer, pro hezke zobrazeni v grafu
d_obec_typ <- d_obec_typ %>%
  mutate(obec_typ = fct_reorder(obec_typ, prumer))

levels(d_obec_typ$obec_typ)


# Vytvoření grafu
ggplot(d_obec_typ, aes(x = factor(rok), y = prumer, fill = obec_typ)) +
  geom_col(position = "dodge") +
  labs(
    title = "Průměrná nezaměstnanost podle typu obce",
    x = "Rok",
    y = "Podíl nezaměstnaných (%)",
    fill = "Kategorie obce"
  ) +
  scale_fill_manual(values = palette) +
  theme_minimal()



# 4) jak se vyvíjela celková nezaměstnanosti v čase po měsících Nezaměstnanost celkem a nezaměstnanost žen
d_souhrn_mesic <- d_souhrn %>%
  group_by(mesic, vuk_text, rok) %>%
  summarize(prumer = round(mean(prumer, na.rm = TRUE), digits =1), .groups = 'drop')

# Vytvoření 2 grafů vedle sebe
ggplot(d_souhrn_mesic, aes(x = factor(mesic), y = prumer, fill = vuk_text)) +
  geom_col(position = "dodge") +
  labs(
    title = "Vývoj celkové nezaměstnanosti a nezaměstnanosti žen v letech 2022 a 2023",
    x = "Měsíc",
    y = "Podíl nezaměstnaných (%)",
    fill = "Legenda"
  ) +
  scale_fill_manual(values = c("Nezaměstnanost celkem" = "blue", "Nezaměstnanost žen" = "#FFA7B6")) +
  theme_minimal() +
  facet_wrap(~ rok) # Pokud chceš rozdělit graf podle roku


# vše v jednom grafu
# propojení nezaměstnanosti a žen s mezerou a seřazení do pořadí v jakém to chci zobarzovat v grafu
d_souhrn_mesic <- d_souhrn_mesic %>%
  mutate(vuk_rok = factor(interaction(vuk_text, rok, sep = " "),
                          levels = c("Nezaměstnanost celkem 2022",
                                     "Nezaměstnanost celkem 2023",
                                     "Nezaměstnanost žen 2022",
                                     "Nezaměstnanost žen 2023")))

ggplot(d_souhrn_mesic, aes(x = factor(mesic), y = prumer, fill = vuk_rok)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  labs(
    title = "Vývoj celkové nezaměstnanosti a nezaměstnanosti žen v letech 2022 a 2023",
    x = "Měsíc",
    y = "Podíl nezaměstnaných (%)",
    fill = "Legenda"
  ) +
  scale_fill_manual(values = c("Nezaměstnanost celkem 2022" = "lightblue",
                               "Nezaměstnanost celkem 2023" = "blue",
                               "Nezaměstnanost žen 2022" = "#FFCDD2",
                               "Nezaměstnanost žen 2023" = "#FFA7B6")) +
  theme_minimal()

# vysvětlení chatgpt 
# interaction(vuk_text, rok): Kombinace proměnných vuk_text a rok do jedné, aby každá kombinace měla vlastní barvu.

# position_dodge2(preserve = "single"): Tento parametr zajistí, že sloupce budou seskupeny podle hodnoty mesic, ale pro každý rok budou mít svůj vlastní odstín barvy.

# scale_fill_manual(): Definuje barvy pro jednotlivé kombinace vuk_text a rok, takže sloupce za rok 2022 a 2023 budou mít různé odstíny, ale zachovají podobnou barevnost pro ženy a celkovou nezaměstnanost.


# pokud bychom chtěli vytvořit kvartály můžeme použít case_when, a pak používat group_by(kvartal)
d_souhrn_mesic <- d_souhrn_mesic %>%
  mutate(kvartal = case_when(
    mesic >= 1 & mesic <= 3 ~ 1,
    mesic >= 4 & mesic <= 6 ~ 2,
    mesic >= 7 & mesic <= 9 ~ 3,
    mesic >= 10 & mesic <= 12 ~ 4
  ))
