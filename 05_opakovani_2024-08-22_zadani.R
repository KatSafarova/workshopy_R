library(czso) # balíček pro práci s daty ČSÚ
library(ggplot2) # grafy
library(tidyr)
library(dplyr)
library(forcats) # práce s faktory
library(stringr) # práce s textovými proměnnými
library(RColorBrewer) # barevná paleta


# nastavení ---------------------------------------------------------------

# Kam se ukládají stažené soubory, pokud bychom je stahovali 
options(czso.dest_dir = "data-input/czso")


# načtení dat -------------------------------------------------------------

# najdi si v katalogu ČSÚ "Uchazeči o zaměstnání dosažitelní a podíl nezaměstnaných osob podle obcí"
# načti si data za rok 2023 a 2022 (zkus to nejdříve přes balíček CŠÚ případně máš už data ve složce)
# zkontroluj zda mají oba datasety za rok 2022 a 2023 stejné proměnné 



# propojení datasetů a připojení dalších dat o území-------------------------------

# propoj oba datasety přes bind_rows()

# použij 
czso_get_table("struktura_uzemi_cr") 

# a vyber z toho pouze proměnné 
# obec_kod, obec_typ, orp_text, kraj_text

# připoj left_joinem ke spojenému datasetu informace o typu obce, orp,  a kraji k datasetu, uved do by = přes kterou proměnou to napojuješ






# co chceme prozkoumat -------------------------------------------------------------------

# řekněme si, že nás zajímají podíly nezaměstnaných,
# jaká ORP měla v roce 2023 nejnižší a nejvyšší nezaměstnanost
# V jakých ORP došlo k největšímu poklesu a nárůstu nezaměstnanosti mezi roky 2022 a 2023?
# jak se nezaměstnanost liší podle obec_typ (velikost obce),
# jak se celková nezaměstnanost a nezaměstnanost žen vyvíjela v jednotlivých měsících mezi roky 2022 a 2023, 

# TODO nezapomeň ve všech úkolech používat group_by na úrovni na které potřebuješ (obec_typ / orp / mesic atd...)


# Prozkoumej jaké kategorie má vuk_text 
# vyfitruj pouze podíl nezaměstnaných osob žen a celkem 
# uprav si názvy proměnných jak ti vyhovuje


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



# 2) V jakých ORP došlo k největšímu poklesu a nárůstu nezaměstnanosti mezi roky 2022 a 2023?


# Nápověda ----------------------------------------------------------------
# bude se ti hodit převést data do širokého formátu
d_wide <- d_orp_zmena %>%
  pivot_wider(names_from = rok, values_from = prumer, names_prefix = "rok_")


# konec nápovědy ----------------------------------------------------------



# 3) jak se lišila celková nezaměstnanost podle proměnné obec_typ? v letech 2022 a 2023
# z datasetu odfiltrujme pryč u obec_typ  "Vojenský újezd"
# znázorni grafem, např.pomocí  geom_col



# 4) jak se vyvíjela v jednotlivých měsících v toku 2022 a 2023 celková nezaměstnanost  a nezaměstnanost žen, 
# znázorní grafem, možné mít dva grafy vedle sebe (za rok 2022 a 2023 nebo podle toho zda je celková nebo žen, nebo vše v 1 grafu)


# volitelný úkol- znázorni na mapě po ORP celkovou nezaměstnanost 