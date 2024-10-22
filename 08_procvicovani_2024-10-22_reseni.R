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
# 1. pročistit 
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
# možné funkce k použití 
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

d <- d_raw %>% 
  clean_names() %>% 
  rename(dop_prost = dopravni_prostredek) %>% 
  distinct()

# duplikaty <- get_dupes(d_raw)

glimpse(d) # pro check celého dataframu, jaké mají proměnné class()



# ošetření textových proměnných -------------------------------------------------------
# ostranění přebytečných mezer na začátku a na konci 
d <- d %>% 
  mutate(across(where(is.character), trimws)) 

# sjednocení velikosti písmen 
d <- d %>%
  mutate(across(c(start, cil, dop_prost), str_to_title)) %>%  # V daných sloupcích 1. písmena velká 
  mutate(across(c(start, cil, dop_prost), str_replace_all, pattern = "\\b(Nad|Pod|U)\\b", replacement = tolower))  # Nahradíme "nad" a "pod" zpět na malá


# dopočtení nových sloupců ------------------------------------------------
# cena za cestu autem dle spotřeby a ceny benzinu 
benzin <- 36.4
spotreba <- 7

d <- d %>% 
  mutate(vzdalenost = as.numeric(vzdalenost), 
         cena = as.numeric(cena), 
         cena = ifelse(dop_prost == "Auto", vzdalenost/100*benzin*spotreba, cena), # u auta cena dle zadaných parametrů, jinak stávající
         cena_1_osoba = cena, 
         cena_3_osoby = ifelse(dop_prost == "Auto", cena/3, cena*3)) # u ata třetinová, jinak trojnásobná


# převod času na minuty ---------------------------------------------------
d$cas
# máme data v tomto zvláštním formátu

# Je to způsobený tím, že Excel ukládá časy jako zlomek dne, což znamená, že 2:35 (2 hodiny a 35 minut) je uložený jako podíl z 24 hodin. V R je načten jako 0.1076388888888889, což je asi 10,76 % z celkového dne.

# zároveň je tam i chyba, čísla nad 1 nejsou podíl z 24 hodin, ale minuty

class(d$cas)
unique(d$cas)

# Chceme převést čas na minuty. V této části je vhodné také do promenne cas_fin připočíst zpoždění pro cestu autem do Jihlavy a Brna a dále pracovat s cas_fin()

d <- d %>% 
  mutate(
    cas = as.numeric(cas)) %>% # z nečíselných hodnot budou NA
  filter(!is.na(cas)) %>% 
  mutate(
    cas = ifelse(cas > 1, cas/(24*60), cas),
    cas_fin = ifelse(dop_prost == "Auto" & cil %in% c("Brno", "Jihlava"), cas + 0.1 * cas, cas), 
    minut_celkem = cas_fin * 24 * 60, # Celkový počet minut jednoduše
    hodiny = floor(cas_fin * 24),  # Celé hodiny zaokrouhlené dolu pro zobrazení ve formátu HH:MM
    minuty = (cas_fin * 24 - hodiny) * 60,  # Přebývající minuty přes celou hodinu
    cas_s_pomlckou = sprintf("%02d:%02d", hodiny, round(minuty)),  # Formátování "HH:MM"
    minut_celkem2 = hodiny * 60 + round(minuty),  # Celkový počet minut z dopočtených hodin a minut
  )

# Pokud d$cas * 24 je 12.75, znamená to 12 hodin a 0.75 hodiny, což odpovídá 45 minutám (0.75 * 60 = 45).

# Formátování času jako "HH": Funkce sprintf() formátuje řetězec, takže hodiny a minuty jsou vyjádřeny jako dvouciferné číslo. Například pokud hodiny jsou 12 a minuty 45, vytvoří řetězec "12:45".
# %02d znamená dvouciferný formát pro hodiny a minuty (doplňuje nuly, pokud je hodnota menší než 10).

# výběr daných proměnných pro check, že vse sedá 
d %>% 
  as_tibble() %>% 
  select(cil, dop_prost, cas, cas_fin, hodiny, minuty, cas_s_pomlckou, minut_celkem, minut_celkem2)


# sjednocení dat (datumů) -------------------------------------------------

unique(d$datum) # jakých unikátních hodnot nabývá datum 

# Nahradíme názvy měsíce číslem pro snadnější úpravy
d$datum <- gsub("října", "10.", d$datum)

# Opravený kód - aplikujeme excel_numeric_to_date jen na číselné hodnoty
d <- d %>%
  mutate(datum_fin = case_when(
    datum == "45580" ~ suppressWarnings(as.Date(excel_numeric_to_date(as.numeric(datum)))),
    # grepl("^\\d+$", datum) ~ suppressWarnings(excel_numeric_to_date(as.numeric(datum))),  # pokud bychom chtěli univerzálněji, pouze pro číselné hodnoty
    TRUE ~ suppressWarnings(dmy(datum))  # Potlačíme varování
  ))

# Výsledek
print(d$datum_fin)

class(d$datum_fin)

# Vysvětlení k Excel číslům:
#   Excel používá číselný formát dat, kde je každý den reprezentován jako pořadové číslo počínaje 1. lednem 1900. Čísla reprezentují počet dní od tohoto data. Například:
# 1. leden 1900 je v Excelu reprezentován jako číslo 1.
# 1. leden 1970 je reprezentován jako číslo 25569, což odpovídá 25 569 dní po 1. lednu 1900.
# 15. říjen 2024 je v Excelu reprezentován jako číslo 45580, což je 45 580 dní po 1. lednu 1900.

# Rozklad regulárního výrazu ^\\d+$: (viz řádek 154)
#   ^: Označuje začátek řetězce. To znamená, že hledání začíná od prvního znaku v řetězci.
# \\d: Označuje libovolnou číslici (0–9). \\d je standardní symbol pro číslice v regulárních výrazech.
# +: Označuje, že předchozí prvek (v tomto případě \\d) se může opakovat jednou nebo vícekrát. To znamená, že hledáme sekvenci jednoho nebo více číslic.
# $: Označuje konec řetězce. To znamená, že po poslední číslici nesmí být žádné jiné znaky.
# Celkový význam:
#   ^\\d+$: Hledá řetězec, který začíná číslicí, obsahuje pouze číslice a končí číslicí. To znamená, že řetězec je tvořen výhradně číslicemi.


# další možnosti k úpravě datumů 
# použít při načtení u read.xlsx detectDates = TRUE, nezobrazí excel čísla, ale i tak je potřeba formáty sjednotit
d1 <- read.xlsx("data/input/cestovani.xlsx", detectDates = TRUE)

# méně univerzální a clever, pokud víme, že je všude jedno datum, můžeme natvrdo přepsat, nebo sjednotit postupně pomocí ifelse či casewhen()
d2 <- d_raw %>% 
  clean_names() %>% 
  mutate(datum = as.Date("2024-10-15"))

class(d2$datum)

# seřazení dle ceny---------------------------------------------------------------
d_cena_1 <- d %>% 
  arrange(cena_1_osoba) 

d_cena_3 <- d %>% 
  arrange(cena_3_osoby)

# final úpravy dataframů pro hezkou formátovanou tabulku ---------

# odstranění přebytečných proměnných, seřazení 
d <- d %>% 
  select(-poznamka, -minut_celkem2, -datum, -cas, -cas_fin, -hodiny, -minuty, -cas_s_pomlckou, -datum_fin, -cena) %>% 
  filter(minut_celkem < 150) %>% 
  mutate(
    pocet_prestupu = ifelse(dop_prost == "Auto", 0, pocet_prestupu),
    minut_celkem = as.integer(minut_celkem), 
    cena_1_osoba = round(cena_1_osoba, digits = 0),  
    cena_3_osoby = round(cena_3_osoby, digits = 0),   
    cena_1_osoba_format = glue("{round(as.numeric(cena_1_osoba))} Kč"),  # Přidání "Kč"
    cena_3_osoby_format = glue("{round(as.numeric(cena_3_osoby))} Kč"),
    vzdalenost_format = glue("{vzdalenost} km")) %>%  
  arrange(minut_celkem)

# gt tabulka s formátováním --------------------------------------------------
# Definujeme barvy pro různé dopravní prostředky
d_color <- d %>%
  mutate(
    barva = case_when(
      dop_prost == "Bus" ~ "lightblue",
      dop_prost == "Vlak" ~ "lightgreen",
      dop_prost == "Auto" ~ "#CBC3E3",
      TRUE ~ "white"  # Defaultní barva
    ),
    cena_1_osoba_color = case_when(
      cena_1_osoba < 150 ~ "darkgreen",
      cena_1_osoba > 500 ~ "red",
      TRUE ~ "black"
    ),
    cena_3_osoby_color = case_when(
      cena_3_osoby < 150 ~ "darkgreen",
      cena_3_osoby > 500 ~ "red",
      TRUE ~ "black"
    )
  )

# Vytvoření gt tabulky s podmíněným formátováním a přejmenováním sloupců
d_table <- d_color %>%
  gt() %>%
  # Skrytí pomocných sloupců (ponechání formátovaných cen pro zobrazení)
  cols_hide(columns = c(barva, cena_1_osoba, cena_3_osoby, cena_1_osoba_color, cena_3_osoby_color, vzdalenost)) %>%
  # Zobrazení formátovaných cen s "Kč"
  cols_label(
    start = "Start",
    cil = "Cíl",
    dop_prost = "Dopravní prostředek",
    vzdalenost_format = "Vzdálenost",
    pocet_prestupu = "Počet přestupů",
    cena_1_osoba_format = "Cena 1 osoba",
    cena_3_osoby_format = "Cena 3 osoby",
    minut_celkem = "Minut celkem"
  ) %>%
  # Podmíněné formátování pro dopravní prostředky
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(
      rows = dop_prost == "Bus"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      rows = dop_prost == "Vlak"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#CBC3E3"),
    locations = cells_body(
      rows = dop_prost == "Auto"
    )
  ) %>%
  # Zvýraznění ceny na základě hodnot
  tab_style(
    style = cell_text(weight = "bold", color = "darkgreen"),
    locations = cells_body(
      columns = cena_1_osoba_format,
      rows = cena_1_osoba < 150
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "red"),
    locations = cells_body(
      columns = cena_1_osoba_format,
      rows = cena_1_osoba > 500
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_body(
      columns = cena_1_osoba_format,
      rows = cena_1_osoba >= 150 & cena_1_osoba <= 500
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "darkgreen"),
    locations = cells_body(
      columns = cena_3_osoby_format,
      rows = cena_3_osoby < 150
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "red"),
    locations = cells_body(
      columns = cena_3_osoby_format,
      rows = cena_3_osoby > 500
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_body(
      columns = cena_3_osoby_format,
      rows = cena_3_osoby >= 150 & cena_3_osoby <= 500
    )
  ) %>%
  # odstranění vybraných čar a další úpravy (viz níže vysvětlení)
  tab_options(
    table_body.hlines.style = "solid",  
    table_body.hlines.width = px(1),    
    table_body.hlines.color = "black",   
    table.border.top.style = "none",    
    table.border.bottom.style = "none", 
    column_labels.border.top.style = "none",    
    column_labels.border.bottom.style = "solid",
    heading.border.bottom.style = "none",
  ) %>%
  # Přidání vnějšího ohraničení
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black", 
      weight = px(2), 
      style = "solid"
    ),
    locations = cells_body()
  ) %>%
  # tab_style(
  #   style = cell_borders(
  #     sides = "all",
  #     color = "black",
  #     weight = px(2),
  #     style = "solid"
  #   ),
  #   locations = cells_column_labels()  # Aplikace ohraničení i na záhlaví tabulky
  # ) %>%
  # tab_style(
  #   style = cell_borders(
  #     sides = "all",
  #     color = "black",
  #     weight = px(2),
  #     style = "solid"
  #   ),
  #   locations = cells_title(groups = "title")  # Aplikace ohraničení i na titulek tabulky
  # ) %>% 
  # Titulek a zvětšení písma pro titulek
  tab_header(
    title = "Porovnání cest různými dopravními prostředky"
  ) %>%
  tab_style(
    style = list(
      cell_text(size = px(24), weight = "bold")
    ),
    locations = cells_title(groups = "title")
  ) %>%
  # Ztučnění názvů proměnných
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  # Převod sloupců Start a Cíl na uppercase
  tab_style(
    style = cell_text(transform = "uppercase"),
    locations = cells_body(columns = c(start, cil))
  ) %>%
  # Ztučnění sloupce Cíl
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = cil)
  ) %>%
  # Přidání zdrojové poznámky
  tab_source_note(
    source_note = "Data z vyhledávače idos.cz k 10. 10. 2024, řazeno dle doby cestování."
  )

# Zobrazení tabulky
d_table

# TODO podívat se na to, proč se neukládá i titulek 
# Uložení tabulky do Wordu
gtsave(d_table, "tabulky/formatovana_tabulka_dopravni_prostredky.docx", expand = 10)

# vysvětlení k tab_options()
# 1. table_body.hlines.style = "solid"
# Co dělá: Nastavuje styl horizontálních čar mezi řádky dat (uvnitř těla tabulky).
# "solid" znamená, že čára bude pevná (plná čára).
# Další možnosti: "dashed" (čárkovaná), "dotted" (tečkovaná), atd.

# 2. table_body.hlines.width = px(1)
# Co dělá: Nastavuje šířku (tloušťku) horizontálních čar mezi řádky dat (uvnitř těla tabulky).
# px(1) znamená, že tloušťka čáry bude 1 pixel.

# 3. table_body.hlines.color = "black"
# Co dělá: Nastavuje barvu horizontálních čar mezi řádky dat.
# "black" znamená, že čáry budou černé.

# 4. table.border.top.style = "none"
# Co dělá: Nastavuje styl horního ohraničení celé tabulky.
# "none" znamená, že horní ohraničení tabulky nebude zobrazeno (nebude čára).

# 5. table.border.bottom.style = "none"
# Co dělá: Nastavuje styl spodního ohraničení celé tabulky.
# "none" znamená, že spodní ohraničení tabulky nebude zobrazeno (nebude čára).

# 6. column_labels.border.top.style = "none"
# Co dělá: Nastavuje styl horní čáry nad záhlavím sloupců (tj. nad názvy sloupců).
# "none" znamená, že nad záhlavím nebude žádná čára.

# 7. column_labels.border.bottom.style = "solid"
# Co dělá: Nastavuje styl spodní čáry pod záhlavím sloupců.
# "solid" znamená, že pod záhlavím bude pevná čára (plná čára).

# 8. heading.border.bottom.style = "none"
# Co dělá: Nastavuje styl spodní čáry pod titulkem tabulky (pokud je titulek použit).
# "none" znamená, že pod titulkem nebude žádná čára.


# zápis do listů 1 excelu -------------------------------------------------

# Rozdělení dat podle dopravního prostředku
vlak <- d %>% filter(dop_prost == "Vlak")
auto <- d %>% filter(dop_prost == "Auto")
bus <- d %>% filter(dop_prost == "Bus")

# Vytvoření nového Excel souboru se 3 listy - první varianta
wb1 <- createWorkbook()

addWorksheet(wb1, "Vlak")
writeData(wb1, sheet = "Vlak", vlak)

addWorksheet(wb1, "Auto")
writeData(wb1, sheet = "Auto", auto)

addWorksheet(wb1, "Bus")
writeData(wb1, sheet = "Bus", bus)

# Uložení do souboru
saveWorkbook(wb1, "tabulky/cesty_dopravni_prostredky_3_sheety_v01.xlsx", overwrite = TRUE)

dopravni_prostredky <- c("Vlak", "Auto", "Bus")

# Vytvoření nového Excel souboru se 3 listy - druhá varianta s funkcní walk
# Vytvoření workbooku
wb2 <- createWorkbook()

# Filtrování dat a přidání listů do workbooku
walk(dopravni_prostredky, ~ {
  data <- d %>% filter(dop_prost == .x)
  addWorksheet(wb2, .x)
  writeDataTable(wb2, sheet = .x, data)
})

# Uložení do Excel souboru
saveWorkbook(wb2, "tabulky/cesty_dopravni_prostredky_3_sheety_v02.xlsx", overwrite = TRUE)


## Načíst všechny listy z Excelu s readxl i s názvy listů do sloupce
dta_from_excel <- imap_dfr(excel_sheets("tabulky/cesty_dopravni_prostredky_3_sheety_v02.xlsx"),
                            \(x, y) read_excel("tabulky/cesty_dopravni_prostredky_3_sheety_v02.xlsx", sheet = x) |>
                              mutate(source_sheet = x))


# Načtení názvů všech listů s openxlsx i s názvy zdrojových dokumentů (alternativa)
sheet_names <- getSheetNames("tabulky/cesty_dopravni_prostredky_3_sheety_v02.xlsx")

# Načtení všech listů a spojení do jednoho datového rámce s přidaným sloupcem `source_sheet`
dta_from_excel2 <- map_dfr(sheet_names, ~ read.xlsx("tabulky/cesty_dopravni_prostredky_3_sheety_v02.xlsx", sheet = .x) %>%
                            mutate(source_sheet = .x))

# uložit každý dopravní prostředek do samostatného excel souboru ----------
# Funkce pro uložení datasetu s úpravou názvu souboru, ve stejné struktuře jako na Petrově workshopu 
ulozit_dataset_do_excelu <- function(df) {
  prostredek <- unique(df$dop_prost)
  
   # Vytvoření cesty k souboru
  file <- file.path("tabulky", paste0("cesty_map_chr_", prostredek, ".xlsx"))
  
  # Uložení do Excelu
  write_xlsx(df, file)
  return(file)
}

# Rozdělení datasetu podle dopravního prostředku a uložení do Excelu
dopravni_excely <- d %>%
  group_split(dop_prost, .keep = TRUE) %>%
  map_chr(ulozit_dataset_do_excelu)

# group_split(dop_prost, .keep = TRUE) rozdělí dataset podle sloupce dop_prost a vytvoří seznam datových rámců.
# map_chr(ulozit_dataset_do_excelu): Funkce ulozit_dataset_do_excelu() je aplikována na každý datový rámec (odpovídající každému dopravnímu prostředku). Protože tato funkce vrací cestu k uloženému souboru, map_chr() vrátí vektor cest k těmto souborům jako textový vektor.

# for pro uložení dat za každý dopr. prostředek ---------------------------------------------------------------

for (dop_prostredek in dopravni_prostredky) {
  data <- d %>% filter(dop_prost == dop_prostredek)
  write_xlsx(data, paste0("tabulky/cesty_for_", dop_prostredek, ".xlsx"))
}


# map s předdefinovanou funkcí na ukládání datasetu -----------------------

ulozit_dataset <- function(prostredek) {
  data <- d %>% filter(dop_prost == prostredek)
  write_xlsx(data, paste0("tabulky/cesty_map_", prostredek, ".xlsx"))
}

# Použití map() k aplikaci funkce na všechny dopravní prostředky
dopravni_prostredky <- c("Vlak", "Auto", "Bus")
map(dopravni_prostredky, ulozit_dataset)


# map s anonymní funkcí ---------------------------------------------------
map(dopravni_prostredky, ~ write_xlsx(d %>% filter(dop_prost == .x), paste0("tabulky/cesty_map_", .x, ".xlsx")))

# ~ write_xlsx(...): Anonymní funkce, která rovnou aplikuje write_xlsx(), filtrování a ukládání v jednom řádku.
# d %>% filter(dop_prost == .x): Filtruje data podle dopravního prostředku .x v rámci map().

# uložení samostatných sobourů s walk 
walk(dopravni_prostredky, ~ write_xlsx(d %>% filter(dop_prost == .x), paste0("tabulky/cesty_walk_", .x, ".xlsx")))

