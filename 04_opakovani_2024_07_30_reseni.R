
# Načtení balíčků ---------------------------------------------------------
library(tidyverse) # základní balíček pro "data science" obsahuje i ggplot2 nebo stringr https://www.tidyverse.org/ 
library(openxlsx) # pro naččtení excelu 
library(janitor) # funke clean_names()
# library(stringr) # práce s textem - vyhledávání určitých patternů apod. - je už v tidyverse
# library(forcats) # práce s factory - je už v tidyverse
library(tmap) # pro mapy
library(sf) # pro mapy
# library(ggplot2) # pro grafy - je už v tidyverse
library(ggtext) # rozšíření k ggplotu  Improved Text Rendering Support for 'ggplot2'
library(skimr) # souhrnné statistiky
library(RColorBrewer) # barevné palety pro grafy či mapy
library(tmaptools) # barevné palety pro grafy či mapy


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

d <- read.xlsx("data/input/2024_04_pocty_ua_zaku_uprchliku_cvicny_dataset.xlsx", startRow = 2, sheet = "orp")


clean_names(d)

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

d |> # |> je jiné zobrazení pipe operátoru %>% 
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
# pomocí funkce slice_max()
nejvice_5_ms <- d %>% 
  slice_max(ms, n = 5) %>% 
  select(orp, ms) %>% 
  print()

# nebo s arrange() a head() - další varianta, jak to jde udělat (jistě ne poslední možná)
nejvice_5_ms <- d %>% 
  arrange(desc(ms)) %>% 
  head(5) %>% 
  select(orp, ms)

print(nejvice_5_ms)

# pomocí funkce slice_min()
nejmene_5_ms <- d %>% 
  slice_min(ms, n = 5) %>% 
  select(orp, ms)

# nebo s arrange() a head()
nejmene_5_ms <- d %>% 
  arrange(ms) %>% 
  head(5) %>% 
  select(orp, ms)

print(nejmene_5_ms)

# 6. V ORP s největším počtem dětí v MŠ se vloudila chyba, číslo má mít o 2 nuly méně, oprav (např. pomocí mutate + if_else), ukázka kódu

d <- d %>%
  mutate(ms = if_else(ms == 5300, 53, ms))

check <- d %>% 
  filter(orp == "Říčany") %>% 
  select(orp, ms) %>% 
  print()


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
  filter(is.na(kraj)) %>% 
  print()

# v druhém datasetu orp jsou názvy ORP jinak 
check <- orp %>% 
  filter(str_detect(orp, "Brandýs|Jaroměř")) %>% 
  print()


# Nápověda (úkol 12) ----------------------------------------------------------------
# názvy orp se v obou datasetech liší - v 1 je změň podle druhého datasetu (je jedno v jakém je budeš měnit) a poté opakuj spojení datasetů 

d_fin <- d_fin %>%
  select(-kraj) %>% 
  mutate(orp = if_else(orp == "JAROMĚŘ", "Jaroměř", orp),
         # orp = if_else(orp == "JAROMĚŘ", str_to_title("JAROMĚŘ"), orp), # alternativně můžeme v případě Jaroměře převést všechny kapitálky kromě první na malá písmena pomocí funkce str_to_title z balíčku stringr
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
  arrange(desc(prumer_zs)) %>% 
  print()


# TODO: nezapomeň si finální nebo rozpracovaný skript uložit!

saveRDS(d_fin, "data/processed/procvicovaci_workshop_rozpracovany.rds") # můžeme definovat do jaké složky se to ukládá, vždy v rámci hlavního adresáře s projektem workshopy_R

# vizualizace - vycházej ze skriptů z minulého workshopu - vyber si graf, nebo mapu -------------------------------------------------------------


# # 13. GRAF: udělej sloupcový graf s počty dětí v ZŠ v jednotlivých krajích --------
# jestli bude horizontální nebo vertiální je na tobě
# vyber si barvu, která se ti líbí 
# uprav theme, jak se ti líbí 
# uprav název grafu, popisky os 
# uprav velikost a font písem, jak se ti líbí 


# relativně jednoduchý příklad zde, šlo by dál upravovata vylepšovat 
kraje %>% 
  ggplot(aes(x = fct_reorder(kraj, -suma_zs), y = suma_zs)) + # fct_reorder řadí proměnnou graf podle suma_zs, - předtím dělá, že je to sestupně
  geom_col(fill = "blue", width = 0.8) +
  geom_text(aes(label = suma_zs), vjust = -0.5, size = 3) + # labely nad sloupci 
  labs(x = "Kraj", # popisky os, titulku, poznámky
       y = "Počet dětí", 
       title = "Počet ukrajinských uprchlických dětí v ZŠ v jednotlivých krajích",
       caption = "Zdroj: Data MŠMT k dubnu 2024") +
  scale_y_continuous(limits = c(0, 8000), 
                     breaks = seq(0, 8000, by = 1000)) + # upravený rozsah a breaks na ose y
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # nakloněné popisky na ose x aby se tam lépe vešly
        panel.grid.major = element_blank(), # odstranění mřížky v pozadí 
        panel.grid.minor = element_blank(), # odstranění 2. mřížky v pozadí 
        axis.title.y =  element_blank(), # skrytí názvu osy y
        axis.title.x =  element_blank(), # skrytí názvu osy y
        plot.caption = element_textbox(color = "#505050"), # barva poznámky pod grafem
        plot.title = element_text(size = 12, face = "bold"), # úprava podoby titulku 
        plot.margin = unit(c(0.1, 0.5, 0.5, 1.5), "cm")) # okraje grafu, údaje pro top, right, left, bottom - zkratka trouble 

# pozor na to, že grafy i mapy vypadají často trochu jinak v Rstudiu a pak v uloženém obrázku (zejména okraje, to jak se vchází texty), doporučuju přohlížet finální výstup a ideálně i tam, kde má být umístěn - např. ve wordu, na webu apod.

# kód níže ukládá poslední graf, případně lze grafy ukládat do konkrétních objektů - např. graf1 <- kraje %>% ... 
ggsave("grafy/pocty_deti_uprchliku_na_zs_kveten_2024.png", plot = last_plot(), bg= "white", height = 9, width = 15.98, unit = "cm", dpi = 300)




# # 14. MAPA: udělej mapu s počty dětí v MŠ v jednotlivých ORP pomocí balíčku tmap --------

# Poznámka: u propojování datasetů u tmap (např. pomocí left_join) je důležité, aby dataset s geospaciálními daty byl při spojení první - abychom připojovali k němu 
## Načti polygony (hranice) ORP --------------------------------------------

orp_pro_mapy <- RCzechia::orp_polygony()

orp_pro_mapy <- orp_pro_mapy %>% 
  clean_names() %>% 
  rename(orp = naz_orp)

orp_mapy <- left_join(d_fin, orp_pro_mapy, by = "orp")

# vyber si paletu, která se ti líbí - bud z pouzivanych, nebo si definuj vlastni 
# styl který se ti líbí - např pretty, ordered, fixed aj. (u fixed si můžeš definovat vlastní hranice intevralů - tzv. breaks) aj. 
# uprav název mapy


# zobrazení všech barevných palet 
display.brewer.all()

# ulozeni 1 z palet z Brewwer pal 
cervenofialova <- get_brewer_pal("RdPu", n = 5)  # reálně bych volila kratší název, zde pro názornost 

# můžeme nějakou barvu (1 či více k paletě přidat), dále lze samozřejmě definovat i své barvy skrze vektor barev

# Přidání bílé barvy na začátek palety
cervenofialova_s_bilou <- c("#FFFFFF", cervenofialova)
zm5_0 <- c("#FFFFFF", "#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")


# pro získání souřadnic krajů pro obrysy krajů (pokud je chceme v mapě s orp mít)
kraj <- RCzechia::kraje()

kraj <- clean_names(kraj) %>% 
  rename("kraj" ="naz_cznuts3")


# obrys krajů, který lze přidat jako další vrstvu do mapy s ORP
kraj_shape <- tm_shape(kraj) + 
  tm_borders(lwd = 2, col = "black")  


# z nějakého důvodu mi R bez této části kódu psalo chybu "Error: Object orp_mapy is neither from class sf, stars, Spatial, Raster, nor SpatRaster"
# daný kód u mě problé=m vyřešil (napsal mi ho chatgpt)
if (!inherits(orp_mapy, "sf")) {
  orp_mapy <- st_as_sf(orp_mapy)
}

# pozor na to, že grafy i mapy vypadají často trochu jinak v Rstudiu a pak v uloženém obrázku (zejména okraje, to jak se vchází texty), doporučuju přohlížet finální výstup a ideálně i tam, kde má být umístěn - např. ve wordu, na webu apod.

# relativně jednoduchý příklad mapy zde, šlo by dál upravovat a vylepšovat 

pocty_ms <- tm_shape(orp_mapy) +
  tm_polygons("ms", # jaká proměnná se zobrazuje
              palette = cervenofialova, # barevná paleta
              style="fixed", breaks = c(0, 50, 100, 200, 600, 1375), # vlastní nastavení hodnot (např. u stylu "order" se lišila výrazněji jen Praha)
              title = "Počty dětí v MŠ", 
              as.count=TRUE, # jestli se kategorie překrývají 1 -5 a 6-10 nebo 1-5 a 5-10...
              legend.show=TRUE, 
              border.col = "black") + 
  # tm_text("ms", size = 0.6, xmod = 0, ymod = -0.25, remove.overlap = TRUE) + # text s čísly na úrovni ORP není dobře vidět 
  tm_credits(text = "Zdroj: MŠMT, data k dubnu 2024.", size = 0.9, position=c(0.0, 0.05)) +  # poznámka k atům, velikost, pozice
  tm_layout(frame = FALSE,
            legend.outside = FALSE, legend.format = list(text.separator = "až", big.mark = " "),
            legend.title.size = 1.1, legend.text.size = 0.8, legend.title.fontface = "bold",
            legend.position = c(0.9, 0.65), # pozice legendy - 1. číslo na soe x (horizontálně - větší víc doprava), 2. číslo vertikálně, větší víc nahoru, možné použít i výrazy jako "left", "right" "top", "bottom", nebo jejiich kombinaci
            main.title = "Počty ukrajinských dětí uprchlíků v MŠ", 
            main.title.size = 1.1,
            main.title.color = "#0868ac",
            main.title.fontface = "bold",
            inner.margins = c(0.12,0.0,0.02,0), # Vector of four values specifying the bottom, left, top, and right margin
            outer.margins = c(0.01,-0.3,0.01,-0.2)) +
  kraj_shape  # Přidání vrstvy s hranicemi krajů

pocty_ms

tmap_save(pocty_ms, "mapy/pocty_deti_ukrajinskych_uprchliku_v_ms.png", height = 9,  width = 15.98, units = "cm", dpi = 300)
