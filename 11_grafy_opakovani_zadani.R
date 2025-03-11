
# načtení balíčků  --------------------------------------------------------
library(dplyr)  # data manipulation 
library(stringr) # práce s textovými proměnnými 
library(ggplot2) # grafy
library(forcats) # práce s faktory
library(tidyr) # čistění dat, funkce drop_na()
library(janitor) # čištění dat, funkce tabyl 
library(glue) # formátování textových řetězců
library(scales) # funkce pro formátování čísel a dalších hodnot


# instalace fontů - dnes s nimi nepracujeme
# install.packages("extrafont")
# library(extrafont)
# font_import()  # Toto může chvíli trvat
# loadfonts(device = "win")  # Načte fonty pro Windows

# více o ggplotu - učební zdroje 
# https://albert-rapp.de/ggplot-series.html
# https://www.youtube.com/watch?v=IWXcw6NHM6E&list=PLBnFxG6owe1HRvUL6A5QNF_8ujP8NdLMc 
# https://sociology-fa-cu.github.io/uvod-do-r-kniha/obsah-grafu.html
# https://www.youtube.com/watch?v=qnw1xDnt_Ec 
# https://petrbouchal.xyz/eval2020/ 

# načtení dat  -------------------------------------------------------------

d <- readRDS("data/processed/doplneny_dataset_opakovani_tidyverse.rds")

# připojení čísleníků s právnickými formami pro úkol ----------------------

c_pr_forma_org <- read.csv("data/input/rejstrikova_data_ekonomicke_subjekty/CIS0056_CS.csv")

c_pr_forma_org <- c_pr_forma_org %>%
  select(pr_forma_org_kod_res = chodnota,
         pr_forma_org_res = text)

d <- d %>%
  left_join(c_pr_forma_org, by = c("forma" ="pr_forma_org_kod_res"))

tabyl(d$pr_forma_org_res)

tabyl(d$rok_vzniku)

# kategorická proměnná pdole roku vzniku
d <- d %>%
  mutate(rok_vzniku_kat = factor(case_when(
    rok_vzniku < 2000 ~ "do 2000",
    rok_vzniku > 2000  & rok_vzniku < 2011 ~ "2001 - 2010",
    rok_vzniku > 2010  & rok_vzniku < 2021 ~ "2011 - 2020",
    rok_vzniku > 2020   ~ "2021 - 2025",
    is.na(rok_vzniku) ~ NA_character_,
  ),
  levels = c("do 2000", "2001 - 2010", "2011 - 2020", "2021 - 2025")))

tabyl(d$rok_vzniku_kat)
tabyl(d$pocet_zam_kat)

# sloučení kategorií podle počtu zaměstnanců do 3
d <- d |> 
  mutate(pocet_zam_3kat = factor(case_when(
    pocet_zam_kat %in% c("0", "Neuvedeno") ~ "Bez zaměstnanců",
    pocet_zam_kat == "do 9" ~ "Do 9 zaměstnanců",
    is.na(pocet_zam_kat) ~ NA_character_,
    TRUE ~ "9+ zaměstnanců"
  ),
  levels = c("Bez zaměstnanců", "Do 9 zaměstnanců", "9+ zaměstnanců"))
  )

tabyl(d$pocet_zam_3kat)

colnames(d)


# syntax ggplotu  ---------------------------------------------------------
ggplot()

ggplot(data = d)

ggplot(data = d, aes(x = rok_vzniku)) 

ggplot(data = d, aes(x = rok_vzniku)) +
  geom_bar()

ggplot(data = d, aes(y = pocet, x = rok_vzniku))

d_pocty <- d |> 
  drop_na(rok_vzniku)|> 
  filter(rok_vzniku > 1989) |> 
  group_by(rok_vzniku) |> 
  summarise(pocet = n())

d_pocty_kat <- d |> 
  drop_na(rok_vzniku)|> 
  group_by(rok_vzniku_kat) |> 
  summarise(pocet = n())

ggplot(data = d_pocty, aes(y = pocet, x = rok_vzniku)) + 
  geom_col()

# název dataframu může být před funkcí ggplot, šikovnější na různé filtrování, můžeme také data transoformovat a pak přímo navázat ggplotem
d_pocty |> 
  # filter(x) |> 
ggplot(aes(y = pocet, x = rok_vzniku)) + 
  geom_col()

# geom_col + geom_line
ggplot(data = d_pocty, aes(y = pocet, x = rok_vzniku)) + 
  geom_col(fill = "darkblue", color = "orange") +
  geom_line(aes(group = 1), color = "red", size = 1, alpha = 0.4) +  # křivka, barva, velikost/tloušťka, průhlednost (0 neviditelná, 1 nejsytější)
  geom_point(color = "darkred", size = 2, alpha = 0.9, shape = 16) +  
  labs(title = "Počet vzniklých ekonomických subjektů od roku 1990",
       x = "Rok vzniku ekonomického subjektu",
       y = NULL,
       caption = "Data k 31. 1. 2025, pouze vzorek 10 000 ekonomických subjektů.") +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  theme_minimal() 

# TODO 
# Zkus v grafu výše změnit textové popisky a intervaly na ose x
# zobraz ve sloupcovém grafu zobrazit souhrnné počty podle zpusob_zaniku (bez NA)
# vyzkoušej si změnit barvu výplně i obrysu sloupců





# Aesthetic specifications - linetype, shapes apod.
# https://ggplot2.tidyverse.org/articles/ggplot2-specs.html 

# když chceme zvýraznit nějakou hodnotu/hodnoty
ggplot(data = d_pocty, aes(y = pocet, x = rok_vzniku, fill = rok_vzniku == 2025)) + 
  geom_col() +
  geom_line(aes(group = 1), color = "red", size = 1, alpha = 0.4) +  
  geom_point(color = "darkred", size = 2, alpha = 0.9, shape = 16) +  
  labs(title = "Počet vzniklých ekonomických subjektů od roku 1990",
       x = "Rok vzniku ekonomického subjektu",
       y = NULL,
       caption = "Data k 31. 1. 2025, pouze vzorek 10 000 ekonomických subjektů.") +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "grey")) +  # 2025 modře, ostatní šedě
  theme_minimal() +
  theme(legend.position = "none")  # Skrytí legendy

# přidání texti s annotate a šipky 
ggplot(data = d_pocty, aes(y = pocet, x = as.numeric(rok_vzniku), fill = rok_vzniku == 2025)) + 
  geom_col() +
  geom_line(aes(group = 1), color = "red", size = 1, alpha = 0.4) +  
  geom_point(color = "darkred", size = 2, alpha = 0.9, shape = 16) +  
  labs(title = "Počet vzniklých ekonomických subjektů od roku 1990",
       x = "Rok vzniku ekonomického subjektu",
       y = NULL,
       caption = "Data k 31. 1. 2025, pouze vzorek 10 000 ekonomických subjektů.") +
  scale_x_continuous(breaks = seq(1990, 2025, by = 5)) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "grey")) +  # 2025 modře, ostatní šedě
  theme_minimal() +
  annotate("segment", x = 2020, xend = 2020, y = max(d_pocty$pocet) * 0.6, yend = max(d_pocty$pocet) * 0.9, 
           arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  annotate("text", x = 2020, y = max(d_pocty$pocet) * 0.95, label = "Významný rok", color = "black", size = 4, fontface = "bold") +
 theme(legend.position = "none")


# nastavení theme set -----------------------------------------------------
# optimální mít definovanou hlavní 1 barvu do sloupcových grafů i přesně definované barevné palety pro různé typy grafů, včetně barev pro neutrální kategorie typu "Nevím", "Chybějící data" apod.
# optimální co nejvíce hodnot načítat z dat

zluta <- "#F7A000"
modra <- "#15448b"

# výchozí nastavení, pokud do grafu přidáme hlavní theme např theme_bw() tak to tím přepíšeme 
theme_set(
  theme_classic(base_size = 12) + # veliksot písem
    theme(
      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 20)), # velikost titulku, tučně, větší prostor pod titulkem 
      plot.subtitle = element_text(size = 13),
      plot.caption = element_text(color = "#8d8d8d"),
      strip.background = element_blank(), # odstranění pozadí pruhu (strip), který se používá v facetovaných grafech (facet_wrap() nebo facet_grid()
      axis.title = element_text(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title.x = element_text(color = "black"),
      axis.title.y =element_text(color = "black")
    )
)



# řazení sloupcových grafů ------------------------------------------------------------
# výchozí: u kategorických dat podle abecedy nebo pořadí faktorů 

min_rok <- min(d$rok_vzniku, na.rm = TRUE)
subjektu_pred_1990 <- d |> filter(rok_vzniku < 1990) |> nrow()


# změna pořadí leveů faktorů s fct_reorder, funguje i samotné reorder
d_pocty_kat |> 
  drop_na(rok_vzniku_kat)|> 
  mutate(rok_vzniku_kat = fct_reorder(rok_vzniku_kat, -pocet)) |> # změna faktorů podle pocet sestupně
  ggplot(aes(y = pocet, x = rok_vzniku_kat)) + 
  geom_col(fill = modra) +
  geom_text(aes(label = label_comma(big.mark = " ")(pocet)), vjust = -0.5, size = 3) +
  # geom_text(aes(label = scales::number(pocet, big.mark = " "), vjust = -0.5), size = 3, show.legend = FALSE) + # Použití scales::number
  # geom_text(aes(label = format(pocet, big.mark = " "), vjust = -0.5), size = 3, show.legend = FALSE) + # Použití base R format
  labs(title = glue("Počet vzniklých ekonomických subjektů od roku {min_rok} podle období vzniku"),
       x = "Období vzniku ekonomického subjektu",
       y = NULL,
       caption = "Data k 31. 1. 2025, pouze vzorek 10 000 ekonomických subjektů. Počet ekonomických subjetků před rokem 1990 marginální.") +
  scale_y_continuous(labels = label_comma(big.mark = " "), expand = expansion(mult = c(0, 0.1)))  # 10% extra prostoru nad maximální hodnotu na ose y


# zalomování textů --------------------------------------------------------

# příliš dlouhé texty, můžeme zalomit manuálně na nový řádek pomocí \n (newline)
d_pocty_kat |> 
  drop_na(rok_vzniku_kat)|> 
  mutate(rok_vzniku_kat = fct_reorder(rok_vzniku_kat, -pocet)) |> # změna faktorů podle pocet sestupně
  ggplot(aes(y = pocet, x = rok_vzniku_kat)) + 
  geom_col(fill = zluta) +
  labs(title = glue("Počet vzniklých ekonomických subjektů od roku {min_rok} podle období vzniku"),
       x = "Období vzniku ekonomického subjektu",
       y = NULL,
       caption = glue("Data k 31. 1. 2025, pouze vzorek 10 000 ekonomických subjektů.\nPočet ekonomických subjetků před rokem 1990 marginální (pouze {subjektu_pred_1990})."))

# různá nastavení v geom_col(), zalomení textu se str_wrap po x znacích 
d_pocty_kat |> 
  drop_na(rok_vzniku_kat)|> 
  mutate(rok_vzniku_kat = fct_reorder(rok_vzniku_kat, -pocet)) |> # změna faktorů podle pocet sestupně
  ggplot(aes(y = pocet, x = rok_vzniku_kat)) +
  geom_col(fill = modra, alpha = 0.8, color = "black", linewidth = 1.5, linetype = "dashed") +
  geom_text(aes(label = label_comma(big.mark = " ")(pocet)), vjust = -0.5, size = 4) +
  labs(title = str_wrap(glue("Počet vzniklých ekonomických subjektů od roku {min_rok} podle období vzniku"), width = 50),
       x = "Období vzniku ekonomického subjektu",
       y = NULL,
       caption = str_wrap(glue("Data k 31. 1. 2025, pouze vzorek 10 000 ekonomických subjektů.\nPočet ekonomických subjektů před rokem 1990 marginální (pouze {subjektu_pred_1990})."), width = 90)
  )  +
  scale_y_continuous(labels = label_comma(big.mark = " "), expand = expansion(mult = c(0, 0.1)))  # 10% extra prostoru nad maximální hodnotu na ose y




# ukládání ----------------------------------------------------------------
# optimální šířka pro word, můžeme uložit buď poslední graf nebo pojmenovaný konkrétní graf
ggsave(paste0("grafy/geom_col_ukazka.png"),  plot = last_plot(), bg= "white", height = 9, width = 15.98, unit = "cm", dpi = 300)


# zobrazení více skupin zde podle počtu zaměstnanců-------------------------------------------------------------

d_pocty_kat_velikost <- d |> 
  drop_na(rok_vzniku_kat, pocet_zam_3kat) |> 
  group_by(rok_vzniku_kat, pocet_zam_3kat) |> 
  summarise(pocet = n(), .groups = "drop") |> 
  group_by(rok_vzniku_kat) |> 
  mutate(podil = pocet / sum(pocet))  

## vedle sebe (position dodge)
d_pocty_kat_velikost |> 
  ggplot(aes(x = rok_vzniku_kat, y = pocet, fill = pocet_zam_3kat)) +  # Sloupce podle roku a kategorií
  geom_col(position = "dodge", alpha = 0.7) +  # Sloupce vedle sebe
  scale_fill_manual(values = c("Bez zaměstnanců" = "red", "Do 9 zaměstnanců" = "#00BFFF", "9+ zaměstnanců" = "green")) +  # Nastavení barev pro jednotlivé kategorie
  labs(title = "Počet vzniklých ekonomických subjektů podle velikosti",
       x = "Období vzniku ekonomického subjektu",
       y = "Počet subjektů",
       fill = NULL) +
  theme(legend.position = "top")


## stacked s počtem 
d_pocty_kat_velikost |> 
  ggplot(aes(x = rok_vzniku_kat, y = pocet, fill = pocet_zam_3kat)) +  # Sloupce podle roku a kategorií
  geom_col(position = "stack", alpha = 0.7) +  # Sloupce na sobě (stacked)
  scale_fill_manual(values = c("Bez zaměstnanců" = "red", "Do 9 zaměstnanců" = "#00BFFF", "9+ zaměstnanců" = "green")) +  # Nastavení barev pro jednotlivé kategorie
  labs(title = "Počet vzniklých ekonomických subjektů podle velikosti",
       x = "Období vzniku ekonomického subjektu",
       y = "Počet subjektů",
       fill = NULL) +
  theme(legend.position = "top")

# stacked podíly - buďto pozice fill (zde), nebo na osu y podil místo počet (níže)
d_pocty_kat_velikost |> 
  ggplot(aes(x = rok_vzniku_kat, y = pocet, fill = pocet_zam_3kat)) +  # Sloupce podle roku a kategorií
  geom_col(position = "fill", alpha = 0.7) +  # Sloupce na sobě (stacked)
  scale_fill_manual(values = c("Bez zaměstnanců" = "red", "Do 9 zaměstnanců" = "#00BFFF", "9+ zaměstnanců" = "green")) +  # Nastavení barev pro jednotlivé kategorie
  labs(title = "Počet vzniklých ekonomických subjektů podle velikosti",
       x = "Období vzniku ekonomického subjektu",
       y = "Podíl subjektů",
       fill = NULL) +
  scale_y_continuous(labels = label_percent(scale = 100, suffix = " %", accuracy = 1)) +  # Osa Y jako procenta s mezerou před %
  theme(legend.position = "top")

# stacked podíly - na ose y podil misto počtu, pak by se pozice fill a stack neměla lišit, přidání labelů 
d_pocty_kat_velikost |> 
  ggplot(aes(x = rok_vzniku_kat, y = podil, fill = pocet_zam_3kat)) +  # Sloupce podle roku a kategorií
  geom_col(position = "fill", alpha = 0.7) +  # Sloupce na sobě (stacked)
  scale_fill_manual(values = c("Bez zaměstnanců" = "red", "Do 9 zaměstnanců" = "#00BFFF", "9+ zaměstnanců" = "green")) +  # Nastavení barev pro jednotlivé kategorie
  labs(title = "Podíly vzniklých ekonomických subjektů podle velikosti",
       x = "Období vzniku ekonomického subjektu",
       y = "Podíl subjektů",
       caption = "Zobrazeny pouze podíly větší než 2 %. Pouze vzorek 10 000 subjektů z RES.",
       fill = NULL) +
  scale_y_continuous(labels = label_percent(scale = 100, suffix = " %", accuracy = 1)) +  # Osa Y jako procenta s mezerou před %
  geom_text(aes(label = ifelse(pocet / sum(pocet) > 0.02, label_percent(accuracy = 1)(pocet / sum(pocet)), "")), 
            position = position_stack(vjust = 0.5),  # Zobrazení textu uprostřed sloupce
            color = "black", size = 3.5) +
  annotate("text", x = "2021 - 2025", y = 0.8, label = "Poslední data", color = "black", size = 3, fontface = "bold") +
  theme(legend.position = "top")



## facety ------------------------------------------------------------------
### facet wrap --------------------------------------------------------------

# Facety jsou rozděleny automaticky podle počtu, který určíte pomocí argumentu ncol (počet sloupců) nebo nrow (počet řádků).
d_pocty_kat_velikost |> 
  ggplot(aes(x = rok_vzniku_kat, y = pocet, fill = pocet_zam_3kat)) +  # Sloupce podle roku a kategorií
  geom_col(position = "dodge", alpha = 0.7) +  # Sloupce vedle sebe
  scale_fill_manual(values = c("Bez zaměstnanců" = "red", "Do 9 zaměstnanců" = "#00BFFF", "9+ zaměstnanců" = "green")) +  # Nastavení barev pro jednotlivé kategorie
  labs(title = "Počet vzniklých ekonomických subjektů podle velikosti",
       x = "Období vzniku ekonomického subjektu",
       y = "Počet subjektů",
       fill = NULL) +
  theme(
    legend.position = "none",  # Odstranění legendy
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotace textu na ose X pro lepší čitelnost
    strip.text = element_text(size = 12),  # Velikost textu pro facety
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) +
  facet_wrap(~ pocet_zam_3kat, scales = "free_y")  # scales = "free_y" pokud nechceme jednotnou osu y, podobně se dá aplikovat i na x (zde nedává smysl)



### facet grid --------------------------------------------------------------
# Umožňuje více kontroly, zda se to má dělit do řádků nebo sloupců.
d_pocty_kat_velikost |> 
  ggplot(aes(x = rok_vzniku_kat, y = pocet, fill = pocet_zam_3kat)) +  # Sloupce podle roku a kategorií
  geom_col(position = "dodge", alpha = 0.7) +  # Sloupce vedle sebe
  scale_fill_manual(values = c("Bez zaměstnanců" = "red", "Do 9 zaměstnanců" = "#00BFFF", "9+ zaměstnanců" = "green")) +  # Nastavení barev pro jednotlivé kategorie
  labs(title = "Počet vzniklých ekonomických subjektů podle velikosti",
       x = "Období vzniku ekonomického subjektu",
       y = "Počet subjektů",
       fill = NULL) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotace textu na ose X pro lepší čitelnost
    strip.text = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ) +
  facet_grid(rows = vars(pocet_zam_3kat), scales = "fixed")  # Použití facet_grid pro společnou osu X




# jiné --------------------------------------------------------------------

# pro bleskové zobrazení počtů lze použít funkci hist pro histogram v rámci R base
hist(d$rok_vzniku)

# jinak lze klasicky histogram i v ggplotu
d %>% 
  drop_na(rok_vzniku) %>% 
  ggplot(aes(x = rok_vzniku)) + 
  geom_histogram(binwidth = 1, fill = "turquoise", color = "black") + 
  labs(title = str_wrap(glue("Počet vzniklých ekonomických subjektů od roku {min_rok} podle období vzniku"), width = 50),
       x = "Období vzniku ekonomického subjektu",
       y = NULL,
       caption = str_wrap(glue("Data k 31. 1. 2025, pouze vzorek 10 000 ekonomických subjektů.\nPočet ekonomických subjektů před rokem 1990 marginální (pouze {subjektu_pred_1990})."), width = 90)
  )  +
  scale_y_continuous(labels = label_comma(big.mark = " "), expand = expansion(mult = c(0, 0.1)))  # 10% extra prostoru nad maximální hodnotu na ose y

# další tipy 
# histogram s upravenými labely pro intervaly na ose x 

# vygenerovaný dataset
set.seed(123)  # Pro reprodukovatelnost

# Definujeme pravděpodobnosti – větší pro věk 30-60
vekove_kategorie <- c(0:100)
pravdepodobnosti <- dnorm(vekove_kategorie, mean = 45, sd = 15)  # Normální rozdělení se středem 45 let
pravdepodobnosti <- pravdepodobnosti / sum(pravdepodobnosti)  # Normalizace na součet 1

vek <- tibble(
  vek = sample(vekove_kategorie, size = 1000, replace = TRUE, prob = pravdepodobnosti),
  pohlavi = sample(c("Muž", "Žena"), size = 1000, replace = TRUE, prob = c(0.31, 0.69))
)


ggplot(data = vek, aes(x = vek)) +
  geom_histogram(binwidth = 20, fill = modra, color = "black", alpha = 0.7) +
  scale_x_continuous(
    breaks = seq(0, 100, by = 20), # Odpovídá začátkům intervalů
    labels = c("0-19", "20-39", "40-59", 
               "60-79", "80-99", "100+")) 


# pyramidový graf ---------------------------------------------------------
# příprava dat - počty pozorování pro věky a hodnoty mužů dát jako záporné
vek_pyr <-  vek |>  
  group_by(vek, pohlavi) |> 
  summarise(pocty = n()) |> 
  mutate(
    pocty = ifelse(pohlavi == "Muž", -pocty, pocty))

ggplot(vek_pyr, aes(x = vek, y = pocty, fill = pohlavi)) +
  # Sloupce s ohraničením černou barvou
  geom_bar(stat = "identity", width = 0.9, color = "black", linewidth = 0.05) +
  
  # Nastavení osy y interval, popisky na ose x, převedené záporných hodnot u mužů na kladné 
  scale_y_continuous(
    limits = c(-30, 30),
    breaks = seq(-30, 30, by = 5),
    labels = function(x) label_number(big.mark = " ", accuracy = 1)(abs(x)),  # Formátování s absolutními hodnotami
    name = "Počty"  # Popisek osy Y
  ) +
  
  # Nastavení osy x (Věk) s odstraněním rozšíření
  scale_x_continuous(name = "Věk", breaks = seq(0, 100, by = 5), expand = c(0, 0)) +
  
  # Otočení os a přidání názvů
  coord_flip() +
  labs(title = "Věková skladba mužů a žen", 
       fill = "Pohlaví",
       caption = "Cvičná data.") +
  
  # Nastavení barev
  scale_fill_manual(values = c("Muž" = modra, "Žena" = zluta), labels = c("Muži", "Ženy")) +
  theme(
    legend.position = c(0.9, 0.9)) +
  labs(title = "Věková skladba mužů a žen", 
       fill = "Pohlaví",
       caption = "Cvičná data.") 


ggsave(paste0("grafy/pyramidovy_graf.png"),  plot = last_plot(), bg= "white", height = 10, width = 15.98, unit = "cm", dpi = 300)


# likertova škála, různá barva labelů  se scale_color manual ------------------------------------

# Vytvoření ručních dat pro dvě otázky
data <- tibble(
  question = c(rep("Question 1", 5), rep("Question 2", 5)),
  answer = rep(c("Určitě souhlasím", "Spíše souhlasím", "Nevím", "Spíše nesouhlasím", "Určitě nesouhlasím"), 2),
  percent = c(25, 15, 20, 25, 15, 30, 20, 2, 28, 20) # Ručně nastavené hodnoty
)

data$answer <- factor(data$answer, levels = c("Určitě nesouhlasím", "Spíše nesouhlasím", "Nevím", "Spíše souhlasím", "Určitě souhlasím"))

levels(data$answer)

# Barvy pro graf
colors <- c(
  "Určitě souhlasím" = "darkgreen",
  "Spíše souhlasím" = "lightgreen",
  "Nevím" = "grey",
  "Spíše nesouhlasím" = "lightcoral",
  "Určitě nesouhlasím" = "darkred"
)

# Vytvoření stacked sloupcového grafu
ggplot(data, aes(x = question, y = percent, fill = answer)) +
  geom_col(position = "stack", color = "black", linewidth = 0.02) +
  geom_text(
    aes(label = ifelse(percent > 5, paste0(percent, " %"), ""),
        color = answer), # Přidání estetiky color podle odpovědi)
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  coord_flip() + # převrací graf, aby byl horizontálně 
  scale_fill_manual(values = colors) +
  labs(
    title = "Likertova škála pro dvě otázky",
    x = "Otázka",
    y = "Podíl (%)",
    fill = ""
  ) +
  scale_color_manual(
    values = c("Určitě souhlasím" = "white", "Spíše souhlasím" = "black", "Nevím" = "black", "Spíše nesouhlasím" = "black", "Určitě nesouhlasím" = "white"), 
    guide = "none") + # Skrytí legendy pro barvy textu +
  guides(fill = guide_legend(reverse = TRUE)) + # často je potřeba legendu převrátit
  theme(axis.ticks = element_blank(), # Skrytí čárek na osách u hodnot 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        legend.position = "top",
        legend.margin = margin(1, 1, -8, -15),
        legend.text = element_text(size = 9), 
        legend.key.width = unit(0.2, 'cm'))

# TODO zkus otočit pořadí barev v grafu i v legendě

# Uložení grafu
ggsave("grafy/likertovky.png", plot = last_plot(), bg = "white", height = 6, width = 15.98, unit = "cm", dpi = 300)





# TODO ÚKOL
# vyfiltruj okresy, kde je více než 100 subjektů
# a zobraz je sloupcovým grafem 
# potom přidej do grafu 2 nejčastější formy (v našem vzorku) podle pr_forma_org_res "Společnost s ručením omezeným" a "Fyzická osoba podnikající dle živnostenského zákona", pro lepší zobrazení v grafu názvy zkrať 


# Vaše tipy k tvorbě grafů, řazení, práci s textem?
