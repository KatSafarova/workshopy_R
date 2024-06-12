
# načtení balíčků  --------------------------------------------------------

library(dplyr)  # data manipulation 
library(stringr) # práce s textovými proměnnými 
library(ggplot2) # grafy
library(forcats) # práce s faktory


# instalace fontů 
# install.packages("extrafont")
# library(extrafont)
# font_import()  # Toto může chvíli trvat
# loadfonts(device = "win")  # Načte fonty pro Windows



# načtení dat  -------------------------------------------------------------

d <- readRDS("data/processed/cvicny_dataset_clean.rds")


# HISTOGRAM ---------------------------------------------------------------

# histogram base plot---------------------------------------------------------------

hist(d$obvykla_delka_spanku)

# Otevření grafického zařízení pro PNG soubor
png("grafy/histogram_base_obvykla_delka_spanku.png", width = 800, height = 600, res =300)

hist(d$obvykla_delka_spanku, col = "purple", # změna barvy
     breaks = seq(3, 10, by = 1), # rozsah sloupců, po jaké hodnotě se dělí (podrobnost)
     xlim = c(3, 15), # limitní hodnoty na ose X
     main = "Obvyklá délka spánku v hodinách",  # titulek
     xlab = "počet hodin", # název osy X
     ylab = "počet respondentů") # název osy Y

# Uzavření grafického zařízení
dev.off()

spanek <- hist(d$obvykla_delka_spanku, col = "#0868ac", 
               main = "Obvyklá délka spánku v hodinách",  # titulek
               breaks = seq(4, 10, by = 1), # rozsah sloupců a interval (jak podrobně se člení)
               ylim = c(0, 25), # limitní hodnoty na ose y
               xlab = "počet hodin", # název osy X
               ylab = "počet respondentů", # název osy Y
               xaxt = 'n') # na ose x nejsou žádné číselné popisky


# přidání počtů nad jednotlivé sloupce
text(spanek$mids, spanek$counts, labels = spanek$counts, pos = 3, cex = 0.8, col = "black") 

# spanek$mids: Toto jsou střední hodnoty (midpoints) intervalů sloupců histogramu.
# spanek$counts: Toto jsou frekvence nebo počty hodnot v jednotlivých intervalech sloupců histogramu.
# labels = spanek$counts: Toto jsou hodnoty, které se mají zobrazit jako popisky.
# pos = 3: Tato hodnota určuje pozici textu. Hodnota 3 znamená, že text bude umístěn nad bodem.
# cex = 0.8: Toto je relativní velikost textu. Čím vyšší hodnota, tím větší text. Zde je text zmenšen na 80 % běžné velikosti.


# Definování bodů a popisků na osu x (po 5000)
x_ticks <- seq(0, 14, by = 0.5)

# Přidání vlastních popisků na osu x s menším písmem a horizontální orientací
axis(1, at = x_ticks, labels = x_ticks, las = 2, cex.axis = 0.8)

# Tato funkce axis() slouží k přidání popisků na osu X existujícího grafu. Podrobněji:

# 1 v prvním argumentu (side) značí, že se jedná o osu X.
# at = x_ticks: Toto jsou pozice, na kterých mají být popisky umístěny. V tomto případě jsou zde uloženy hodnoty proměnné x_ticks, která byla definována dříve.
# labels = x_ticks: Toto jsou hodnoty, které mají být zobrazeny jako popisky na ose X. Tyto hodnoty jsou také zde uloženy v proměnné x_ticks.
# las = 1: Tato volba určuje orientaci popisků. Hodnota 1 znamená, že popisky budou umístěny horizontálně (las = 0 by je umístil vertikálně).
# cex.axis = 0.8: Tato volba určuje velikost textu na ose X. Hodnota 0.8 znamená, že velikost textu na ose X bude 80 % běžné velikosti.
# Celkově funkce axis() slouží k přizpůsobení popisků na ose X podle specifikací definovaných v argumentech. V tomto konkrétním případě umisťuje popisky na osu X v souladu s hodnotami v proměnné x_ticks a nastavuje jejich orientaci na horizontální. Navíc nastavuje velikost textu na ose X na 80 % běžné velikosti.


# histogram ggplot --------------------------------------------------------

ggplot()

ggplot(data = d, aes(x = obvykla_delka_spanku))

# základní histogram 
ggplot(data = d, aes(x = obvykla_delka_spanku)) + 
  geom_histogram()

# histogram s úpravami
d %>% 
  drop_na(obvykla_delka_spanku) %>% 
  ggplot(aes(x = obvykla_delka_spanku)) + 
  geom_histogram(binwidth = 1, fill = "turquoise", color = "black") + 
  labs(title = "Obvyklá délka spánku v hodinách",
       x = "počet hodin",
       y = "počet respondentů") + 
  theme_classic() + 
  theme(panel.grid.major = element_blank(),   # odstraní hlavní mřížku
        panel.grid.minor = element_blank())   # odstraní menší mřížku


# histogram s více kategoriemi, vzdělání + úprava velikostí a stylu fontů 
d %>% 
  drop_na(obvykla_delka_spanku) %>% 
  ggplot(aes(x = obvykla_delka_spanku, fill = vzdelani_3kat)) + 
  geom_histogram(binwidth = 1,  position = "dodge", alpha = 0.5) +
  labs(title = "Obvyklá délka spánku v hodinách",
       x = "počet hodin",
       y = "počet respondentů",
       fill = "Vzdělání") +
  scale_fill_manual(values = c("red", "#00BFFF", "green")) + # Nastavení barev pro jednotlivé kategorie
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "italic"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10))

# facety 
d %>% 
  drop_na(obvykla_delka_spanku) %>% 
  ggplot(aes(x = obvykla_delka_spanku, fill = vzdelani_3kat)) + 
  geom_histogram(binwidth = 1,  position = "dodge", alpha = 0.5) +
  facet_wrap(~ vzdelani_3kat) + # Přidání facetování na základě kategorie vzdělání
  labs(
    title = "Obvyklá délka spánku v hodinách podle vzdělání",
    x = "Počet hodin",
    y = "Počet respondentů",
    fill = "Vzdělání"
  ) +
  scale_fill_manual(values = c("red", "#00BFFF", "green")) + # Nastavení barev pro jednotlivé kategorie
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "italic"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 14, face = "bold") # Nastavení velikosti a stylu fontu pro názvy facetu
  )

ggsave("grafy/obvykla_delka_spanku_podle_vzdelani.png", plot = last_plot(), bg= "white", height = 7, width = 15.98, unit = "cm", dpi = 300)


# https://www.klikzone.cz/sekce-html/html-barvy.php


# histogram s více kategoriemi, pohlaví
d %>% 
  drop_na(obvykla_delka_spanku) %>% 
  ggplot(aes(x = obvykla_delka_spanku, fill = pohlavi)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.9) +
  labs(title = "Obyvklá délka spánku v hodinách",
       x = "počet hodin",
       y = "počet respondentů",
       fill = "Vzdělání") +
  theme_classic () +
  scale_fill_manual(values = c("blue", "red")) # Nastavení barev pro jednotlivé kategorie


# Úkol
# Udělejte histogram pomocí ggplotu k dsitrbuci věku 
# Poté zkuste požít facety - vytvořit dva grafy v 1 podle pohlaví 
# Použijte jiné barvy než máme ve skriptu
# facetový graf si uložte 


# SCATTERPLOT ---------------------------------------------------------------

# scatterplot base ---------------------------------------------------------------
x <- d$vek
y <- d$obvykla_delka_spanku

plot(x, y, main = "Vztah mezi věkem a počtem hodin spánku",
     xlab = "věk", ylab = "obvyklý počet hodin spánku",
     pch = 21, frame = FALSE)

# Definujte barvy pro každé pohlaví
gender_colors <- c("muži" = "blue", "ženy" = "red") 

# využijte barvy pro pohlaví
plot(x, y, main = "Vztah mezi věkem a počtem hodin spánku",
     xlab = "věk", ylab = "obvyklý počet hodin spánku",
     pch = 8, frame = TRUE, col = gender_colors[d$pohlavi])

abline(lm(y ~ x, data = d), col = "darkgreen", lwd = 2)



# scttaerplot ggplot ------------------------------------------------------

ggplot(data = d, aes(x=vek, y = obvykla_delka_spanku)) +
  geom_point() 

ggplot(data = d, aes(x=vek, y = obvykla_delka_spanku, color = pohlavi)) +
  geom_point(shape = 17, size = 2, alpha =0.7) +
  theme_bw()

gender_colors <- c("muži" = "blue", "ženy" = "red")

d %>% 
  drop_na(obvykla_delka_spanku) %>% 
  ggplot(aes(x = vek, y = obvykla_delka_spanku, color = pohlavi)) + 
  geom_point(shape = 6) + 
  scale_color_manual(values = gender_colors) +  # Použije definované barvy
  labs(
    title = "Vztah mezi věkem a počtem hodin spánku",
    x = "věk",
    y = "obvyklý počet hodin spánku",
    color = "Pohlaví"
  ) + 
  theme_classic() +  
  geom_smooth(method = "lm", color = "darkgreen",  linetype = "dotted", size = 1)  # Přidá regresní čáru (výchozí typ "solid")


# BARPLOT -----------------------------------------------------------------

# barplot base -----------------------------------------------------------------
# Vypočteme průměrnou délku spánku pro každé pohlaví
prum_spanek_pohlavi <- d %>%
filter(!is.na(obvykla_delka_spanku)) %>% # Filtrujte NA hodnoty délky spánku
  group_by(pohlavi) %>% # Seskupit data podle pohlaví
  summarize(prum_spanek = mean(obvykla_delka_spanku))

prum_spanek <- prum_spanek_pohlavi$prum_spanek

# Vytvoříme barplot s průměrnou délkou spánku na ose y
barplot(prum_spanek,
        main = "Průměrná délka spánku podle pohlaví",
        xlab = "Pohlaví",
        ylab = "Průměrná délka spánku (hodiny)",
        col = c("blue", "red"),
        legend = levels(d$pohlavi))

# prum_spanek <- tapply(d$obvykla_delka_spanku, d$pohlavi, mean, na.rm=TRUE)



# barplot ggplot ----------------------------------------------------------
# geom_bar() a geom_col()

ggplot(data = d, aes(x = pohlavi)) +
  geom_bar(color = "orange", width = 0.5) +  # Určí šířku sloupců na 0.5
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5) +
  labs(x = "Pohlaví", y = "Počet respondentů", title = "Počet mužů a žen ve vzorku")

# dejme tomu, že chceme zobrazit ve slopupcovém grafu průměrný vek pro muže a ženy
# pro 2 proměnné se používá geom_col
# nejdřív si musíme průměrný věk pro obě pohalví spočítat 
vek_muzi_zeny <- d %>%
  group_by(pohlavi) %>%
  summarize(mean_vek = mean(vek, na.rm = TRUE))

# Existují dva typy sloupcových grafů: geom_bar() a geom_col(). 
# geom_bar() způsobí, že výška sloupce je úměrná počtu případů v každé skupině. 
# Pokud chcete, aby výška sloupců odpovídala hodnotám v datech, použijte místo toho funkci geom_col().

vek_muzi_zeny %>% 
  drop_na(mean_vek) %>% 
  ggplot(aes(x = pohlavi, y = mean_vek)) +
  geom_col(color = "green", fill = "purple",  width = 0.5) + 
  geom_text(aes(label = round(mean_vek, 1)), vjust = -0.5, color = "black") +
  labs (x = "Věk", y = "průměrný počet hodin spánku", title = "Kolik hodin spí muži a ženy") 

# + 
#   coord_flip()


# ggplot(data = vek_muzi_zeny, aes(x = pohlavi, y = mean_vek)) +
#   geom_bar(color = "green", fill = "purple", width = 0.5, stat = "identity") + 
#   geom_text(aes(label = round(mean_vek, 1)), vjust = -0.5, color = "black") +
#   labs(x = "Věk", y = "Průměrný počet hodin spánku", title = "Kolik hodin spí muži a ženy")

vek_muzi_zeny %>% 
  drop_na(mean_vek) %>% 
  ggplot(aes(x = pohlavi, y = mean_vek)) +
  geom_col(color = "green", fill = "purple",  width = 0.5) + 
  geom_text(aes(label = round(mean_vek, 1)), vjust = -0.5, color = "black") +
  labs (x = "Věk", y = "průměrný počet hodin spánku", title = "Kolik hodin spí muži a ženy")



# BOXPLOT -----------------------------------------------------------------

# boxplot base-----------------------------------------------------------------
boxplot(obvykla_delka_spanku ~ pohlavi, data = d,
        main = "Distribuce délky spánku podle pohlaví",
        xlab = "Pohlaví",
        ylab = "Délka spánku (hodiny)",
        col = c("blue", "red"),
        names = levels(d$pohlavi))



# boxplot ggplot ----------------------------------------------------------
# geom_boxplot()
ggplot(data = d, aes(x = pohlavi, y = vek, fill = pohlavi)) +
  geom_boxplot() +
  scale_fill_manual(values = c("muži" = "lightblue", "ženy" = "orange")) +
  labs(x = "Pohlaví", y = "Věk", title = "Průměrný věk mužů a žen")



# kombinace geomů scatterplot a boxplot  --------------------------------------------------------
ggplot(data = d, aes(x=vek, y = obvykla_delka_spanku, color = pohlavi)) +
  geom_point(alpha =0.7) +
  geom_boxplot() +
  theme_bw()

# body i za boxplotem, jiné barvy, jemnější osa y 
ggplot(data = d, aes(x = pohlavi, y = obvykla_delka_spanku, color = pohlavi)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.2)) +
  geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75), outlier.shape = NA) +
  labs(x = "Pohlaví", y = "Obvyklá délka spánku v hodinách", title = "Délka spánku podle věku a pohlaví",
       color = "Pohlaví") + # color upravuje název legendy 
  theme_classic() +
  # theme(legend.position = "none") +
  scale_color_manual(values = c("muži" = "blue", "ženy" = "red")) +
  scale_y_continuous(breaks = seq(0, max(na.omit(d$obvykla_delka_spanku)), by = 0.5))


# TODO Úkol 
# Uděljte boxplot pro věkové rozložení, vyzkoušejte si změnit titulek, velikost titulku, změnu theme
# zkuste udlěat 3 boxploty v 1 grafu vedle sebe, podle vzdělání 1 pro lidi se ZŠ, SŠ a VŠ vzděláním


# sloupcový horizontální pro baterii otázek -------------------------------
# příklad grafu, který jsem použila, se smyšlenými hodnotami 
# kód by se jistě dal zefektivnit, více používat vlastní funkce atd.

d_up <- readRDS("data/input/cvicna_data_uprchlici_prehazena.rds") 

colnames(d_up)


# výběr soupců pro graf
co_chybi <- d_up %>%
  select(starts_with("cochybi"))

celkem_respondentu <- as.numeric(nrow(d_up))


data_long <- co_chybi %>%
  pivot_longer(cols = starts_with("cochybi"), names_to = "otazka", values_to = "odpoved") %>%
  filter(!is.na(odpoved)) %>%  # Odstranění řádků s chybějícími odpověďmi
  group_by(otazka) %>%
  reframe(pocet_odpovedi = if_else(str_detect(otazka, "other"), sum(odpoved != ""), sum(odpoved == "Ano"))) %>%
  ungroup() %>%
  mutate(celkem = celkem_respondentu,  # Nastavení stejné hodnoty 'celkem' pro všechny řádky
         procento = round(pocet_odpovedi / celkem * 100, digits = 1),
         procento = paste0(procento, " %")) %>%
  distinct()  


colnames(co_chybi)

data_long <- data_long %>%
  mutate(otazka = recode(otazka,
                         "cochybi_1_osvojeni_pokroc" = "osvojení pokročilé úrovně češtiny",
                         "cochybi_2_zajisteni_stabi" = "zajištění stabilního bydlení",
                         "cochybi_3_financni_stabil" = "finanční stabilita či osamostatnění",
                         "cochybi_4_zlepseni_postav" = "zlepšení postavení na trhu práce",
                         "cochybi_5_zajisteni_hlida" = "hlídání dětí během práce",
                         "cochybi_6_lepsi_zacleneni" = "lepší začlenění do české společnosti",
                         "cochybi_7_situace_mladist" = "situace mladistvých 15+ (studium/práce)",
                         "cochybi_8_pomoc_osobam_se" = "pomoc OZP",
                         "cochybi_9_pomoc_osobam_ve" = "pomoc osobám v pracovní neschopnosti",
                         "cochybi_10_pomoc_seniorum" = "pomoc seniorům",
                         "cochybi_11_zadna_z_uvedeny" ="žádná z uvedených",
                         "cochybi_12_na_tuto_otazku" = "Už jsem odpovídal/a."))

# Použití fct_reorder() pro řazení otázek sestupně podle počtu odpovědí Ano
data_long$otazka <- fct_reorder(data_long$otazka, data_long$pocet_odpovedi, .desc = TRUE)

poznamka1 <- paste0("Respondenti souhrnně za projekty i soc. služby, N=", celkem_respondentu, ", možnost více odpovědí")

# Vytvoření grafu s ggplot2 s procentuálními hodnotami a změněnou barvou
ggplot(data_long, aes(x = fct_reorder(otazka, pocet_odpovedi), y = pocet_odpovedi)) +
  geom_bar(stat = "identity", aes(fill = otazka)) +  # Barva podle otazka
  scale_fill_manual(values = c(
    "Už jsem odpovídal/a." = "gray",
    "žádná z uvedených" = "gray",
    "osvojení pokročilé úrovně češtiny" =  "#ddc028",
    "zajištění stabilního bydlení" =  "#ddc028",
    "finanční stabilita či osamostatnění" =  "#ddc028",
    "zlepšení postavení na trhu práce" =  "#ddc028",
    "hlídání dětí během práce" =  "#ddc028",
    "lepší začlenění do české společnosti" =  "#ddc028",
    "situace mladistvých 15+ (studium/práce)" =  "#ddc028",
    "pomoc OZP" =  "#ddc028",
    "pomoc osobám v pracovní neschopnosti" =  "#ddc028",
    "pomoc seniorům" =  "#ddc028"
  ), guide = "none") +
  coord_flip() +  # Otočení grafu
  geom_text(aes(label = paste0(procento, " (", pocet_odpovedi, ")")), hjust = -0.1) +
  scale_x_discrete(limits = rev(levels(data_long$otazka))) +  # Rozšíření limitu osy x
  scale_y_continuous(limits = c(0, 800), expand = c(0, 0)) +  # Nastavení limitu osy y
  labs(y = "Podíl a počet respondentů, kteří vnímají dané potřeby jako nedostatečně pokryté", 
       # title = "Jaké potřeby uprchlíků v ČR nejsou dle respondentů dostatečně pokryty",
       x = NULL, 
       caption = poznamka1) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 10, t=10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y =  element_text(size = 10, color = "black"),
        axis.title.x = element_text(size = 9.5, hjust = 1, color = "#505050"),
        axis.text.x = element_blank(),
        plot.caption = element_text(color = "#505050")) 

ggsave("grafy/nenaplnene_potreby_uprchliku_smyslena_data.png", plot = last_plot(), bg= "white", height = 7, width = 15.98, unit = "cm", dpi = 300)

