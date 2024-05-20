

library(dplyr)
library(openxlsx) # práce s xlsx soubory, read.xlsx ()
# library(reschola)
library(tidyr) # transformace dat do tidy formátu https://github.com/rstudio/cheatsheets/blob/main/tidyr.pdf , drop_na()
library(janitor) # čištění dat, clean_names()
# library(stringr) # práce s textovýmni proměnnými
library(lubridate) # úprava datumů
library(RCzechia) # využívá se hlavně pro tvrobu map, obsahuje szenam obcí, orp, krajů 
library(skimr) # souhrnné statistiky  
library(stats)
library(ggplot2)




# načtení dat  -------------------------------------------------------------

d <- readRDS("data/processed/dataset_clean.rds")

# základní grafy v base ------------------------------------------------------------

# histogram ---------------------------------------------------------------

hist(d$obvykla_delka_spanku)

hist(d$obvykla_delka_spanku, col = "purple", 
     breaks = seq(4, 10, by = 0.5), 
     xlim = c(4, 10), 
     main = "Obyvklá délka spánku v hodinách", 
     xlab = "počet hodin", 
     ylab = "počet respondentů")


# histogram s více kategoriemi
ggplot(d %>% drop_na(obvykla_delka_spanku), aes(x = obvykla_delka_spanku, fill = vzdelani_3kat)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.5) +
  labs(title = "Obyvklá délka spánku v hodinách",
       x = "počet hodin",
       y = "počet respondentů",
       fill = "Vzdělání") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "blue", "green")) # Nastavení barev pro jednotlivé kategorie

ggplot(d %>% drop_na(obvykla_delka_spanku), aes(x = obvykla_delka_spanku, fill = pohlavi)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.5) +
  labs(title = "Obyvklá délka spánku v hodinách",
       x = "počet hodin",
       y = "počet respondentů",
       fill = "Vzdělání") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red")) # Nastavení barev pro jednotlivé kategorie

x <- d$vek
y <- d$obvykla_delka_spanku


# scatterplot ---------------------------------------------------------------
plot(x, y, main = "Vztah mezi věkem a počtem hodin spánku",
     xlab = "věk", ylab = "obvyklý počet hodin spánku",
     pch = 19, frame = FALSE)

# Definujte barvy pro každé pohlaví
gender_colors <- c("muži" = "blue", "ženy" = "red") # Předpokládáme, že 'muž' bude modrý a 'žena' bude červená

# Použijte barvy podle pohlaví
plot(x, y, main = "Vztah mezi věkem a počtem hodin spánku",
     xlab = "věk", ylab = "obvyklý počet hodin spánku",
     pch = 19, frame = FALSE, col = gender_colors[d$pohlavi])
abline(lm(y ~ x, data = d), col = "darkgreen", lwd = 2)



# barplot -----------------------------------------------------------------
# Vypočteme průměrnou délku spánku pro každé pohlaví
average_sleep <- tapply(d$obvykla_delka_spanku, d$pohlavi, mean, na.rm=TRUE)

# Vytvoříme barplot s průměrnou délkou spánku na ose y
barplot(average_sleep,
        main = "Průměrná délka spánku podle pohlaví",
        xlab = "Pohlaví",
        ylab = "Průměrná délka spánku (hodiny)",
        col = c("blue", "red"),
        legend = levels(d$pohlavi))


# boxplot -----------------------------------------------------------------
boxplot(obvykla_delka_spanku ~ pohlavi, data = d,
        main = "Distribuce délky spánku podle pohlaví",
        xlab = "Pohlaví",
        ylab = "Délka spánku (hodiny)",
        col = c("blue", "red"),
        names = levels(d$pohlavi))


# ggplot2  ----------------------------------------------------------------



