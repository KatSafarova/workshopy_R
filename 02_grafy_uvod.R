
# načtení balíčků  --------------------------------------------------------

library(dplyr)
library(openxlsx) # práce s xlsx soubory, read.xlsx ()
library(tidyr) # transformace dat do tidy formátu https://github.com/rstudio/cheatsheets/blob/main/tidyr.pdf , drop_na()
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


# histogram s více kategoriemi, vzdělání
ggplot(d %>% drop_na(obvykla_delka_spanku), aes(x = obvykla_delka_spanku, fill = vzdelani_3kat)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.5) +
  labs(title = "Obyvklá délka spánku v hodinách",
       x = "počet hodin",
       y = "počet respondentů",
       fill = "Vzdělání") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "#00BFFF", "green")) # Nastavení barev pro jednotlivé kategorie

# https://www.klikzone.cz/sekce-html/html-barvy.php

# histogram s více kategoriemi, pohlaví
ggplot(d %>% drop_na(obvykla_delka_spanku), aes(x = obvykla_delka_spanku, fill = pohlavi)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.9) +
  labs(title = "Obyvklá délka spánku v hodinách",
       x = "počet hodin",
       y = "počet respondentů",
       fill = "Vzdělání") +
  theme_bw () +
  scale_fill_manual(values = c("blue", "red")) # Nastavení barev pro jednotlivé kategorie



# scatterplot ---------------------------------------------------------------
x <- d$vek
y <- d$obvykla_delka_spanku

plot(x, y, main = "Vztah mezi věkem a počtem hodin spánku",
     xlab = "věk", ylab = "obvyklý počet hodin spánku",
     pch = 21, frame = FALSE)

# Definujte barvy pro každé pohlaví
gender_colors <- c("muži" = "blue", "ženy" = "red") 

# Použijte barvy podle pohlaví
plot(x, y, main = "Vztah mezi věkem a počtem hodin spánku",
     xlab = "věk", ylab = "obvyklý počet hodin spánku",
     pch = 8, frame = TRUE, col = gender_colors[d$pohlavi])
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
# 2 základní atributy
# aes co se má zobrazit na které škále
ggplot(data = d, aes(x=vek, y = obvykla_delka_spanku))

# geom_point()
ggplot(data = d, aes(x=vek, y = obvykla_delka_spanku)) +
  geom_point()

ggplot(data = d, aes(x=vek, y = obvykla_delka_spanku)) +
  geom_point() 

ggplot(data = d, aes(x=vek, y = obvykla_delka_spanku, color = pohlavi)) +
  geom_point(shape = 17, size = 2, alpha =0.7) +
  theme_bw()

# geom_bar()
# # geom_bar zobrazuje počty, má jen 1 osu 
ggplot(data = d, aes(x = pohlavi)) +
  geom_bar()


# geom_col()
# chceme zobrazit ve slopupcovém grafu průměrný vek pro muže a ženy
# nejdřív si musíme průměrný věk pro obě pohalví spočítat 
agg_data <- d %>%
  group_by(pohlavi) %>%
  summarize(mean_vek = mean(vek, na.rm = TRUE))

ggplot(data = agg_data, aes(x = pohlavi, y = mean_vek)) +
  geom_col(color = "green", fill = "purple") + 
  labs (x = "Věk", y = "průměrný počet hodin spánku", title = "Kolik hodin spí muži a ženy")


# geom_boxplot()
ggplot(data = d, aes(x = pohlavi, y = vek, fill = pohlavi)) +
  geom_boxplot() +
  scale_fill_manual(values = c("muži" = "lightblue", "ženy" = "orange")) +
  labs(x = "Pohlaví", y = "Věk", title = "Průměrný věk mužů a žen")


# kombiance geomů
ggplot(data = d, aes(x=vek, y = obvykla_delka_spanku, color = pohlavi)) +
  geom_point(alpha =0.7) +
  geom_boxplot() +
  theme_bw()

ggplot(data = d, aes(x = pohlavi, y = obvykla_delka_spanku, color = pohlavi)) +
  geom_point(alpha = 0.7, position = position_jitter(width = 0.2)) +
  geom_boxplot(alpha = 0.5, position = position_dodge(width = 0.75), outlier.shape = NA) +
  theme_bw() +
  labs(x = "Pohlaví", y = "Obvyklá délka spánku", title = "Délka spánku podle věku a pohlaví")
