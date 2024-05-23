
library(readr)
library(dplyr)
library(openxlsx)
library(sf)
library(tmap)
library(ggplot2)
library(shinyjs)
library(janitor)
library(extrafont)
library(reschola)


# barvy -------------------------------------------------------------------

Blues5 <- c("#c3cbdb", "#9ba9c4", "#637599", "#3b5486", "#263268")

mf5 <- c("#edf8fb", "#b3cde3", "#8c96c6", "#8856a7", "#810f7c")

zm5 <- c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")

zm5_0 <- c("#FFFFFF", "#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")


# tmaptools::palette_explorer()



# úprava reálných dat na cvičná -------------------------------------------

# df_kraj$pocet_respondentu <- sample(0:150, nrow(df_kraj), replace = TRUE)
# df_orp$pocet_respondentu <- sample(0:200, nrow(df_orp), replace = TRUE)



# načtení dat -------------------------------------------------------------

df_kraj <- get_processed_data("kraje_pocty_projektu_cvicna_data.rds")
df_orp <- get_processed_data("orp_pocty_projektu_cvicna_data.rds")



# mapy --------------------------------------------------------------------


# načtení polygonů a sjednocení názvů proměnné  
kraj <- RCzechia::kraje()
orp <- RCzechia::orp_polygony()
# mc <- RCzechia::casti()

kraj <- clean_names(kraj)
orp <- clean_names(orp)
# mc <- clean_names(mc)


kraj <- kraj %>% 
  rename("kraj" ="naz_cznuts3")

orp <- orp %>% 
  rename("orp" ="naz_orp", 
         "kraj" = "naz_cznuts3")


kraj_mapy <-  kraj %>% 
  left_join(df_kraj, by ="kraj") %>% 
  filter(probiha_je_vas_projekt == "Ano" & kraj != "Celá ČR")

orp_bezi <-  df_orp %>% 
  filter(probiha_je_vas_projekt == "Ano")

orp_mapy <-  orp %>% 
  left_join(orp_bezi, by ="orp") %>% 
  mutate(pocet_respondentu = if_else(is.na(pocet_respondentu), 0, pocet_respondentu)) 



# počty -------------------------------------------------------------------
# kraje běžící 
pocty_kraj <- tm_shape(kraj_mapy) +
  tm_polygons("pocet_respondentu", palette = zm5, style="pretty", title = "Celkem projektů \na soc. služeb v krajích", as.count=TRUE, legend.show=FALSE, border.col = "black") +
  tm_text("pocet_respondentu", size = 0.6, xmod = 0, ymod = -0.25) + 
  tm_credits(text = "Cvičná data pro účely workshopu.", size = 0.9, position=c(0.0, 0.05)) +  
  tm_layout(frame = FALSE,
            legend.outside = FALSE, legend.format = list(text.separator = "až"),
            legend.title.size = 1.1, legend.text.size = 0.8, legend.title.fontface = "bold",
            legend.position = c(0.85, 0.65), 
            inner.margins = c(0.12,0.0,0.05,0.0),
            outer.margins = c(0.0,-0.2,0.0,-0.2),
            main.title = "Počty běžících projektů a sociálních služeb v krajích pro osoby s dočasnou ochranou",
            main.title.size = 1,
            main.title.color = "#0868ac",
            main.title.fontface = "bold",  
            fontfamily = "Arial")  


pocty_kraj

tmap_save(pocty_kraj, "pocty_kraj.png", height = 9,  width = 15.98, units = "cm")


# instalace fontů 
# install.packages("extrafont")
# library(extrafont)
# font_import()  # Toto může chvíli trvat
# loadfonts(device = "win")  # Načte fonty pro Windows



# počty ORP 
kraj_shape <- tm_shape(kraj_mapy) + 
  tm_borders(lwd = 2, col = "black")  

pocty_orp <- tm_shape(orp_mapy) + 
  tm_polygons("pocet_respondentu", palette = zm5_0, style="fixed", breaks=c(0, 30, 80, 120, 180, 250), title = "Celkem projektů \na soc. služeb v ORP", as.count=TRUE, legend.show=TRUE, border.col = "black") +
  # tm_text("pocet_respondentu", size = 0.6, xmod = 0, ymod = -0.25) + 
  tm_credits(text = "Cvičná data pro účely workshopu.", size = 0.9, position=c(0.0, 0.05)) +  
  tm_layout(frame = FALSE,
            legend.outside = FALSE, legend.format = list(text.separator = "až"),
            legend.title.size = 1.0, legend.text.size = 0.8, legend.title.fontface = "bold",
            legend.position = c(0.85, 0.65), 
            inner.margins = c(0.12,0.0,0.05,0.0),
            outer.margins = c(0.0,-0.15,0.05,0.0)) + 
  kraj_shape  # Přidání vrstvy s hranicemi krajů

pocty_orp

tmap_save(pocty_orp, "orp_pocty.png", height = 9, width = 15.98, units = "cm")

