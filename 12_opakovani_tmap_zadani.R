library(dplyr)
library(sf)
library(tmap)
library(tmaptools)
library(ggplot2)
library(janitor)
library(RColorBrewer)
library(cols4all)
library(RCzechia)
# install.packages("cols4all", dependencies = TRUE)



# barevné palety ----------------------------------------------------------

zm5 <- c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")


# c4a_gui()

# z c4a_gui je možné barvy zkopírovat a uložit si je nebo pracovat s názvem palety
# c("#FF9D9A", "#77AADD", "#F1CE63", "#2CA02C", "#B07AA1", "#9EDAE5", "#CC6677")

# nebo pro barvy z ColorBrewer funkce z tmaptools
modra <- get_brewer_pal("Blues", n = 6)

# načtení dat -------------------------------------------------------------
# cvičná data o počtech respondentů, kteří vyplnili dotazník 
df_orp <- readRDS("data/processed/orp_pocty_projektu_cvicna_data.rds") 
df_kraj <- readRDS("data/processed/kraje_pocty_projektu_cvicna_data.rds")


# načtení polygonů a sjednocení názvů proměnné  
orp <- RCzechia::orp_polygony()
kraj <- RCzechia::kraje()

orp <- clean_names(orp)
kraj <- clean_names(kraj)


kraj <- kraj %>% 
  rename("kraj" ="naz_cznuts3")

orp <- orp %>% 
  rename("orp" ="naz_orp", 
         "kraj" = "naz_cznuts3")

df_orp <- df_orp |> 
  filter(probiha_je_vas_projekt == "Ano") |> 
  filter(!is.na(orp))


df_kraj <- df_kraj |> 
  filter(probiha_je_vas_projekt == "Ano") |> 
  filter(kraj != "Celá ČR") |> 
  filter(!is.na(kraj))

# data obsahují sloupec geometry, který určuje kde se daná orp v mapě vykreslí 
head(orp)


orp <- orp |> 
  left_join(df_orp, by ="orp") |> 
  mutate(pocet_respondentu = if_else(is.na(pocet_respondentu), 0, pocet_respondentu)) 


kraj <- kraj %>% 
  left_join(df_kraj, by ="kraj") |> 
  filter(probiha_je_vas_projekt == "Ano") 

rm(df_orp, df_kraj)



# syntax tmap ------------------------------------------------------------------
# princip vrstvení jako v ggplotu pomocí +
# https://r-tmap.github.io/tmap/index.html 


# výchozí zobrazení ČR 
tm_shape(orp) +
  tm_fill()

# když specifikujeme barvu výplně a hranic polygonů 
tm_shape(orp) +
  tm_fill(fill = "yellow", col = "blue")

# přidání hranic orp 
tm_shape(orp) +
  tm_fill(fill = "yellow", col = "blue") +
  tm_borders(lwd = 5) # tloušťka obrysů hranic 


# můžeme si uložit hranice pro kraj a následně je přidat k orp
kraj_shape <- tm_shape(kraj) + 
  tm_borders(lwd = 2, col = "black")  

# přidání dalších hranic
tm_shape(orp) +
  tm_fill(fill = "yellow", col = "blue") +
  tm_borders(lwd = 1, col = "grey70") + 
  kraj_shape


# titulek
tm_shape(orp) +
  tm_fill(fill = "yellow", col = "blue") +
  tm_borders(lwd = 1, col = "grey70") + 
  tm_borders(lwd = 1, col = "grey70") + 
  kraj_shape +
  tm_title("Mapa ORP v ČR", 
           color = "#0868ac", 
           fontface = "bold", 
           fontfamily = "sans") + 
  tm_layout(
    bg.color = "lightblue", # barva pozadí 
    frame = FALSE) # bez rámečku



# počty -------------------------------------------------------------------
tm_shape(orp) +
  tm_polygons(fill = "pocet_respondentu", 
              fill.scale = tm_scale_intervals())

# TODO změn barvy 
# TODO vyzkoušej si zobrazit různé styly pro intervaly viz  https://tmap.geocompx.org/xx-scales

style1 <- tm_shape(orp) +
  tm_polygons(fill = "pocet_respondentu", 
              fill.scale = tm_scale_intervals(style =))

style1

style2 <- tm_shape(orp) +
  tm_polygons(fill = "pocet_respondentu", 
              fill.scale = tm_scale_intervals(style =),
              fill.legend = tm_legend())

style2

# vyzkoušej si různ= styly pro tm_scale continuous 
style3 <- tm_shape(orp) +
  tm_polygons(fill = "pocet_respondentu", 
              fill.scale = tm_scale_continuous())
style3


# legenda + titulek ----------------------------------------------------------

tm_shape(orp) +
  tm_polygons(fill = "pocet_respondentu", 
              fill.scale = tm_scale_intervals(
                as.count = TRUE,
                style = "pretty",
                values = zm5,  # Nový argument místo palette,
                label.format = list(text.separator = " až ", big.mark = " ", digits = 0)  # **Změna oddělovače a formátování čísel**
              ), 
              fill.legend = tm_legend(
                title = "",
                na.show = FALSE,
                # textNA = "Missing",
                frame = FALSE, 
                # reverse = TRUE,
                orientation = "landscape",
                position = tm_pos_on_top()
              )) +
  tm_title(text = "Počty respondentů v ORP") +
  tm_layout(
    frame = FALSE, 
    inner.margins = c(0, 0, 0.15 ,0), #Vector of four values specifying the bottom, left, top, and right margin
    component.autoscale = FALSE)

# TODO změn pozici legendy jak se ti bude líbit viz obecně různé pozice  
# https://r-tmap.github.io/tmap/articles/adv_positions 


# labely pro hodnoty v mapě ------------------------------------------------------------------
graf1 <- tm_shape(kraj) +
  tm_polygons(fill = "pocet_respondentu", 
              fill.scale = tm_scale_intervals(
                as.count = TRUE,
                style = "pretty",
                values = "carto.purp_or",  
                n = 7,
                label.format = list(text.separator = " až ", big.mark = " ", digits = 0)),
              fill.legend = tm_legend_hide()  # Skrytí legendy
  ) +
  tm_text(text = "pocet_respondentu", 
          size = 0.8, 
          # angle = 20, 
          ymod = -0.25) +
  tm_layout(
    frame = FALSE, 
    component.autoscale = FALSE, 
    outer.margins = c(0.1, 0, 0, 0)) + #Vector of four values specifying the bottom, left, top, and right margin
  tm_credits(
    text = "Data k 23. 3. 2025.",
    size = 0.9,  
    position = c(0.0, 0.02)  
  ) +
  tm_credits(
    text = "Administrativní data MPSV.",
    size = 0.7,  
    position = c(0.0, 0.02), 
    fontface = "italic"
  )

graf1

# TODO vyber si jinou paletu z c4a_gui() a změň ji; přidej titulek, změň pozici tm_credits, změň počet barev na 3

# ukládání dat ----------------------------------------------------------
tmap_save(graf1, "mapy/tmap_cviceni_labely.png", height = 9,  width = 15.98, units = "cm", dpi = 600)


# další zajímavé tipy -----------------------------------------------------
# https://r-tmap.github.io/tmap/articles/basics_components
# https://r-tmap.github.io/tmap/articles/basics_facets 
# https://r-tmap.github.io/tmap/articles/basics_charts
# https://r-tmap.github.io/tmap/articles/versus_ggplot2


