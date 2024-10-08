
# načtení balíčků
library(tidyverse)
library(RColorBrewer) # barevné palety
library(janitor) # čištění
library(openxlsx) # načítání souborů
library(webshot2)

# představení různých balíčků pro tvorbu a formátování tabulek
# https://r-graph-gallery.com/table.html
# https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r
# https://themockup.blog/posts/2022-06-13-gtextras-cran/

# Načtení balíčků pro tabulky
library(gt) # https://www.youtube.com/watch?v=gh_e6tmjhLA 
#https://www.youtube.com/watch?v=jptX745mp7M 
#https://www.youtube.com/watch?v=22YpMXufSvw&list=PLBnFxG6owe1EzCt-QHgLxKzgRY3GvOHuw&index=9
# https://jthomasmock.github.io/gtExtras/reference/
# https://gt.albert-rapp.de/styling

library(gtExtras)
# https://jthomasmock.github.io/gtExtras/reference/ 
# https://themockup.blog/posts/2022-06-13-gtextras-cran/

library(rpivotTable) #https://www.youtube.com/watch?v=98UvbWW6fLo
library(htmlwidgets)
library(knitr)

# statistické -------------------------------------------------------------
library(gtsummary) #tbl_summary() #https://www.youtube.com/watch?v=gohF7pp2XCg 
#https://www.youtube.com/watch?v=hyP3Hx_1kTM
library(tableone) # sd, hladina p
# https://rempsyc.remi-theriault.com/articles/table # apa style tables

# korelační matice
library(corrgram)
library(PerformanceAnalytics)
library(ggcorrplot)
library(Hmisc)
library(corrplot)
library(htmlTable)

# interaktivní ------------------------------------------------------------
library(formattable)
library(flextable)
library(reactable) #https://www.youtube.com/watch?v=E3ubwU5Uyqw 
#https://www.youtube.com/watch?v=mrGKySJ-cJc&list=PLBnFxG6owe1EzWjq_rOfdWcWP8QBxcm58&index=34
library(DT)

# (nejen) v RMarkdownu
library(kableExtra)
library(knitr)

set.seed(45) # pro replikovatelnost

# tabulky -----------------------------------------------------------------

hodnota <- c(1, 2, 3, 4, 5, 6, 7)

# Příklad dat
data1 <- data.frame(
  kategorie = c("A", "A", "B", "B", "C", "C", "C"),
  hodnota = as.character(as.roman(hodnota))
)

# table
tab_table <- table(data1$kategorie, data1$hodnota)
print(tab_table)

# group_by + summarize
tab_souhrnna <- data1 %>%
  group_by(kategorie) %>%
  summarise(count = n()) %>% 
  print()

# tabyl z balíčku janitor
tabyl(data1$kategorie)

# cvičná data
df1 <- data.frame(
  Name = c("Alice", "Bob", "Charlie", "Jane"),
  Score = c(90, 85, 95, 75)
)


# gt
gt_table1 <- df1 %>%
  gt() %>%
  tab_header(
    title = "Student Scores",  # Titulek tabulky
    subtitle = "A summary of student performance"
  ) %>%
  tab_source_note(
    source_note = "Note: Scores above 90 are excellent."
  ) %>%
  data_color( # Aplikuje barevné škálování na základě hodnot ve sloupci Score.
    columns = Score,
    fn = scales::col_numeric(  
      palette = c("red", "yellow", "green"), # barvy pro krajní kategorie
      domain = c(75, 100) # rozsah hodnot, pro který platí formátování
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_body(
      columns = Score,
      rows = Score >= 90  # Zvýraznění hodnot nad 90
    )
  )

# Zobrazení tabulky
gt_table1

my_palette <- brewer.pal(4, "Set1")  # 4 barvy z palety "Set1"
display.brewer.pal(4, "Set1")

# přesně zadané barvy, optimálně pro kategorické proměnné
gt_table2 <- df1 %>%
  gt() %>%
  tab_header(
    title = "Student Scores",  # Titulek tabulky
    subtitle = "A summary of student performance"
  ) %>%
  tab_source_note(
    source_note = "Note: Scores above 90 are excellent."
  ) %>%
  data_color(
    columns = Score,
    fn = scales::col_factor(
      palette = my_palette,  # Použijeme přesné barvy z palety
      levels = c(75, 85, 90, 95)  # Nastavíme konkrétní hodnoty (faktory), které odpovídají barvám
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_body(
      columns = Score,
      rows = Score >= 90  # Zvýraznění hodnot nad 90
    )
  ) %>% 
  # Nastavení šedé barvy pro ostatní hodnoty ve sloupci Score
  tab_style(
    style = cell_text(color = "grey15"),  # Bílé písmo pro ostatní hodnoty
    locations = cells_body(
      columns = Score,
      rows = Score < 90  # Aplikace na hodnoty menší než 90
    )
  ) %>% 
  gt_theme_espn()  # Použití konkrétního tématu z gtextras

gt_table2

# gt_theme_espn()
# Tento theme je inspirován vizuálním stylem používaným na webu ESPN, zejména pro sportovní tabulky.
# Vzhled: Čisté linie, černé záhlaví a šedé odstíny pro tělo tabulky
# mnoho dalších, např:
# gt_theme_excel()
# gt_theme_guardian()
# gt_theme_dark()

# stejná barva podbarvení a ohraničení buněk 
gt_table3 <- df1 %>%
  gt() %>%
  # Titulek tabulky bez čar
  tab_header(
    title = "Student Scores"  # Titulek tabulky
  ) %>%
  # Odstranění čar pod titulkem a přidání mezery
  tab_options(
    table.border.top.width = 0,       # Odstraní horní čáru tabulky
    heading.border.bottom.width = 0,  # Odstraní čáru pod titulkem
    heading.padding = px(10)          # Přidá 10 pixelů mezery pod titulek
  ) %>%
  # Zbarvení sloupce Score
  data_color(
    columns = Score,
    fn = function(x) "lightblue"  # Všechny buňky ve sloupci Score budou mít barvu "lightblue"
  ) %>%
  # Ohraničení všech buněk včetně hlavičky
  tab_style(
    style = cell_borders(
      sides = "all",          # Ohraničení na všech stranách
      color = "black",        # Barva ohraničení
      weight = px(2),         # Tloušťka čáry (2 pixely)
      style = "solid"         # Typ čáry (plná čára)
    ),
    locations = list(
      cells_body(),           # Ohraničení všech buněk v těle tabulky
      cells_column_labels()   # Ohraničení hlavičky (názvy proměnných)
    )
  ) %>%
  # Stylování hlavičky (názvy proměnných)
  tab_style(
    style = list(
      cell_text(weight = "bold"),        # Tučné písmo
      cell_fill(color = "#f2c300")       # Tmavě žluté podbarvení (hex kód pro tmavě žlutou)
    ),
    locations = cells_column_labels()   # Pouze na hlavičku tabulky
  )

gt_table3

# Vytvoření tabulky s přizpůsobeným stylem titulku a fontu
gt_table4 <- df1 %>%
  gt() %>%
  tab_header(
    title = "Student Scores",  # Titulek tabulky
    subtitle = "A summary of student performance"
  ) %>%
  tab_style(
    style = cell_text(font = "Arial", size = px(24), weight = "bold"),  # Změna fontu a velikosti titulku
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_text(font = "Arial", size = px(18), weight = "normal"),  # Změna fontu a velikosti podtitulku
    locations = cells_title(groups = "subtitle")
  )

gt_table4

# Nastavení cesty k Chrome pro uložení jako png - nastav si svou cestu 
Sys.setenv(CHROMOTE_CHROME = "C:/Users/katerina.safarova/AppData/Local/Google/Chrome/Application/chrome.exe")

# Export do PNG
gtsave(gt_table1, "tabulky/gt_table1.png", vwidth = 1772, vheight = 1181, expand = 10)


# flextable ---------------------------------------------------------------

# flextable 
flextable(df1) %>%
  theme_box() %>%
  autofit()

# Vytvoření flextable s podmíněným formátováním
ft <- flextable(df1) %>%
  set_caption("Student Scores Summary") %>%  # Přidání titulku
  bg(i = ~ Score > 90, j = "Score", bg = "green") %>%  # Zelená pro hodnoty nad 90
  bg(i = ~ Score >= 85 & Score <= 90, j = "Score", bg = "yellow") %>%  # Žlutá pro hodnoty mezi 85 a 90
  bg(i = ~ Score < 85, j = "Score", bg = "red") %>%  # Červená pro hodnoty pod 85 (není zde relevantní, ale pro ukázku)
  bold(i = ~ Score >= 90)  # Zvýraznění hodnot nad 90 tučným písmem

# Zobrazení flextable
ft
# Export do Wordu nebo PowerPointu
save_as_docx(ft, path = "tabulky/table_flextable.docx")

# Vysvětlení:
#   bg(i = ..., j = ..., bg = ...): Tento formátování aplikuje podmíněné zbarvení na konkrétní buňky. i odkazuje na řádky, které mají být formátovány, a j na sloupec (v tomto případě "Score"). Používáme různé podmínky pro různé rozsahy hodnot.
# bold(i = ...): Aplikuje zvýraznění tučným písmem na hodnoty ve sloupci Score, které jsou větší nebo rovné 90.
# Tímto způsobem můžeme aplikovat podmíněné formátování na různé hodnoty a zvýrazňovat konkrétní buňky v rámci tabulky ve flextable.


# další balíčky -----------------------------------------------------------

# formattable
formattable(df1, list(
  Score = color_tile("lightgreen", "lightblue") # hraniční barvy
))


# Použití formattable pro podmíněné formátování textové barvy
formattable(df1, list(
  Score = formatter(
    "span",
    style = x ~ ifelse(x >= 90, "background-color:lightgreen;",
                       ifelse(x >= 85, "background-color:orange;", 
                              "background-color:lightcoral;"))
  )
))


# DT
datatable(df1) %>%
  formatStyle(
    'Score',
    backgroundColor = styleInterval(c(88), c('lightcoral', 'lightgreen'))
  )

# reactable
reactable(df1, columns = list(
  Score = colDef(
    style = function(value) {
      color <- if (value > 90) "green" else if (value > 80) "orange" else "red"
      list(background = color)
    }
  )
))


# ÚKOL  -------------------------------------------------------------------
# TODO vyber si jeden z balíčků a naformátuj si svou tabulku dle svých preferencí 


