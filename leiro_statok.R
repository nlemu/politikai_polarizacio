##########
## Ebben a dokumentumban található a leíró statisztikás ábrákhoz szükséges replikációs kód
##########

# A munkakönyvtárat, ahonnan a CSV fájlokat beolvassuk és a célkönyvtárat, ahova a PDF-ek mentésre kerülnek, manuálisan kell beállítani.

# A kód replikálja a dolgozat 3., 4. és 5. táblázatát

setwd("Kerem/sajat/konyvtarat/allitson/be")

library(dplyr)
library(knitr)
library(kableExtra)
library(tinytex)

teljes_korpusz_df <- read.csv("teljes_korpusz_df.csv", fileEncoding = "UTF-8")
bigram_lista <- read.csv("bigram_lista.csv", fileEncoding = "UTF-8")
speaker_metadata <- read.csv("speaker_metadata.csv", fileEncoding = "UTF-8")
felszolalasok <- read.csv("felszolalasok.csv", fileEncoding = "UTF-8") # politikai tartalom nélküliek nincsenek már benne (Babel machine alapján)


# unique phrases
length(unique(bigram_lista$bigram))

# spoken total
sum(teljes_korpusz_df$N, na.rm = TRUE)

# unique speakers
length(unique(speaker_metadata$speaker))


### Teljes korpuszhoz

# Ciklusonkénti statisztikák kiszámítása
summary_ciklusonkent <- teljes_korpusz_df %>%
  group_by(period) %>%
  summarise(
    egyedi_fideszes_kepviselok = n_distinct(speaker[party == "Fidesz"]),
    egyedi_mszps_kepviselok    = n_distinct(speaker[party == "MSZP"]),
    egyedi_bigramok            = n_distinct(bigram),
    osszes_fideszes_kifejezes  = sum(N[party == "Fidesz"]),
    osszes_mszps_kifejezes     = sum(N[party == "MSZP"])
  ) %>%
  arrange(period)

# Felszólalások számának csatolása
felszolalasok_szama <- felszolalasok %>%
  group_by(period) %>%
  summarise(osszes_felszolalas = n(), .groups = "drop")

summary_ciklusonkent <- summary_ciklusonkent %>%
  left_join(felszolalasok_szama, by = "period")

# Magyar oszlopnevek beállítása
colnames(summary_ciklusonkent) <- c(
  "Ciklus", 
  "# fidesz képviselők", 
  "# mszp képviselők", 
  "Egyedi bigramok", 
  "Fideszes", 
  "MSZPs", 
  "Összes felszólalás"
)

# LaTeX táblázat létrehozása
tabla_latex <- kable(summary_ciklusonkent, format = "latex", booktabs = TRUE,
                     align = "c") %>%
  add_header_above(c(" " = 4, "Összes kifejezés" = 2, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  add_header_above(c("3. táblázat: Ciklusonkénti leíró statisztikák (teljes korpusz)" = ncol(summary_ciklusonkent)), bold = TRUE)

# LaTeX dokumentum szöveg összeállítása
teljes_tex <- c(
  "\\documentclass{article}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[a4paper, left=1.5cm, right=2.5cm, top=2.5cm, bottom=2.5cm]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{longtable}",
  "\\usepackage{caption}",
  "\\usepackage{float}",
  "\\usepackage{array}",
  "\\usepackage{graphicx}",
  "\\usepackage[table]{xcolor}",
  "\\usepackage{hyperref}",
  "\\captionsetup[table]{labelformat=empty}",
  "\\title{Ciklusonkénti leíró statisztikák}",
  "\\date{}",
  "\\begin{document}",
  "\\maketitle",
  "{\\small",
  as.character(tabla_latex),
  "}",
  "\\end{document}"
)


# Mentés és PDF generálás
setwd("Kerem/sajat/konyvtarat/allitson/be")
writeLines(teljes_tex, "leiro_stat_teljes.tex")
tinytex::latexmk("leiro_stat_teljes.tex")


### Gazdasági korpusz

gazdasag_korpusz <- teljes_korpusz_df %>%
  filter(predicted %in% c("both", "econ"))

# Ciklusonkénti statisztikák kiszámítása
summary_ciklusonkent <- gazdasag_korpusz %>%
  group_by(period) %>%
  summarise(
    egyedi_fideszes_kepviselok = n_distinct(speaker[party == "Fidesz"]),
    egyedi_mszps_kepviselok    = n_distinct(speaker[party == "MSZP"]),
    egyedi_bigramok            = n_distinct(bigram),
    osszes_fideszes_kifejezes  = sum(N[party == "Fidesz"]),
    osszes_mszps_kifejezes     = sum(N[party == "MSZP"])
  ) %>%
  arrange(period)

# Felszólalások számának csatolása
felszolalasok_szama <- felszolalasok %>%
  filter(major_topic_pred %in% c(1, 3, 4, 5, 7, 8, 10, 13, 14, 15, 18)) %>%
  group_by(period) %>%
  summarise(osszes_felszolalas = n(), .groups = "drop")

summary_ciklusonkent <- summary_ciklusonkent %>%
  left_join(felszolalasok_szama, by = "period")

# Magyar oszlopnevek beállítása
colnames(summary_ciklusonkent) <- c(
  "Ciklus", 
  "# fidesz képviselők", 
  "# mszp képviselők", 
  "Egyedi bigramok", 
  "Fideszes", 
  "MSZPs", 
  "Összes felszólalás"
)

# LaTeX táblázat létrehozása
tabla_latex <- kable(summary_ciklusonkent, format = "latex", booktabs = TRUE,
                     align = "c") %>%
  add_header_above(c(" " = 4, "Összes kifejezés" = 2, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  add_header_above(c("5. táblázat: Ciklusonkénti leíró statisztikák (gazdasági korpusz)" = ncol(summary_ciklusonkent)), bold = TRUE)

# LaTeX dokumentum szöveg összeállítása
teljes_tex <- c(
  "\\documentclass{article}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[a4paper, left=1.5cm, right=2.5cm, top=2.5cm, bottom=2.5cm]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{longtable}",
  "\\usepackage{caption}",
  "\\usepackage{float}",
  "\\usepackage{array}",
  "\\usepackage{graphicx}",
  "\\usepackage[table]{xcolor}",
  "\\usepackage{hyperref}",
  "\\captionsetup[table]{labelformat=empty}",
  "\\title{Ciklusonkénti leíró statisztikák}",
  "\\date{}",
  "\\begin{document}",
  "\\maketitle",
  "{\\small",
  as.character(tabla_latex),
  "}",
  "\\end{document}"
)


# Mentés és PDF generálás
writeLines(teljes_tex, "leiro_stat_gazdasagi.tex")
tinytex::latexmk("leiro_stat_gazdasagi.tex")


### Kulturalis korpusz

kulturalis_korpusz <- teljes_korpusz_df %>%
  filter(predicted %in% c("both", "cult"))

# Ciklusonkénti statisztikák kiszámítása
summary_ciklusonkent <- kulturalis_korpusz %>%
  group_by(period) %>%
  summarise(
    egyedi_fideszes_kepviselok = n_distinct(speaker[party == "Fidesz"]),
    egyedi_mszps_kepviselok    = n_distinct(speaker[party == "MSZP"]),
    egyedi_bigramok            = n_distinct(bigram),
    osszes_fideszes_kifejezes  = sum(N[party == "Fidesz"]),
    osszes_mszps_kifejezes     = sum(N[party == "MSZP"])
  ) %>%
  arrange(period)

# Felszólalások számának csatolása --> ezt kell átírni, hogy ne mindenhol ugyanaz legyen ### itt
felszolalasok_szama <- felszolalasok %>%
  filter(major_topic_pred %in% c(2, 6, 7, 12, 13, 14, 16, 19, 23)) %>%
  group_by(period) %>%
  summarise(osszes_felszolalas = n(), .groups = "drop")

summary_ciklusonkent <- summary_ciklusonkent %>%
  left_join(felszolalasok_szama, by = "period")

# Magyar oszlopnevek beállítása
colnames(summary_ciklusonkent) <- c(
  "Ciklus", 
  "# fidesz képviselők", 
  "# mszp képviselők", 
  "Egyedi bigramok", 
  "Fideszes", 
  "MSZPs", 
  "Összes felszólalás"
)

# LaTeX táblázat létrehozása
tabla_latex <- kable(summary_ciklusonkent, format = "latex", booktabs = TRUE,
                     align = "c") %>%
  add_header_above(c(" " = 4, "Összes kifejezés" = 2, " " = 1)) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  add_header_above(c("4. táblázat: Ciklusonkénti leíró statisztikák (kulturalis korpusz)" = ncol(summary_ciklusonkent)), bold = TRUE)

# LaTeX dokumentum szöveg összeállítása
teljes_tex <- c(
  "\\documentclass{article}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[a4paper, left=1.5cm, right=2.5cm, top=2.5cm, bottom=2.5cm]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{longtable}",
  "\\usepackage{caption}",
  "\\usepackage{float}",
  "\\usepackage{array}",
  "\\usepackage{graphicx}",
  "\\usepackage[table]{xcolor}",
  "\\usepackage{hyperref}",
  "\\captionsetup[table]{labelformat=empty}",
  "\\title{Ciklusonkénti leíró statisztikák}",
  "\\date{}",
  "\\begin{document}",
  "\\maketitle",
  "{\\small",
  as.character(tabla_latex),
  "}",
  "\\end{document}"
)


writeLines(teljes_tex, "leiro_stat_kulturalis.tex")
tinytex::latexmk("leiro_stat_kulturalis.tex")
