#######
# Ez a szkript a 9. táblázatát replikálja, vagyis ciklusonként és pártonként a nem nulla magyarázóerővel bíró bigrammok számát tartalmazó ábrát
# Egységes regularizáció mindhárom korpuszra (teljes, kulturális, gazdasági) -- a dolgozatban eltérő regularizációs értékek voltak választva (lásd: azokat replikáló szkriptek)
#######

# A munkakönyvtárat, ahonnan a CSV fájlokat beolvassuk és a célkönyvtárat, ahova a PDF-ek mentésre kerülnek, manuálisan kell beállítani.

library(dplyr)
library(tidyr)
library(Matrix)
library(distrom)
library(kableExtra)
library(knitr)
library(tinytex)
library(purrr)

setwd("Kerem/sajat/konyvtarat/allitson/be")

teljes_korpusz_df <- read.csv("teljes_korpusz_df.csv", fileEncoding = "UTF-8")
speaker_metadata <- read.csv("speaker_metadata.csv", fileEncoding = "UTF-8")

teljes_korpusz_df <- merge(teljes_korpusz_df, speaker_metadata[, c("speaker", "period", "id_speaker", "fidesz")], 
                           by = c("speaker", "period"), 
                           all.x = TRUE)


calculate_zeta_summary <- function(korpusz_df, speaker_metadata_df, korpusz_nev = "teljes") {
  library(dplyr)
  library(Matrix)
  library(distrom)
  
  meta_df <- speaker_metadata_df %>%
    filter(id_speaker %in% korpusz_df$id_speaker) %>%
    mutate(
      session = factor(case_when(
        period == "1998-2002" ~ 3,
        period == "2002-2006" ~ 4,
        period == "2006-2010" ~ 5,
        period == "2010-2014" ~ 6,
        period == "2014-2018" ~ 7
      )),
      majority = factor(case_when(
        (period == "1998-2002" & fidesz == 1) ~ 1,
        (period == "2002-2006" & fidesz == 0) ~ 1,
        (period == "2006-2010" & fidesz == 0) ~ 1,
        (period == "2010-2014" & fidesz == 1) ~ 1,
        (period == "2014-2018" & fidesz == 1) ~ 1,
        TRUE ~ 0
      )),
      chamber = factor(1),
      gender = factor(gender)
    )
  
  speakers <- unique(korpusz_df$id_speaker)
  bigrams  <- unique(korpusz_df$bigram)
  
  C <- sparseMatrix(
    i = match(korpusz_df$id_speaker, speakers),
    j = match(korpusz_df$bigram, bigrams),
    x = korpusz_df$N,
    dims = c(length(speakers), length(bigrams)),
    dimnames = list(speakers, bigrams)
  )
  
  X <- sparse.model.matrix(~ 0 + session + majority + gender, data = meta_df)
  qx <- qr(as.matrix(X))
  X <- X[, qx$pivot[1:qx$rank]]
  rownames(X) <- meta_df$id_speaker
  X <- X[rownames(C), ]
  
  F <- sparse.model.matrix(~ 0 + session, data = meta_df)
  F <- F * meta_df$fidesz
  colnames(F) <- paste0(colnames(F), "_F_", korpusz_nev)
  rownames(F) <- meta_df$id_speaker
  F <- F[rownames(C), ]
  
  mu <- log(rowSums(C))
  
  cl <- parallel::makeCluster(2)
  fit <- dmr(cl = cl,
             covars = cbind(X, F),
             counts = C,
             mu = mu,
             free = 1:ncol(X),
             fixedcost = 0.01,
             lambda.start = Inf,
             lambda.min.ratio = 0.01,
             nlambda = 150,
             standardize = FALSE)
  stopCluster(cl)
  
  coefs <- coef(fit, k = log(nrow(X)), corrected = FALSE)
  coefs_F <- coefs[colnames(F), ]
  
  zeta_long <- as.data.frame(as.table(as.matrix(coefs_F))) %>%
    rename(session_id = Var1, bigram = Var2, zeta_value = Freq) %>%
    mutate(
      party = case_when(
        zeta_value > 0 ~ "Fidesz",
        zeta_value < 0 ~ "MSZP",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(party)) %>%
    left_join(
      data.frame(
        session_id = paste0("session", 3:7, "_F_", korpusz_nev),
        period_label = c("1998-2002", "2002-2006", "2006-2010", "2010-2014", "2014-2018")
      ),
      by = "session_id"
    )
  
  zeta_summary <- zeta_long %>%
    group_by(period_label, party) %>%
    summarise(non_zero_count = n(), .groups = "drop")
  
  return(zeta_summary)
}


zeta_summary_teljes_df     <- calculate_zeta_summary(teljes_korpusz_df, speaker_metadata, "teljes")
zeta_summary_gazdasagi_df  <- calculate_zeta_summary(filter(teljes_korpusz_df, predicted %in% c("both", "econ")), speaker_metadata, "gazdasagi")
zeta_summary_kulturalis_df <- calculate_zeta_summary(filter(teljes_korpusz_df, predicted %in% c("both", "cult")), speaker_metadata, "kulturalis")


### PDF kimentése
setwd("Kerem/sajat/konyvtarat/allitson/be")

# Átalakítás - külön df-ek összevonása
join_all_zeta_tables <- function(df, korpusznev) {
  df %>%
    pivot_wider(names_from = party, values_from = non_zero_count) %>%
    rename(
      !!paste0("Fidesz_", korpusznev) := Fidesz,
      !!paste0("MSZP_", korpusznev) := MSZP
    )
}

df_teljes     <- join_all_zeta_tables(zeta_summary_teljes_df, "teljes")
df_gazdasagi  <- join_all_zeta_tables(zeta_summary_gazdasagi_df, "gazdasagi")
df_kulturalis <- join_all_zeta_tables(zeta_summary_kulturalis_df, "kulturalis")

zeta_summary_combined <- reduce(
  list(df_teljes, df_gazdasagi, df_kulturalis),
  left_join,
  by = "period_label"
)

# Tisztább oszlopnevek
colnames(zeta_summary_combined) <- c(
  "Ciklus", 
  "Fidesz", "MSZP", 
  "Fidesz", "MSZP", 
  "Fidesz", "MSZP"
)

# LaTeX táblázat létrehozása
tabla_latex <- kable(zeta_summary_combined, format = "latex", booktabs = TRUE, align = "c") %>%
  add_header_above(c(" " = 1, "Teljes korpusz" = 2, "Gazdasági korpusz" = 2, "Kulturális korpusz" = 2)) %>%
  kable_styling(latex_options = c("striped", "hold_position")) %>%
  add_header_above(c("Nem nulla magyarázóerővel bíró bigramok száma" = 7), bold = TRUE)

# LaTeX dokumentum (elforgatva)
full_tex <- c(
  "\\documentclass{article}",
  "\\usepackage[utf8]{inputenc}",
  "\\usepackage[a4paper, left=2.5cm, right=2.5cm, top=2.5cm, bottom=2.5cm]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{longtable}",
  "\\usepackage{caption}",
  "\\usepackage{float}",
  "\\usepackage{array}",
  "\\usepackage{graphicx}",
  "\\usepackage[table]{xcolor}",
  "\\usepackage{pdflscape}",
  "\\begin{document}",
  "\\begin{landscape}",
  "{\\small",
  as.character(tabla_latex),
  "}",
  "\\end{landscape}",
  "\\end{document}"
)

# Mentés és PDF generálás
writeLines(full_tex, "zeta_summary_table.tex")
tinytex::latexmk("zeta_summary_table.tex")
