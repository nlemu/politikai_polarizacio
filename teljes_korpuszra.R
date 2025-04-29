### Az összes, előfeldolgozás után megmaradó felszólaláson a számítások elvégzése ###

# A munkakönyvtárat (ahonnan a CSV fájlokat beolvassuk) és a célkönyvtárat (ahova a PDF-ek mentésre kerülnek) manuálisan kell beállítani.

# A kód replikálja a dolgozat 1. ábráját és 6. táblázatát

library(dplyr)
library(ggplot2)
library(tidyr)
library(Matrix)
library(distrom)
library(kableExtra)
library(knitr)
library(tinytex)

setwd("Kerem/sajat/konyvtarat/allitson/be")

teljes_korpusz_df <- read.csv("teljes_korpusz_df.csv", fileEncoding = "UTF-8")
speaker_metadata <- read.csv("speaker_metadata.csv", fileEncoding = "UTF-8")


# "id_speaker" és "fidesz" változók hozzáadása
teljes_korpusz_df <- merge(teljes_korpusz_df, speaker_metadata[, c("speaker", "period", "id_speaker", "fidesz")], 
                  by = c("speaker", "period"), 
                  all.x = TRUE)

bigram_party_period <- teljes_korpusz_df %>%
  group_by(period, party, bigram) %>%
  summarise(N = sum(N), .groups = "drop")

bigram_comparison <- bigram_party_period %>%
  pivot_wider(
    names_from = party,
    values_from = N,
    values_fill = list(N = 0)
  ) %>%
  mutate(fidesz_vs_mszp = Fidesz - MSZP)


# speaker_metadata előkészítése
speaker_metadata <- speaker_metadata %>%
  mutate(
    # session változó létrehozása
    session = case_when(
      period == "1998-2002" ~ 3,
      period == "2002-2006" ~ 4,
      period == "2006-2010" ~ 5,
      period == "2010-2014" ~ 6,
      period == "2014-2018" ~ 7,
      TRUE ~ NA_real_
    ),
    
    # region változó (minden érték HUN)
    region = "HUN",
    state = "HUN",
    # chamber változó (minden érték 1)
    chamber = 1,
    
    # majority változó kiszámítása
    majority = case_when(
      (period == "1998-2002" & fidesz == 1) ~ 1,
      (period == "2002-2006" & fidesz == 0) ~ 1,
      (period == "2006-2010" & fidesz == 0) ~ 1,
      (period == "2010-2014" & fidesz == 1) ~ 1,
      (period == "2014-2018" & fidesz == 1) ~ 1,
      TRUE ~ 0  # Minden más esetben 0
    )
  )

speaker_metadata <- speaker_metadata %>%
  mutate(
    session = factor(session),
    state = factor(state),
    gender = factor(gender),
    chamber = factor(chamber),
    majority = factor(majority)
  )


### C(ijt) mátrix létrehozása

# sor: id_speaker
# oszlop: bigramm

# Egyedi id_speaker és bigram értékek
speakers <- unique(teljes_korpusz_df$id_speaker)
bigrams <- unique(teljes_korpusz_df$bigram)

# Indexek létrehozása a sparseMatrix számára
row_index <- match(teljes_korpusz_df$id_speaker, speakers)
col_index <- match(teljes_korpusz_df$bigram, bigrams)

# Ritka mátrix létrehozása
C_tdk <- sparseMatrix(i = row_index, 
                      j = col_index, 
                      x = teljes_korpusz_df$N, 
                      dims = c(length(speakers), length(bigrams)), 
                      dimnames = list(speakers, bigrams))

# regularizált számítások
X_tdk <- sparse.model.matrix( ~ 0 + session + majority + gender,
                              data = speaker_metadata)

qx_tdk          <- qr(as.matrix(X_tdk))

X_tdk           <- X_tdk[, qx_tdk$pivot[1:qx_tdk$rank]]

rownames(X_tdk) <- speaker_metadata$id_speaker

X_tdk           <- X_tdk[rownames(C_tdk), ]

F_tdk           <- sparse.model.matrix(~ 0 + session, data = speaker_metadata)

F_tdk           <- F_tdk * speaker_metadata$fidesz

colnames(F_tdk) <- paste(colnames(F_tdk), 'F_tdk', sep = '_')

rownames(F_tdk) <- speaker_metadata$id_speaker

F_tdk           <- F_tdk[rownames(C_tdk), ]

mu_tdk <- log(rowSums(C_tdk))

cl_tdk <- makeCluster(2, type = ifelse(.Platform$OS.type == 'unix', 'FORK', 'PSOCK'))

# paraméterek
fit_tdk <- dmr(
  cl = cl_tdk,
  covars = cbind(X_tdk, F_tdk),
  counts = C_tdk,
  mu = mu_tdk,
  free = 1:ncol(X_tdk),
  fixedcost = 0.015,
  lambda.start = Inf,
  lambda.min.ratio = 0.015,
  nlambda = 150,
  standardize = FALSE
)


stopCluster(cl_tdk)

# Koefficiensek
coefs_tdk   <- coef(fit_tdk, k = log(nrow(X_tdk)), corrected = F)
coefs_X_tdk <- coefs_tdk[colnames(X_tdk), ]
coefs_F_tdk <- coefs_tdk[colnames(F_tdk), ]

session_tdk        <- speaker_metadata$session
names(session_tdk) <- speaker_metadata$id_speaker
session_tdk        <- session_tdk[rownames(C_tdk)]
session_tdk        <- as.matrix(session_tdk)

I_tdk           <- sparse.model.matrix(~ 0 + session, data = speaker_metadata)
rownames(I_tdk) <- speaker_metadata$id_speaker
I_tdk           <- I_tdk[rownames(C_tdk), ]
phi_tdk         <- I_tdk %*% coefs_F_tdk

utility_mszp_tdk        <- cbind(1, X_tdk) %*% rbind(coefs_tdk['intercept', ], coefs_X_tdk)
utility_fidesz_tdk        <- utility_mszp_tdk + phi_tdk

party_ratio_tdk        <- rowSums(exp(utility_mszp_tdk)) / rowSums(exp(utility_fidesz_tdk))
likelihood_ratio_tdk   <- party_ratio_tdk * exp(phi_tdk)
rho_tdk                <- likelihood_ratio_tdk / (1 + likelihood_ratio_tdk)
q_F_tdk                <- exp(utility_fidesz_tdk) / rowSums(exp(utility_fidesz_tdk))
q_M_tdk                <- exp(utility_mszp_tdk) / rowSums(exp(utility_mszp_tdk))
pi_tdk                 <- rowSums(0.5 * q_F_tdk * rho_tdk + 0.5 * q_M_tdk * (1 - rho_tdk))

stopifnot(all.equal(rho_tdk, q_F_tdk / (q_F_tdk + q_M_tdk)))

# Átlagos pártos polarizáció kiszámítása
average_pi_tdk       <- tapply(pi_tdk, list(session_tdk), mean) 
average_pi_tdk       <- average_pi_tdk[order(as.integer(names(average_pi_tdk)))]
average_pi_tdk       <- data.frame(session = as.integer(names(average_pi_tdk)), average_pi_tdk = average_pi_tdk)


### Validáció ###

# módszertani validáció -- párt címkék random újraosztása (eredeti valószínűségek szerint)

session_probs <- speaker_metadata %>%
  group_by(session) %>%
  summarise(prob_fidesz = mean(fidesz))

speaker_probs <- speaker_metadata %>%
  left_join(session_probs, by = "session") %>%
  group_by(id_speaker) %>%
  summarise(mean_prob_fidesz = mean(prob_fidesz))

table(speaker_probs$mean_prob_fidesz)

set.seed(42)

speaker_metadata_reassign <- speaker_metadata %>%
  left_join(speaker_probs, by = "id_speaker") %>%
  mutate(
    fidesz_reassigned = rbinom(n(), 1, mean_prob_fidesz),
    majority_reassign = case_when(
      session %in% c(4, 5) & fidesz_reassigned == 0 ~ 1,
      session %in% c(6, 7) & fidesz_reassigned == 1 ~ 1,
      TRUE ~ 0
    )
  )


# Számítások

X_tdk_reassigned <- sparse.model.matrix(~ 0 + session + majority_reassign + gender, 
                                        data = speaker_metadata_reassign)

qx_tdk_reassigned <- qr(as.matrix(X_tdk_reassigned))
X_tdk_reassigned <- X_tdk_reassigned[, qx_tdk_reassigned$pivot[1:qx_tdk_reassigned$rank]]

rownames(X_tdk_reassigned) <- speaker_metadata_reassign$id_speaker
X_tdk_reassigned <- X_tdk_reassigned[rownames(C_tdk), ]

F_tdk_reassigned <- sparse.model.matrix(~ 0 + session, data = speaker_metadata_reassign)
F_tdk_reassigned <- F_tdk_reassigned * speaker_metadata_reassign$fidesz_reassigned

colnames(F_tdk_reassigned) <- paste(colnames(F_tdk_reassigned), 'F_tdk', sep = '_')
rownames(F_tdk_reassigned) <- speaker_metadata_reassign$id_speaker
F_tdk_reassigned <- F_tdk_reassigned[rownames(C_tdk), ]

mu_tdk_reassigned <- log(rowSums(C_tdk))

cl_tdk_reassigned <- makeCluster(2, type = ifelse(.Platform$OS.type == 'unix', 'FORK', 'PSOCK'))

# paraméterek
fit_tdk_reassigned <- dmr(
  cl = cl_tdk_reassigned,
  covars = cbind(X_tdk_reassigned, F_tdk_reassigned),
  counts = C_tdk,
  mu = mu_tdk_reassigned,
  free = 1:ncol(X_tdk_reassigned),
  fixedcost = 0.015,
  lambda.start = Inf,
  lambda.min.ratio = 0.015,
  nlambda = 150,
  standardize = TRUE
)


stopCluster(cl_tdk_reassigned)

coefs_tdk_reassigned   <- coef(fit_tdk_reassigned, k = log(nrow(X_tdk_reassigned)), corrected = FALSE)
coefs_X_tdk_reassigned <- coefs_tdk_reassigned[colnames(X_tdk_reassigned), ]
coefs_F_tdk_reassigned <- coefs_tdk_reassigned[colnames(F_tdk_reassigned), ]

session_tdk_reassigned <- speaker_metadata_reassign$session
names(session_tdk_reassigned) <- speaker_metadata_reassign$id_speaker
session_tdk_reassigned <- session_tdk_reassigned[rownames(C_tdk)]
session_tdk_reassigned <- as.matrix(session_tdk_reassigned)

I_tdk_reassigned <- sparse.model.matrix(~ 0 + session, data = speaker_metadata_reassign)
rownames(I_tdk_reassigned) <- speaker_metadata_reassign$id_speaker
I_tdk_reassigned <- I_tdk_reassigned[rownames(C_tdk), ]
phi_tdk_reassigned <- I_tdk_reassigned %*% coefs_F_tdk_reassigned

utility_mszp_tdk_reassigned <- cbind(1, X_tdk_reassigned) %*% rbind(coefs_tdk_reassigned['intercept', ], coefs_X_tdk_reassigned)
utility_fidesz_tdk_reassigned <- utility_mszp_tdk_reassigned + phi_tdk_reassigned

party_ratio_tdk_reassigned <- rowSums(exp(utility_mszp_tdk_reassigned)) / rowSums(exp(utility_fidesz_tdk_reassigned))
likelihood_ratio_tdk_reassigned <- party_ratio_tdk_reassigned * exp(phi_tdk_reassigned)
rho_tdk_reassigned <- likelihood_ratio_tdk_reassigned / (1 + likelihood_ratio_tdk_reassigned)

q_F_tdk_reassigned <- exp(utility_fidesz_tdk_reassigned) / rowSums(exp(utility_fidesz_tdk_reassigned))
q_M_tdk_reassigned <- exp(utility_mszp_tdk_reassigned) / rowSums(exp(utility_mszp_tdk_reassigned))

pi_tdk_reassigned <- rowSums(0.5 * q_F_tdk_reassigned * rho_tdk_reassigned + 
                               0.5 * q_M_tdk_reassigned * (1 - rho_tdk_reassigned))

stopifnot(all.equal(rho_tdk_reassigned, q_F_tdk_reassigned / (q_F_tdk_reassigned + q_M_tdk_reassigned)))

# Átlagos pártos polarizáció kiszámítása (reassigned adatokon)
average_pi_tdk_reassigned <- tapply(pi_tdk_reassigned, list(session_tdk_reassigned), mean)
average_pi_tdk_reassigned <- average_pi_tdk_reassigned[order(as.integer(names(average_pi_tdk_reassigned)))]
average_pi_tdk_reassigned <- data.frame(session = as.integer(names(average_pi_tdk_reassigned)),
                                        average_pi_tdk_reassigned = average_pi_tdk_reassigned)


## Ábra

average_pi_tdk <- average_pi_tdk %>%
  rename(valós = average_pi_tdk)

average_pi_tdk_reassigned <- average_pi_tdk_reassigned %>%
  rename(random = average_pi_tdk_reassigned)

combined_df <- left_join(average_pi_tdk, average_pi_tdk_reassigned, by = "session") %>%
  pivot_longer(cols = c(valós, random), names_to = "type", values_to = "value")

session_labels <- c("1998–2002", "2002–2006", "2006–2010", "2010–2014", "2014–2018")
combined_df$year <- factor(session_labels[combined_df$session - 2], levels = session_labels)

ggplot(combined_df, aes(x = year, y = value, group = type, color = type, linetype = type)) +
  geom_line(size = 1) +
  geom_point(size = 4) +
  scale_color_manual(values = c("valós" = "red4", "random" = "gray40")) +
  scale_linetype_manual(values = c("valós" = "solid", "random" = "dashed")) +
  labs(
    title = expression("Pártos polarizáció alakulása a teljes korpuszon"),
    y = "Átlagos pártos polarizáció",
    x = NULL,
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.35, face = "italic", size = 14),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) +
  ylim(0.500, 0.51)



### Validáció 2 -- legpártosabb bigramok ciklusonként ###


extract_top_partisan_phrases <- function(zeta_matrix, top_n = 10) {
  partisan_phrases_list <- list()
  
  for (session_name in rownames(zeta_matrix)) {
    zeta_values <- zeta_matrix[session_name, ]
    
    # Top kifejezések Fidesz irányba (pozitív zeta)
    top_fidesz_indices <- order(zeta_values, decreasing = TRUE)[1:top_n]
    top_fidesz_phrases <- data.frame(
      session_id = session_name,
      party = "Fidesz",
      bigram = colnames(zeta_matrix)[top_fidesz_indices],
      zeta_value = zeta_values[top_fidesz_indices],
      abs_zeta = abs(zeta_values[top_fidesz_indices])
    )
    
    # Top kifejezések MSZP irányba (negatív zeta)
    top_mszp_indices <- order(zeta_values)[1:top_n]
    top_mszp_phrases <- data.frame(
      session_id = session_name,
      party = "MSZP",
      bigram = colnames(zeta_matrix)[top_mszp_indices],
      zeta_value = zeta_values[top_mszp_indices],
      abs_zeta = abs(zeta_values[top_mszp_indices])
    )
    
    partisan_phrases_list[[session_name]] <- rbind(top_fidesz_phrases, top_mszp_phrases)
  }
  
  result_df <- do.call(rbind, partisan_phrases_list)
  rownames(result_df) <- NULL
  return(result_df)
}

# Táblázat készítése gyakorisággal együtt
create_partisan_table <- function(data, top_n = 10) {
  results <- list()
  
  for (session in unique(data$session_id)) {
    session_df <- data %>% filter(session_id == session)
    period_label <- unique(session_df$period_label)
    
    top_fidesz <- session_df %>%
      filter(party == "Fidesz") %>%
      arrange(desc(zeta_value)) %>%
      head(top_n)
    
    top_mszp <- session_df %>%
      filter(party == "MSZP") %>%
      arrange(zeta_value) %>%
      head(top_n)
    
    for (i in 1:top_n) {
      results[[length(results) + 1]] <- data.frame(
        Ciklus = period_label,
        Fideszes_kifejezés = top_fidesz$bigram[i],
        Használat_F = top_fidesz$frequency_fidesz[i],
        Használat_M = top_fidesz$frequency_mszp[i],
        MSZPs_kifejezés = top_mszp$bigram[i],
        Használat_F_mszp = top_mszp$frequency_fidesz[i],
        Használat_M_mszp = top_mszp$frequency_mszp[i],
        stringsAsFactors = FALSE
      )
    }
  }
  
  final_table <- do.call(rbind, results)
  return(final_table)
}


# Top pártos kifejezések kinyerése
top_partisan_phrases <- extract_top_partisan_phrases(coefs_F_tdk, top_n = 10) # n állítható

session_period_map <- data.frame(
  session_id = paste0("session", 3:7, "_F_tdk"),
  period_label = c("1998-2002", "2002-2006", "2006-2010", "2010-2014", "2014-2018")
)

top_phrases_with_period <- top_partisan_phrases %>%
  left_join(session_period_map, by = "session_id")

bigram_frequency_by_party <- bigram_party_period %>%
  filter(party %in% c("Fidesz", "MSZP")) %>%
  pivot_wider(names_from = party, values_from = N, values_fill = list(N = 0)) %>%
  rename(
    frequency_fidesz = Fidesz,
    frequency_mszp = MSZP,
    period_label = period
  )

# Összekapcsolás a bigram + időszak alapján
top_phrases_with_freq <- top_phrases_with_period %>%
  left_join(bigram_frequency_by_party, by = c("bigram", "period_label"))

# összeállított táblázat
partisan_table <- create_partisan_table(top_phrases_with_freq, top_n = 10)


# PDF generálása ciklusonként (összesen 5 PDF)
cleaned_table <- partisan_table %>%
  rename(
    Fideszes = Fideszes_kifejezés,
    MSZPs = MSZPs_kifejezés,
    `#F` = Használat_F,
    `#M` = Használat_M,
    `#F.` = Használat_F_mszp,
    `#M.` = Használat_M_mszp
  ) %>%
  group_by(Ciklus) %>%
  mutate(sor_index = row_number()) %>%
  ungroup()

# Célkönyvtár
output_dir <- "kerem/sajat/celkonyvtarat/allitson/be"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

unique_ciklusok <- unique(cleaned_table$Ciklus)

for (ciklus in unique_ciklusok) {
  ciklus_df <- cleaned_table %>%
    filter(Ciklus == ciklus) %>%
    select(-Ciklus, -sor_index)
  
  colnames(ciklus_df) <- c(
    paste0("Fideszes (", ciklus, ")"),
    "#F", "#M",
    paste0("MSZPs (", ciklus, ")"),
    "#F.", "#M."
  )
  
  tbl_latex <- kable(ciklus_df, format = "latex", booktabs = TRUE, longtable = TRUE) %>%
    kable_styling(latex_options = c("hold_position", "striped")) %>%
    row_spec(10, extra_latex_after = "\\midrule")
  
  full_tex <- c(
    "\\documentclass{article}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage{booktabs}",
    "\\usepackage[a4paper, margin=1in]{geometry}",
    "\\usepackage{longtable}",
    "\\usepackage{xcolor}",
    "\\usepackage{colortbl}",
    "\\begin{document}",
    paste0("\\section*{Top pártos kifejezések – ", ciklus, " (teljes korpusz)}"),
    "{\\small",
    as.character(tbl_latex),
    "}",
    "\\end{document}"
  )
  
  base_name <- gsub("[^0-9]", "", ciklus)
  tex_path <- file.path(output_dir, paste0("top_kifejezesek_teljes_korpusz_", base_name, ".tex"))
  
  writeLines(full_tex, tex_path)
  tinytex::latexmk(tex_path)
}
