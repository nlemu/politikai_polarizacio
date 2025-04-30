########
# Ebben a szkriptben az előfeldolgozás lépéseinek reprodukálása található
# Replikálja a teljes_korpusz_df csv fájlt és a bigram_lista csv fájlt
########

library(dplyr)
library(tidytext)
library(HunMineR)
library(data.table)
library(Matrix)
library(tidyr)
library(officer)

setwd("Kerem/sajat/konyvtarat/allitson/be")

df <- read.csv("nem_elofeldolgozott.csv", fileEncoding = "UTF-8")

econ_topics <- c(1, 3, 4, 5, 7, 8, 10, 13, 14, 15, 18)
cult_topics <- c(2, 6, 7, 12, 13, 14, 16, 19, 23)

# "Predicted" változó létrehozása
df <- df %>%
  mutate(predicted = case_when(
    major_topic_pred %in% econ_topics & major_topic_pred %in% cult_topics ~ "both",
    major_topic_pred %in% econ_topics ~ "econ",
    major_topic_pred %in% cult_topics ~ "cult",
    TRUE ~ "neither"
  ))


# Szöveg előfeldolgotása a "text_lemma" változón
df$text_lemma <- tolower(df$text_lemma) # kisbetűssé alakítás
df$text_lemma <- gsub("\\(.*?\\)", "", df$text_lemma)  # Zárójelben lévő részek eltávolítása
df$text_lemma <- gsub("[-']", "", df$text_lemma)  # Kötőjelek és aposztrófok törlése
df$text_lemma <- gsub("[[:punct:]]", " ", df$text_lemma)  # Egyéb írásjelek cseréje szóközre


# Alap dataframe Data.table-lé átalakítása (gyorsítás miatt)
setDT(df)

# Tibble-re alakítás, majd tokenizálás
bigrams <- df %>%
  as_tibble() %>%
  unnest_tokens(bigram, text_lemma, token = "ngrams", n = 2) %>%
  as.data.table()

### Bigramok szűrése (dolgozatban leírt előfeldolgozási szempontok szerint)
# Bigramok előfordulásainak számolása ciklusonként (period és speaker szintjén)
top_bigrams <- bigrams[, .N, by = .(bigram, period, speaker)]

# Egy bigram legalább egy ciklusban (period) elérte-e az 5 előfordulást
valid_bigrams <- top_bigrams[, any(N >= 5), by = bigram][V1 == TRUE, bigram]

# Csak azokat a bigramokat tartjuk meg, amelyek teljesítik a feltételt
filtered_bigrams <- bigrams[bigram %in% valid_bigrams]

# Bigram szintű összegzés
bigram_summary <- bigrams[bigram %in% valid_bigrams, .(total_speakers = uniqueN(speaker), total_occurrences = .N), by = bigram]

# Legalább 5 felszólaló és összesen legalább 10 előfordulás
filtered_bigrams <- bigram_summary[total_speakers >= 5 & total_occurrences >= 10]



### procedurális bigramok és alacsony szemantikai jelentéssel bíró bigramok kiszűrése ###

vegse_exclude <- readLines("vegso_exclude.txt", encoding = "UTF-8")

filtered_bigrams <- filtered_bigrams[!bigram %in% vegso_exclude]


### azon bigramok kiszűrése, amiben szerepel stopword
uj_szavak <- c("orbán", "képviselőtárs", "államtitkár", "képviselő", "úr", "asszony", "elnök", "országgyűlés", "köztársasági","köszön", "ház", "szó", "ülés", "bizottsági", 
               "általános", "vita", "csengő", "megkocogtatás", "szeret", "kormánypárt", "határozati", "módosítás", "miniszter", "gyurcsány", "tisztelt", "taps", "mszp", "sor", 
               "módosító", "javaslat", "szóló", "törvény", "fidesz", "padsor", "ellenzék", "ellenzéki", "kormány", "kormánypárti","bizottság", "albizottság", "miniszterelnök", 
               "időkeret", "jelez", "elfogad", "törvényjavaslat", "szocialista", "medgyessy", "ajánlási", "alelnök", "benyújtott", "dr", "elmúlt", "elnézés", "elnöki", "érdekes", 
               "fideszmagyar", "szabaddemokrata", "jobbik", "napirend", "napirendi", "határozat", "szövetség", "választási", "vonatkozó", "szocialistaszabad", "válaszol", "zaj")
stopszavak <- c(HunMineR::data_stopwords_extra, uj_szavak)


bigrams_15552 <- filtered_bigrams[!sapply(bigram, function(b) {
  words <- unlist(strsplit(tolower(b), " "))
  any(words %in% stopszavak)
})]


final_df <- bigrams[bigram %in% bigrams_15552$bigram,
                    .N,
                    by = .(speaker, period, party, bigram, predicted, major_topic_pred)]

# Átalakítás data.frame-mé
final_df <- as.data.frame(final_df)


# Létrehozott fájl mentése (teljes_korpusz_replikalt)
write.csv(final_df, file = "teljes_korpusz_df_replikalt.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Létrehozott bigram lista mentése (bigram_lista_replikalt)
bigrams_15552_2 <- bigrams_15552 %>% 
  select(-total_occurrences, -total_speakers)

write.csv(bigrams_15552_2,
          file = "bigram_lista_replikalt.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

