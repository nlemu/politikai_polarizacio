# politikai_polarizacio

Ez a GitHub repository a *„Magyar pártos polarizáció elemzése gépi tanulási módszerrel”* című TDK dolgozat eredményeinek replikációjához szükséges kódokat és adatállományokat tartalmazza.  
**Szerzők:** Novák Levente Péter és Kaló Eszter

---

## Tartalom

A repository az alábbi fájlokat tartalmazza:

- **6 db R fájl** (`.R`)
- **1 db szövegfájl** (`.txt`)
- **2 db táblázat** (`.csv`)
- **3 db Word dokumentum** (`.docx`), amelyek Google Drive-ra feltöltött `.csv` fájlok elérési útját tartalmazzák.

*A `.docx` fájlok által hivatkozott nagy méretű állományok technikai okokból nem kerültek feltöltésre a repository-ba, emiatt alternatív úton érhetők el.*

---

## Fájlok és céljuk

Az alábbi `.R` fájlok a dolgozatban szereplő táblázatok és ábrák reprodukálásához szükséges szkripteket tartalmazzák:

- **`teljes_korpusz_letrehozasa.R`**  
  A `teljes_korpusz_df.csv` és `bigram_lista.csv` fájlok előállítását végzi.  
  **Használt inputok:** `nem_elofeldogozott.csv`, `vegso_exclude.txt`

- **`leiro_statok.R`**  
  A dolgozat 3., 4. és 5. táblázatának reprodukciójához.  
  **Használt inputok:** `teljes_korpusz_df.csv`, `bigram_lista.csv`, `speaker_metadata.csv`, `felszolalasok.csv`

- **`teljes_korpuszra.R`**  
  Az 1. ábra és a 6. táblázat reprodukciójához.  
  **Használt inputok:** `teljes_korpusz_df.csv`, `speaker_metadata.csv`

- **`economic_korpusz.R`**  
  A 3. ábra és a 7. táblázat reprodukciójához.  
  **Használt inputok:** `teljes_korpusz_df.csv`, `speaker_metadata.csv`

- **`cultural_korpusz.R`**  
  A 2. ábra és a 8. táblázat reprodukciójához.  
  **Használt inputok:** `teljes_korpusz_df.csv`, `speaker_metadata.csv`

- **`nem_nulla_magyarazo_ero.R`**  
  A 9. ábra reprodukciójához.  
  **Használt inputok:** `teljes_korpusz_df.csv`, `speaker_metadata.csv`

---

## Használt R csomagok és verziók

Az alábbi R csomagverziók voltak használatban a szkriptek futtatása során:

- `dplyr`: 1.1.3  
- `ggplot2`: 3.5.1  
- `tidyr`: 1.3.0  
- `Matrix`: 1.6.1  
- `distrom`: 1.0.1  
- `kableExtra`: 1.3.4  
- `knitr`: 1.43  
- `tinytex`: 0.57  
- `tidytext`: 0.4.2  
- `HunMineR`: 0.0.0.9000  
- `data.table`: 1.14.8  
- `officer`: 0.6.8  
- `purrr`: 1.0.2
