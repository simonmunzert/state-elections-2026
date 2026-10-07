# Four things open election data tell us about the 2026 state elections

Material for the Hertie School panel "Is the centre cracking? What recent elections reveal about
Germany's changing party landscape", 8 October 2026.

## Layout

- `code/` — R scripts, organised by finding (see below); `_theme.R` holds shared colours, theme and helpers.
- `data/input/` — downloaded sources (wahlrecht.de pages, GERDA, Landeswahlleitung MV, Berlin precinct data,
  zweitstimme forecasts, KAS exit-poll extracts, the 1946-2016 election-date file).
- `data/output/` — the scraped state-election dataset (`state_elections_long.csv`) and all tables.
- `data/sources/` — research notes with all sources and literature, far-right cross-check table.
- `figures/` — all figures, named by finding (`01_*` to `05_*`).
- `slides/` — the xaringan deck `state-elections-2026.Rmd` (+ rendered HTML); `slides/template-medem25/` is the
  older deck used as scaffold. The deck uses its own copies of the figures in `slides/pics/analysis/`.

## Scripts (run from `code/`, in this order, with a UTF-8 locale)

```bash
cd code && for f in 0*.R; do LANG=en_US.UTF-8 Rscript "$f"; done
```

| Script | Finding | Content |
|---|---|---|
| `00_data_state_elections.R` | data | scrapes all 257 state elections 1946-2026 from wahlrecht.de (vote shares, seats, turnout, harmonised party codes, far-right flag); adds Hamburg 2025 by hand |
| `01_centre.R` | 1. The centre is not cracking | 2026 results at a glance, CDU/CSU+SPD share, effective number of parties, swings, plurality winners |
| `02_far_right.R` | 2. Far right: not new, but stronger | far-right parties in Landtage 1946-2026 (timeline, strength, re-entry), AfD by state election, AfD vs. federal 2025 |
| `03a_cleavages_age_education.R` | 3. Age and geography | vote by age and by education (exit polls via KAS) |
| `03b_cleavages_urban_rural.R` | 3. Age and geography | municipality-level results (GERDA, MV file) by size and density |
| `03c_cleavages_berlin.R` | 3. Age and geography | Berlin 2026 by distance to the centre, inside/outside the S-Bahn-Ring, maps |
| `04_turnout.R` | 4. Turnout is back | turnout 1946-2026, net gains from non-voters (infratest dimap), turnout persistence after surges |
| `05_bonus_forecasts.R` | bonus | zweitstimme.org frozen forecasts vs. results |

Rendering the deck from the command line needs RStudio's pandoc:

```bash
cd slides && RSTUDIO_PANDOC=/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64 LANG=en_US.UTF-8 Rscript -e 'rmarkdown::render("state-elections-2026.Rmd")'
```

## Data sources

- wahlrecht.de (`/ergebnisse/<state>.htm`): all state elections since 1946, all parties, seats, turnout.
- GERDA, german-elections.com (CC BY 4.0): municipality- and constituency-level results 1946-2026; R package `gerda`.
- Landeswahlleitung MV (wahlen.mvnet.de): municipality CSV for the 2026 election (not yet in GERDA).
- Landeswahlleiterin Berlin / AfS Berlin-Brandenburg (CC BY 3.0 DE): precinct results 2026, 2023 recomputed on
  2026 precincts, structural data; geometries from gdi.berlin.de; S-Bahn-Ring from OpenStreetMap (ODbL).
- KAS Monitor Wahl- und Sozialforschung: infratest dimap and FGW exit polls and vote flows (hand-coded / parsed).
- zweitstimme.org API archive: frozen state forecasts.
- StatePol (statepol.github.io/Database) and the Bundeswahlleiterin's "Ergebnisse früherer Landtagswahlen"
  PDF were checked but are not used in the scripts.
