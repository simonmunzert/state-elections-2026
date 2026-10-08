###########################################################
# 00: Scrape all German state election results (1946-2026)
# Source: wahlrecht.de/ergebnisse/<state>.htm
# Output: output/state_elections_long.csv
#   one row per state x election x party, with vote share,
#   seats, turnout, total seats, party code + party family
###########################################################

suppressMessages({
  library(tidyverse); library(rvest); library(haven); library(janitor)
})

states <- tribble(
  ~land, ~slug,                    ~name_en,                 ~east,
  "bw",  "baden-wuerttemberg",     "Baden-Württemberg",      0,
  "by",  "bayern",                 "Bavaria",                0,
  "be",  "berlin",                 "Berlin",                 0,
  "bb",  "brandenburg",            "Brandenburg",            1,
  "hb",  "bremen",                 "Bremen",                 0,
  "hh",  "hamburg",                "Hamburg",                0,
  "he",  "hessen",                 "Hesse",                  0,
  "mv",  "mecklenburg-vorpommern", "Mecklenburg-Vorpommern", 1,
  "ni",  "niedersachsen",          "Lower Saxony",           0,
  "nw",  "nordrhein-westfalen",    "North Rhine-Westphalia", 0,
  "rp",  "rheinland-pfalz",        "Rhineland-Palatinate",   0,
  "sl",  "saarland",               "Saarland",               0,
  "sn",  "sachsen",                "Saxony",                 1,
  "st",  "sachsen-anhalt",         "Saxony-Anhalt",          1,
  "sh",  "schleswig-holstein",     "Schleswig-Holstein",     0,
  "th",  "thueringen",             "Thuringia",              1
)

# ---- download (cached in input/wahlrecht) --------------------------------
dir.create("../data/input/wahlrecht", showWarnings = FALSE, recursive = TRUE)
for (i in seq_len(nrow(states))) {
  f <- file.path("../data/input/wahlrecht", paste0(states$slug[i], ".htm"))
  if (!file.exists(f) || file.size(f) < 1000) {
    url_slug <- ifelse(states$slug[i] == "mecklenburg-vorpommern", "mecklenburg", states$slug[i])
    download.file(paste0("https://www.wahlrecht.de/ergebnisse/", url_slug, ".htm"), f,
                  quiet = TRUE, headers = c("User-Agent" = "Mozilla/5.0"))
  }
}

# ---- parser ---------------------------------------------------------------
clean_num <- function(x) {
  x <- str_replace_all(x, "[–—­\\s]", "")  # dashes, soft hyphen
  x <- str_replace(x, ",", ".")
  x <- str_remove_all(x, "[^0-9.]")
  suppressWarnings(as.numeric(x))
}

parse_state <- function(land, slug) {
  h  <- read_html(file.path("../data/input/wahlrecht", paste0(slug, ".htm")))
  tb <- html_elements(h, "table")
  tb <- tb[[which.max(sapply(tb, function(x) length(html_elements(x, "tr"))))]]
  ylab   <- html_elements(tb, "thead tr:first-child th.jahr") |> html_text2()
  years  <- ylab |> str_extract("\\d{4}") |> as.integer()
  prelim <- ylab |> str_detect("\\*")
  elec_no <- seq_along(years)
  rows <- html_elements(tb, "tbody tr")
  out <- map_dfr(rows, function(r) {
    cells <- html_elements(r, "td") |> html_text2()
    label <- str_squish(str_replace_all(cells[1], "[­]", ""))
    vals  <- cells[-1]
    vals  <- vals[seq_len(2 * length(years))]
    tibble(land = land, elec_no = elec_no, year = years, prelim = prelim,
           label_raw = label,
           pct   = clean_num(vals[seq(1, length(vals), 2)]),
           seats = clean_num(vals[seq(2, length(vals), 2)]))
  })
  out
}

raw <- map2_dfr(states$land, states$slug, parse_state)

# ---- turnout / total seats -----------------------------------------------
turnout <- raw |>
  filter(str_detect(label_raw, regex("^Wahlbe", ignore_case = TRUE))) |>
  transmute(land, elec_no, turnout = pct, total_seats = seats)

res <- raw |>
  filter(!str_detect(label_raw, regex("^Wahlbe", ignore_case = TRUE))) |>
  left_join(turnout, by = c("land", "elec_no"))

# Hamburg 2025 (2 March 2025) is missing on wahlrecht.de -> add manually
# Source: Statistikamt Nord, final result
hh2025 <- tribble(
  ~label_raw, ~pct, ~seats,
  "SPD", 33.5, 45, "CDU", 19.8, 26, "GRÜNE/GAL", 18.5, 25, "DIE LINKE", 11.2, 15,
  "AfD", 7.5, 10, "Volt", 3.3, NA, "FDP", 2.3, NA, "BSW", 1.8, NA, "Sonstige", 2.1, NA) |>
  mutate(land = "hh", elec_no = max(res$elec_no[res$land == "hh"]) + 1L,
         year = 2025L, prelim = FALSE, turnout = 67.7, total_seats = 121)
res <- bind_rows(res, hh2025)

# ---- harmonize party labels ------------------------------------------------
# strip footnote markers (digits/superscripts at end, "1, 4" etc.)
res <- res |>
  mutate(label = label_raw |>
           str_remove_all("[¹²³⁴⁵⁶]") |>
           str_remove("\\s*\\d+(,\\s*\\d+)*$") |>
           str_remove("\\d$") |>
           str_squish())

# combined-row disambiguation by year (labels like "DRP/NPD", "SRP/NPD/DVU")
res <- res |>
  mutate(party = case_when(
    str_detect(label, "^CDU|^CSU|Niederdeutsche") ~ "cdu",
    str_detect(label, "^SPD") ~ "spd",
    str_detect(label, "^FDP|^DPS|^F\\.D\\.P") ~ "fdp",
    str_detect(label, "GR.NE|GAL|^B.90") ~ "gru",
    str_detect(label, "LINKE|Linke|^PDS") ~ "lin",
    str_detect(label, "^AfD") ~ "afd",
    str_detect(label, "^BSW") ~ "bsw",
    str_detect(label, "SRP/NPD/DVU") & year <= 1955 ~ "srp",
    str_detect(label, "SRP/NPD/DVU") & year <= 1980 ~ "npd",
    str_detect(label, "SRP/NPD/DVU") ~ "dvu",
    str_detect(label, "^DRP/NPD") & year <= 1965 ~ "drp",
    str_detect(label, "^DRP/NPD") ~ "npd",
    str_detect(label, "^NPD") ~ "npd",
    str_detect(label, "^SRP") ~ "srp",
    str_detect(label, "^DVU") ~ "dvu",
    str_detect(label, "^REP") ~ "rep",
    str_detect(label, "^Schill|^SCHILL") ~ "schill",
    str_detect(label, "^BIW") ~ "biw",
    str_detect(label, "^DSU") ~ "dsu",
    str_detect(label, "^DP$|^NLP/DP|^DP\\b") ~ "dp",
    str_detect(label, "BHE|GDP") ~ "bhe",
    str_detect(label, "KPD|DKP|^KP/") ~ "kpd",
    str_detect(label, "SED|SEW") ~ "sew",
    str_detect(label, "PIRATEN") ~ "pir",
    str_detect(label, "FREIE W|^FW$|Freie W|BVB") ~ "fw",
    str_detect(label, "^SSV|^SSW") ~ "ssw",
    str_detect(label, "STATT") ~ "statt",
    str_detect(label, "^Sonstige") ~ "oth",
    TRUE ~ str_to_lower(str_replace_all(label, "[^A-Za-z]", ""))
  ))

# party families (for "extreme right" analyses)
far_right <- c("afd", "npd", "dvu", "rep", "schill", "srp", "drp", "biw", "dsu")
res <- res |>
  mutate(family = case_when(
    party %in% c("cdu") ~ "CDU/CSU",
    party == "spd" ~ "SPD",
    party == "fdp" ~ "FDP",
    party == "gru" ~ "Greens",
    party %in% c("lin", "sew") ~ "Left",
    party == "afd" ~ "AfD",
    party == "bsw" ~ "BSW",
    party %in% far_right ~ "Far right (pre-AfD)",
    party == "kpd" ~ "KPD/DKP",
    party == "oth" ~ "Other",
    TRUE ~ "Other (named)"
  ),
  far_right = party %in% far_right)

# ---- election dates ------------------------------------------------------
# 1946-2016 from the zweitstimme .dta, 2017-2026 hand-coded
dta <- read_dta("../data/input/ltw_ergebnisse_bis2016.dta") |>
  filter(land != "europa") |>
  mutate(land = recode(land,
    'baden-wuerttemberg'='bw','bayern'='by','berlin'='be','brandenburg'='bb',
    'bremen'='hb','hamburg'='hh','hessen'='he','mecklenburg'='mv',
    'niedersachsen'='ni','nordrhein-westfalen'='nw','rheinland-pfalz'='rp',
    'saarland'='sl','sachsen'='sn','sachsen-anhalt'='st',
    'schleswig-holstein'='sh','thueringen'='th')) |>
  distinct(land, year, date = as.Date(eventdate))

dates_new <- tribble(
  ~land, ~year, ~date,
  "sl",2017,"2017-03-26","sh",2017,"2017-05-07","nw",2017,"2017-05-14","ni",2017,"2017-10-15",
  "by",2018,"2018-10-14","he",2018,"2018-10-28",
  "hb",2019,"2019-05-26","bb",2019,"2019-09-01","sn",2019,"2019-09-01","th",2019,"2019-10-27",
  "hh",2020,"2020-02-23",
  "bw",2021,"2021-03-14","rp",2021,"2021-03-14","st",2021,"2021-06-06","be",2021,"2021-09-26","mv",2021,"2021-09-26",
  "sl",2022,"2022-03-27","sh",2022,"2022-05-08","nw",2022,"2022-05-15","ni",2022,"2022-10-09",
  "be",2023,"2023-02-12","hb",2023,"2023-05-14","by",2023,"2023-10-08","he",2023,"2023-10-08",
  "th",2024,"2024-09-01","sn",2024,"2024-09-01","bb",2024,"2024-09-22",
  "hh",2025,"2025-03-02",
  "bw",2026,"2026-03-08","rp",2026,"2026-03-22","st",2026,"2026-09-06","be",2026,"2026-09-20","mv",2026,"2026-09-20"
) |> mutate(date = as.Date(date))

dates <- bind_rows(dta, dates_new) |> distinct(land, year, date) |>
  arrange(land, year, date) |> group_by(land, year) |> mutate(k = row_number()) |> ungroup()

res <- res |>
  group_by(land, year) |> mutate(k = dense_rank(elec_no)) |> ungroup() |>
  left_join(dates, by = c("land", "year", "k")) |>
  mutate(date = coalesce(date, as.Date(paste0(year, "-07-01")))) |>
  left_join(states |> select(land, name_en, east), by = "land") |>
  arrange(land, year, desc(pct)) |>
  select(land, name_en, east, elec_no, year, date, prelim, party, family, far_right,
         label_raw, pct, seats, turnout, total_seats)

# manual corrections of turnout where wahlrecht.de deviates from the official final result
# (checked against the Landeswahlleitungen, 8 Oct 2026)
res <- res |>
  mutate(turnout = case_when(land == "rp" & year == 2026 ~ 68.4,   # wahlen.rlp.de: 68,4 (wahlrecht.de shows 63,5)
                             land == "st" & year == 2026 ~ 77.8,   # final result (wahlrecht.de preliminary 77,7)
                             land == "mv" & year == 2026 ~ 78.0,   # final result, wahlen.mvnet.de (wahlrecht.de preliminary 78,1)
                             TRUE ~ turnout))

# "other" share: 100 minus named parties (wahlrecht "Sonstige" is sometimes empty)
res <- res |>
  group_by(land, elec_no) |>
  mutate(pct = if_else(party == "oth" & is.na(pct),
                       round(100 - sum(pct[party != "oth"], na.rm = TRUE), 1), pct)) |>
  ungroup()

# ---- checks ----------------------------------------------------------------
missing_dates <- res |> filter(is.na(date)) |> distinct(land, year)
if (nrow(missing_dates)) print(missing_dates)
cat("Elections:", nrow(distinct(res, land, year)), "| rows:", nrow(res), "\n")
print(res |> filter(is.na(pct) & !is.na(seats)) |> nrow())
# vote shares of named parties + Sonstige should be ~100
chk <- res |> group_by(land, year) |> summarise(s = sum(pct, na.rm = TRUE), .groups = "drop") |>
  filter(abs(s - 100) > 1.5)
if (nrow(chk)) { cat("Sum-of-shares off by >1.5 in:\n"); print(chk) }

write_csv(res, "../data/output/state_elections_long.csv")
cat("Written: ../data/output/state_elections_long.csv\n")
