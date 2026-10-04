###########################################################
# 04b: Finding 4. Age and geography: the urban-rural divide
# Municipality-level results (GERDA; Landeswahlleitung MV for MV 2026) by population size and density.
# Output: figures 04_urban_rural_*.png, table urban_rural_by_size.csv
###########################################################

source("_theme.R")
# ---- data ---------------------------------------------------------------------
cols <- c("ags", "election_year", "election_date", "eligible_voters", "valid_votes", "turnout", "afd", "spd", "gruene", "fdp", "linke_pds", "bsw")
harm <- read_csv("../data/input/gerda/state_harm_21.csv", show_col_types = FALSE, guess_max = 5000,
                 col_select = all_of(c(cols, "state_name", "cdu_csu"))) |>
  rename(state = state_name, cdu = cdu_csu)
unharm <- read_csv("../data/input/gerda/state_unharm.csv", show_col_types = FALSE, guess_max = 5000,
                   col_select = all_of(c(cols, "state", "cdu", "csu"))) |>
  mutate(cdu = coalesce(cdu, csu)) |> select(-csu)

# MV 2026 is not in GERDA yet: official municipality file from the Landeswahlleitung
# (wahlen.mvnet.de, final result 29 Sept 2026). Postal votes of amtsangehörige Gemeinden
# sit in separate "Briefwahl <Amt>" pseudo-rows and are dropped here.
mv <- read_csv2("../data/input/mv/ltw2026_mv_gemeinden_utf8.csv", skip = 5, show_col_types = FALSE, na = c("", "x"),
                locale = locale(decimal_mark = ",")) |>
  filter(Ausgabe == "A", `Erst-/Zweitstimme` == 2, !str_starts(Gemeindename, "Briefwahl")) |>
  transmute(ags = as.character(Gemeinde), election_year = 2026, election_date = as.Date("2026-09-20"),
            state = "Mecklenburg-Vorpommern", eligible_voters = Wahlberechtigte, valid_votes = `Gültige Stimmen`,
            turnout = Wahlbeteiligung / 100,
            afd = AfD / valid_votes, cdu = CDU / valid_votes, spd = SPD / valid_votes, gruene = GRÜNE / valid_votes,
            fdp = FDP / valid_votes, linke_pds = `Die Linke` / valid_votes, bsw = BSW / valid_votes)

# recent elections: harmonised GERDA plus MV 2026
recent <- bind_rows(harm |> filter(election_year >= 2024, !is.na(afd)), mv) |>
  filter(state != "Hamburg")

cv <- read_csv("../data/input/gerda/ags_area_pop_emp.csv", show_col_types = FALSE) |>
  filter(year == 2021) |> select(ags = ags_21, name = ags_name_21, pop = population_ags, area = area_ags, density = pop_density_ags)

m <- recent |> left_join(cv, by = "ags") |>
  filter(!is.na(pop), !is.na(afd), eligible_voters > 0) |>
  mutate(across(c(afd, cdu, spd, gruene, fdp, linke_pds, bsw), ~ . * 100),
         pop_k = pop,  # population in thousands
         size = cut(pop_k, c(0, 2, 5, 10, 20, 50, 100, Inf),
                    labels = c("< 2k", "2-5k", "5-10k", "10-20k", "20-50k", "50-100k", "> 100k"), right = FALSE),
         east = state %in% c("Saxony-Anhalt", "Mecklenburg-Vorpommern", "Saxony", "Brandenburg", "Thuringia"),
         label = paste0(state, " ", election_year))
cat("Municipalities matched:", nrow(m), "of", nrow(recent), "\n")
print(m |> count(label))

# ---- 1. AfD by municipality size class (weighted by eligible voters) -------------
by_size <- m |> group_by(label, state, east, election_year, size) |>
  summarise(n = n(), voters = sum(eligible_voters),
            across(c(afd, cdu, spd, gruene, linke_pds, bsw), ~ weighted.mean(., valid_votes, na.rm = TRUE)), .groups = "drop")
write_csv(by_size, "../data/output/tables/urban_rural_by_size.csv")
gap <- by_size |> group_by(label) |> summarise(smallest = afd[1], largest = afd[n()], gap = smallest - largest) |> arrange(desc(gap))
print(gap)

state_cols <- c("Saxony-Anhalt 2026" = "#cc0065", "Mecklenburg-Vorpommern 2026" = "#e0609a", "Thuringia 2024" = "#7a003c",
                "Saxony 2024" = "#b03070", "Brandenburg 2024" = "#d890b8", "Baden-Württemberg 2026" = "#000000", "Rhineland-Palatinate 2026" = "#777777")
p15 <- ggplot(by_size, aes(size, afd, colour = label, group = label)) +
  geom_line(linewidth = 1.5) + geom_point(size = 3) +
  geom_text_repel(data = by_size |> group_by(label) |> filter(size == last(size)), aes(label = label),
                  hjust = 0, nudge_x = .25, direction = "y", family = "Fira Sans", size = 3.6, show.legend = FALSE, segment.colour = "#bbbbbb") +
  scale_colour_manual(values = state_cols, guide = "none") +
  scale_y_continuous(labels = \(x) paste0(x, "%"), breaks = seq(0, 60, 10), limits = c(0, 60)) +
  scale_x_discrete(expand = expansion(add = c(.4, 2.6))) +
  labs(title = "Village vs. city: the AfD vote falls with municipality size everywhere",
       subtitle = "AfD vote share by municipality population (classes), eligible-voter weighted. East German states in pink, West in grey/black.",
       x = "municipality population", y = NULL) +
  theme_sm()
ggsave("../figures/04_urban_rural_afd_by_size.png", p15, width = 12, height = 7, dpi = 200, device = agg_png, bg = "white")

# ---- 2. scatter: density vs AfD share, per state ------------------------------
cors <- m |> group_by(label) |> summarise(r = cor(log10(density), afd, use = "complete.obs"), n = n()) |> mutate(txt = sprintf("r = %.2f (n = %d)", r, n))
print(cors)
p16 <- ggplot(m, aes(density, afd)) +
  geom_point(aes(size = eligible_voters), alpha = .25, colour = "#009EE0", shape = 16) +
  geom_smooth(aes(weight = eligible_voters), method = "loess", formula = y ~ x, se = FALSE, colour = "#000000", linewidth = 1.2, span = .8) +
  geom_text(data = cors, aes(x = 3000, y = 68, label = txt), hjust = 1, family = "Fira Sans", size = 3.6, colour = "#333333") +
  facet_wrap(~ label, nrow = 2) +
  scale_x_log10(breaks = c(10, 100, 1000), labels = c("10", "100", "1,000")) +
  scale_size_area(max_size = 10, guide = "none") +
  scale_y_continuous(labels = \(x) paste0(x, "%"), limits = c(0, 72)) +
  labs(title = "AfD vote share and population density, municipality level",
       subtitle = "Each dot = one municipality (size = eligible voters); line = voter-weighted loess. r = correlation with log density.",
       x = "inhabitants per km² (log scale)", y = NULL) +
  theme_sm(14)
ggsave("../figures/04_urban_rural_density.png", p16, width = 15, height = 8, dpi = 200, device = agg_png, bg = "white")

# ---- 3. all parties by size, Saxony-Anhalt and MV 2026 -------------------------
party_cols <- c(CDU = "#000000", SPD = "#E3000F", Greens = "#46962b", Left = "#BE3075", AfD = "#009EE0", BSW = "#7B2A7A")
ps <- by_size |> filter(election_year == 2026, east) |>
  select(label, size, AfD = afd, CDU = cdu, SPD = spd, Greens = gruene, Left = linke_pds, BSW = bsw) |>
  pivot_longer(AfD:BSW, names_to = "party", values_to = "pct") |>
  mutate(party = factor(party, levels = names(party_cols)))
p17 <- ggplot(ps, aes(size, pct, colour = party, group = party)) +
  geom_line(linewidth = 1.5) + geom_point(size = 3) +
  geom_text_repel(data = ps |> group_by(label, party) |> filter(size == last(size)), aes(label = party), hjust = 0, nudge_x = .25, direction = "y",
                  family = "Fira Sans", size = 3.6, show.legend = FALSE, segment.colour = "#bbbbbb") +
  facet_wrap(~ label) +
  scale_colour_manual(values = party_cols, guide = "none") +
  scale_y_continuous(labels = \(x) paste0(x, "%")) +
  scale_x_discrete(expand = expansion(add = c(.4, 1.6))) +
  labs(title = "Only the AfD has a steep rural gradient. The Greens are its mirror image.",
       subtitle = "Vote share by municipality population, 2026 state elections (voter-weighted)", x = "municipality population", y = NULL) +
  theme_sm(14)
ggsave("../figures/04_urban_rural_parties_2026.png", p17, width = 14, height = 6.5, dpi = 200, device = agg_png, bg = "white")

cat("done\n")
