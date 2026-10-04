###########################################################
# 04c: Finding 4. Age and geography: Berlin, centre vs. periphery
# Berlin 2026 precinct results, 2023 on 2026 precincts, geometries and the S-Bahn-Ring.
# Output: figures 04_berlin_*.png, tables berlin_*.csv
###########################################################

source("_theme.R")
suppressMessages({ library(sf); library(patchwork) })
# ---- results 2026, aggregated to Briefwahlbezirk (Urnen + Brief) ----------------------
r26 <- read_delim("../data/input/berlin/Datenexport_AGH2026_Zweitstimme_W_BE.csv", delim = ";", locale = locale(decimal_mark = ",", encoding = "UTF-8"),
                  show_col_types = FALSE, guess_max = 5000) |>
  transmute(bwb = Briefwahlbezirk, ostwest = OstWest, wber = WberIns, valid = Gueltig,
            CDU = P01, SPD = P02, Greens = P03, Left = P04, AfD = P05, FDP = P06) |>
  group_by(bwb) |> summarise(ostwest = first(ostwest), wber = sum(wber), valid = sum(valid),
                             across(CDU:FDP, sum), .groups = "drop")
r23 <- read_csv("../data/input/berlin/AGH2023_Zweitstimme_on_2026_Wahlbezirke_from_xlsx.csv", show_col_types = FALSE) |>
  transmute(bwb = Briefwahlbezirksnummer, valid23 = `Gültige Stimmen`, CDU23 = CDU, SPD23 = SPD, Greens23 = GRÜNE, Left23 = `Die Linke`, AfD23 = AfD) |>
  group_by(bwb) |> summarise(across(valid23:AfD23, sum), .groups = "drop")
str <- read_csv("../data/input/berlin/AGH2026_Strukturdaten_Wahlbezirke_from_xlsx.csv", show_col_types = FALSE) |>
  transmute(bwb = paste0(Bezirksnummer, Briefwahlbezirksnummer), pop = `Einwohner Anzahl`, pop65 = `Einwohner 65 und älter Anzahl`,
            foreign = `Ausländer Anzahl`, mig = `Deutsche 18+ Migrationshintergrund Anzahl`) |>
  group_by(bwb) |> summarise(across(pop:mig, sum), .groups = "drop")

d <- r26 |> left_join(r23, by = "bwb") |> left_join(str, by = "bwb") |>
  mutate(across(c(CDU, SPD, Greens, Left, AfD, FDP), ~ 100 * . / valid),
         across(c(CDU23, SPD23, Greens23, Left23, AfD23), ~ 100 * . / valid23),
         share65 = 100 * pop65 / pop, share_foreign = 100 * foreign / pop)

# ---- geometry: distance to the Brandenburg Gate, inside the Ring ----------------------
g <- st_read("../data/input/berlin/gdi_agh2026_bwb_epsg25833.geojson", quiet = TRUE) |> select(bwb) |> st_make_valid()
gate <- st_sfc(st_point(c(13.3777, 52.5163)), crs = 4326) |> st_transform(st_crs(g))
ring <- st_read("../data/input/berlin/sbahn_ring_polygon_wgs84.geojson", quiet = TRUE) |> st_transform(st_crs(g)) |> st_make_valid()
cen <- st_centroid(g)
g$dist_km <- as.numeric(st_distance(cen, gate)) / 1000
g$in_ring <- as.logical(st_within(cen, ring, sparse = FALSE)[, 1])
d <- d |> left_join(st_drop_geometry(g), by = "bwb") |> filter(!is.na(dist_km), ostwest %in% c("O", "W")) |>
  mutate(east = ostwest == "O", band = cut(dist_km, c(0, 2, 4, 6, 8, 11, 15, 30), labels = c("0-2", "2-4", "4-6", "6-8", "8-11", "11-15", "15+"), right = FALSE),
         ring_lab = if_else(in_ring, "inside the Ring", "outside the Ring"), ew_lab = if_else(east, "East Berlin", "West Berlin"))
cat("Briefwahlbezirke with data:", nrow(d), "| inside ring:", sum(d$in_ring), "| East:", sum(d$east), "\n")
write_csv(d |> select(-ostwest), "../data/output/tables/berlin_bwb_2026.csv")

# ---- 1. shares by distance band, 2026 and change vs 2023 -------------------------------
long <- d |> select(bwb, valid, band, CDU, SPD, Greens, Left, AfD, CDU23, SPD23, Greens23, Left23, AfD23) |>
  pivot_longer(c(CDU, SPD, Greens, Left, AfD), names_to = "party", values_to = "pct") |>
  mutate(pct23 = case_when(party == "CDU" ~ CDU23, party == "SPD" ~ SPD23, party == "Greens" ~ Greens23, party == "Left" ~ Left23, party == "AfD" ~ AfD23)) |>
  group_by(band, party) |> summarise(pct = weighted.mean(pct, valid), pct23 = weighted.mean(pct23, valid, na.rm = TRUE), .groups = "drop") |>
  mutate(change = pct - pct23, party = factor(party, levels = names(party_cols)))
print(long |> select(band, party, pct) |> pivot_wider(names_from = party, values_from = pct) |> mutate(across(where(is.numeric), \(x) round(x, 1))))
pa <- ggplot(long, aes(band, pct, colour = party, group = party)) + geom_line(linewidth = 1.5) + geom_point(size = 3) +
  ggrepel::geom_text_repel(data = long |> filter(band == "15+"), aes(label = party), hjust = 0, nudge_x = .3, direction = "y", family = "Fira Sans", size = 4, show.legend = FALSE) +
  scale_colour_manual(values = party_cols, guide = "none") + scale_y_continuous(labels = \(x) paste0(x, "%")) +
  scale_x_discrete(expand = expansion(add = c(.4, 1.4))) +
  labs(title = "Vote share 2026 by distance from the Brandenburg Gate", x = "km from the centre", y = NULL) + theme_sm(14)
pb <- ggplot(long, aes(band, change, colour = party, group = party)) + geom_hline(yintercept = 0, colour = "#999999") +
  geom_line(linewidth = 1.5) + geom_point(size = 3) +
  ggrepel::geom_text_repel(data = long |> filter(band == "15+"), aes(label = party), hjust = 0, nudge_x = .3, direction = "y", family = "Fira Sans", size = 4, show.legend = FALSE) +
  scale_colour_manual(values = party_cols, guide = "none") + scale_y_continuous(labels = \(x) sprintf("%+.0f", x)) +
  scale_x_discrete(expand = expansion(add = c(.4, 1.4))) +
  labs(title = "Change vs. 2023 (percentage points)", x = "km from the centre", y = NULL) + theme_sm(14)
p20 <- (pa | pb) + plot_annotation(
  title = "Berlin 2026: the centre went Left, the periphery stayed CDU and AfD",
  subtitle = "1,572 postal-vote districts (Urnen + Brief), grouped by the distance of their centroid from the Brandenburg Gate; vote-weighted means",
  theme = theme(plot.title = element_text(face = "bold", size = 18, family = "Fira Sans"), plot.subtitle = element_text(colour = "#555555", family = "Fira Sans"),
                plot.caption = element_text(colour = "#888888", size = 9, hjust = 0, family = "Fira Sans"), plot.background = element_rect(fill = "white", colour = NA)))
ggsave("../figures/04_berlin_distance.png", p20, width = 15, height = 6.5, dpi = 200, device = agg_png, bg = "white")

# ---- 2. "The Ring is the new Wall": 2x2 East/West x inside/outside --------------------------
cell <- d |> group_by(ew_lab, ring_lab) |>
  summarise(across(c(CDU, SPD, Greens, Left, AfD), ~ weighted.mean(., valid)), n = n(), valid = sum(valid), .groups = "drop") |>
  mutate(left_bloc = SPD + Greens + Left)
print(cell |> mutate(across(where(is.numeric), \(x) round(x, 1))))
write_csv(cell, "../data/output/tables/berlin_ring_vs_wall.csv")
cl <- cell |> pivot_longer(c(CDU, SPD, Greens, Left, AfD), names_to = "party", values_to = "pct") |>
  mutate(party = factor(party, levels = names(party_cols)), cellname = paste0(ew_lab, ", ", ring_lab),
         cellname = factor(cellname, levels = c("East Berlin, inside the Ring", "West Berlin, inside the Ring", "East Berlin, outside the Ring", "West Berlin, outside the Ring")))
p21 <- ggplot(cl, aes(pct, fct_rev(cellname), colour = party)) +
  geom_line(aes(group = cellname), colour = "#dddddd", linewidth = 3) + geom_point(size = 6) +
  ggrepel::geom_text_repel(aes(label = round(pct)), direction = "x", nudge_y = .32, segment.colour = NA, family = "Fira Sans", size = 3.5, show.legend = FALSE) +
  scale_colour_manual(values = party_cols) + scale_x_continuous(labels = \(x) paste0(x, "%"), limits = c(0, 45)) +
  labs(title = "The Ring is the new Wall: inside vs. outside matters more than East vs. West",
       subtitle = "Vote shares 2026 in the four combinations of East/West Berlin and inside/outside the S-Bahn-Ring (vote-weighted)", x = NULL, y = NULL) +
  theme_sm(15) + theme(panel.grid.major.y = element_blank()) + guides(colour = guide_legend(override.aes = list(size = 4)))
ggsave("../figures/04_berlin_ring.png", p21, width = 12, height = 7, dpi = 200, device = agg_png, bg = "white")

# ---- 3. variance decomposition: East/West vs. distance -----------------------------------
vd <- map_dfr(c("Left", "AfD", "CDU", "Greens", "SPD"), function(p) {
  f <- function(rhs) summary(lm(as.formula(paste(p, "~", rhs)), data = d, weights = valid))$r.squared
  tibble(party = p, `East/West only` = f("east"), `distance only` = f("log(dist_km)"), `inside Ring only` = f("in_ring"),
         `East/West + distance` = f("east + log(dist_km)"))
})
print(vd |> mutate(across(where(is.numeric), ~ round(100 * ., 0))))
write_csv(vd, "../data/output/tables/berlin_variance_decomposition.csv")

# same for 2023 and for the left bloc: how did the East-West gap evolve?
gap <- d |> mutate(left26 = SPD + Greens + Left, left23 = SPD23 + Greens23 + Left23) |>
  group_by(ew_lab) |> summarise(left26 = weighted.mean(left26, valid), left23 = weighted.mean(left23, valid, na.rm = TRUE),
                                afd26 = weighted.mean(AfD, valid), afd23 = weighted.mean(AfD23, valid, na.rm = TRUE), .groups = "drop")
print(gap)

# ---- 4. map: Linke and AfD 2026 with the Ring ---------------------------------------------
m <- g |> left_join(d |> select(bwb, Left, AfD, CDU), by = "bwb")
ring_line <- st_cast(ring, "MULTILINESTRING")
map_one <- function(var, title, hi, lims) {
  ggplot() + geom_sf(data = m, aes(fill = .data[[var]]), colour = NA) +
    geom_sf(data = ring_line, colour = "#000000", linewidth = .7, linetype = "22") +
    scale_fill_gradient(low = "#f7f7f7", high = hi, limits = lims, oob = scales::squish, na.value = "#e0e0e0", labels = \(x) paste0(x, "%"), name = NULL) +
    labs(title = title) + theme_void(base_family = "Fira Sans") +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = .5), legend.position = "bottom", legend.key.width = unit(1.4, "cm"), legend.key.height = unit(.3, "cm"))
}
p22 <- map_one("Left", "Die Linke, 2026", "#BE3075", c(0, 50)) + map_one("AfD", "AfD, 2026", "#009EE0", c(0, 40)) + map_one("CDU", "CDU, 2026", "#000000", c(0, 40)) +
  plot_annotation(title = "Berlin 2026 by postal-vote district. Dashed line: the S-Bahn-Ring",
                  theme = theme(plot.title = element_text(face = "bold", size = 18, family = "Fira Sans"),
                                plot.caption = element_text(colour = "#888888", size = 9, hjust = 0, family = "Fira Sans"), plot.background = element_rect(fill = "white", colour = NA)))
ggsave("../figures/04_berlin_map.png", p22, width = 12, height = 7, dpi = 200, device = agg_png, bg = "white")

cat("done\n")
