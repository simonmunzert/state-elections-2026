###########################################################
# 01: Finding 1. The centre is not cracking (it has been eroding for 40 years)
# The 2026 results at a glance, CDU/CSU + SPD combined share, effective number of
# parliamentary parties, swings 2026, plurality winners by decade.
# Output: figures 01_*.png, tables in ../data/output/tables/
###########################################################

source("_theme.R")
d <- load_state_elections()

###########################################################
# 6. The 2026 state elections at a glance
###########################################################
r26 <- d |> filter(year == 2026, !is.na(pct)) |>
  mutate(family = if_else(family %in% names(family_cols), family, "Other")) |>
  group_by(name_en, date, family) |> summarise(pct = sum(pct), seats = sum(seats, na.rm = TRUE), .groups = "drop") |>
  mutate(family = factor(family, levels = names(family_cols)),
         panel = paste0(name_en, "\n", format(date, "%e %B %Y")),
         panel = fct_reorder(panel, as.numeric(date)))
p7 <- ggplot(r26, aes(x = pct, y = fct_rev(family), fill = family)) +
  geom_col(width = .7) +
  geom_vline(xintercept = 5, colour = "#999999", linetype = "dotted") +
  geom_text(aes(label = sprintf("%.1f", pct)), hjust = -.15, family = "Fira Sans", size = 3.5) +
  facet_wrap(~ panel, nrow = 1) +
  scale_fill_manual(values = family_cols, guide = "none") +
  scale_x_continuous(limits = c(0, 52), expand = c(0, 0)) +
  labs(title = "Five state elections in 2026: five different outcomes",
       subtitle = "Vote shares in %. Dotted line: 5% threshold.", x = NULL, y = NULL) +
  theme_sm(14) + theme(panel.grid.major.y = element_blank(), strip.text = element_text(face = "bold", hjust = 0))
save_fig(p7, "01_results_2026.png", 12, 7)


###########################################################
# 2. Is the centre cracking? CDU/CSU + SPD combined share
###########################################################
centre <- d |> filter(party %in% c("cdu", "spd")) |>
  group_by(land, name_en, elec_no, year, date, region, east) |>
  summarise(centre = sum(pct, na.rm = TRUE), .groups = "drop") |>
  filter(centre > 0)
write_csv(centre, "../data/output/tables/centre_share_by_election.csv")
lab26 <- centre |> filter(year == 2026) |> mutate(lab = paste0(name_en, " ", round(centre), "%"))
p3 <- ggplot(centre, aes(date, centre, colour = region)) +
  geom_point(alpha = .55, size = 2) +
  geom_smooth(method = "loess", span = .5, se = FALSE, linewidth = 1.4, formula = y ~ x) +
  geom_text_repel(data = lab26, aes(label = lab), family = "Fira Sans", size = 3.6, nudge_x = 800, direction = "y", hjust = 0, segment.colour = "#999999", show.legend = FALSE) +
  scale_colour_manual(values = reg_cols) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100), labels = \(x) paste0(x, "%")) +
  scale_x_date(breaks = decade_breaks, date_labels = "%Y", limits = as.Date(c("1946-01-01", "2033-01-01"))) +
  labs(title = "The centre is not cracking. It has been eroding for 40 years.",
       subtitle = "Combined vote share of CDU/CSU and SPD at every state election, 1946-2026", x = NULL, y = NULL) +
  theme_sm()
save_fig(p3, "01_centre_share.png", 12, 7)

centre_decade <- centre |> mutate(decade = floor(year / 10) * 10) |> group_by(region, decade) |>
  summarise(mean_centre = round(mean(centre), 1), n = n(), .groups = "drop")
print(centre_decade |> pivot_wider(names_from = region, values_from = c(mean_centre, n)))
write_csv(centre_decade, "../data/output/tables/centre_share_by_decade.csv")


###########################################################
# 3. Fragmentation: effective number of parliamentary parties
###########################################################
enp <- d |> filter(!is.na(seats), seats > 0, party != "oth") |>
  group_by(land, name_en, elec_no, year, date, region) |>
  summarise(enp = 1 / sum((seats / sum(seats))^2), n_parties = n(), .groups = "drop")
write_csv(enp, "../data/output/tables/enp_by_election.csv")
p4 <- ggplot(enp, aes(date, enp, colour = region)) +
  geom_point(alpha = .55, size = 2) +
  geom_smooth(method = "loess", span = .5, se = FALSE, linewidth = 1.4, formula = y ~ x) +
  geom_text_repel(data = enp |> filter(year == 2026), aes(label = paste0(name_en, " ", round(enp, 1))),
                  family = "Fira Sans", size = 3.6, nudge_x = 800, direction = "y", hjust = 0, segment.colour = "#999999", show.legend = FALSE) +
  scale_colour_manual(values = reg_cols) +
  scale_x_date(breaks = decade_breaks, date_labels = "%Y", limits = as.Date(c("1946-01-01", "2033-01-01"))) +
  scale_y_continuous(breaks = 1:7) +
  labs(title = "State parliaments are more fragmented than ever",
       subtitle = "Effective number of parliamentary parties (Laakso-Taagepera, seat-based) at every state election", x = NULL, y = NULL) +
  theme_sm()
save_fig(p4, "01_fragmentation_enp.png", 12, 7)


###########################################################
# 7. Swing vs previous election, 2026
###########################################################
swing <- d |> filter(party %in% c("cdu", "spd", "afd", "gru", "lin", "fdp", "bsw")) |>
  arrange(land, party, elec_no) |> group_by(land, party) |>
  mutate(pct_prev = lag(pct), swing = pct - pct_prev) |> ungroup() |>
  filter(year == 2026, !is.na(swing)) |>
  mutate(family = if_else(family %in% names(family_cols), family, "Other"),
         family = factor(family, levels = names(family_cols)),
         panel = fct_reorder(paste0(name_en, "\n", format(date, "%e %B %Y")), as.numeric(date)))
write_csv(swing |> select(name_en, date, party, pct, pct_prev, swing), "../data/output/tables/swing_2026.csv")
p8 <- ggplot(swing, aes(x = swing, y = fct_rev(family), fill = family)) +
  geom_col(width = .7) + geom_vline(xintercept = 0, colour = "#333333") +
  geom_text(aes(label = sprintf("%+.1f", swing), hjust = if_else(swing >= 0, -.15, 1.15)), family = "Fira Sans", size = 3.5) +
  facet_wrap(~ panel, nrow = 1) +
  scale_fill_manual(values = family_cols, guide = "none") +
  scale_x_continuous(limits = c(-27, 27)) +
  labs(title = "Change vs. previous state election (percentage points), 2026", x = NULL, y = NULL) +
  theme_sm(14) + theme(panel.grid.major.y = element_blank(), strip.text = element_text(face = "bold", hjust = 0))
save_fig(p8, "01_swing_2026.png", 15, 5.5)


###########################################################
# 8. Seat shares: how close to a majority? / plurality winners
###########################################################
seat_top <- d |> filter(!is.na(seats), seats > 0) |> mutate(seat_share = seats / total_seats) |>
  filter(far_right) |> arrange(desc(seat_share)) |> select(name_en, year, party, pct, seats, total_seats, seat_share) |> head(10)
print(seat_top); write_csv(seat_top, "../data/output/tables/far_right_top_seat_shares.csv")

winner <- d |> filter(!is.na(pct), party != "oth") |> group_by(land, name_en, elec_no, year, region) |>
  slice_max(pct, n = 1, with_ties = FALSE) |> ungroup() |>
  mutate(decade = paste0(floor(year / 10) * 10, "s"))
print(winner |> count(decade, family) |> pivot_wider(names_from = family, values_from = n, values_fill = 0))
write_csv(winner |> select(name_en, year, region, party, pct), "../data/output/tables/plurality_winner_by_election.csv")


cat("done\n")
