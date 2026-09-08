# 11 descriptives - setting figures.R
#
# Descriptives. Every figure in the Empirical Setting section: worker
# demographics, co-hire set sizes, the origin catchment map, language
# fluency, household composition, risk-sharing transfers, network size,
# generator coverage, the inherited-tie ECDF, tie multiplexity, and the
# layer co-occurrence heatmaps. Also the fifteen-layer question table.
#
# Feeds: Empirical Setting (all fig_setting_* figures; table_generators).
#
# Input:  respondents.parquet, dyads.parquet, gn_edgelist.parquet,
#         Ethiopia admin boundary shapefiles
# Output: 04 latex/figures/fig_setting_*.pdf,
#         04 latex/tables/table_generators.tex, 04 latex/pn/pn_setting.tex
#
# Extra packages: sf and patchwork (loaded below, not in 00 config.R).
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# The three derived datasets, and the plot theme shared by every figure.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "11 descriptives - setting figures.R"

col_green <- "#a8d5a2"
col_purple <- "#e0b0FF"
col_wine <- "#722F37"
fig_alpha <- 0.7
theme_set(theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom"))

resp <- read_parquet(file.path(dir_derived, "respondents.parquet")) %>%
  filter(!in_problem)
d <- read_parquet(file.path(dir_derived, "dyads.parquet"))
gn <- read_parquet(file.path(dir_derived, "gn_edgelist.parquet"))

# Household-composition columns, needed only here.
extra <- read_baseline_cols(c("SubmissionDate", "worker_id", "consent_survey",
  "hh_spouse", "hh_children", "hh_parents", "hh_siblings", "hh_othfamily",
  "hh_workers_fam", "hh_workers_nonfam", "hh_othnnfamily")) %>%
  dedup_respondents() %>%
  select(-SubmissionDate, -consent_survey)
resp <- resp %>%
  left_join(extra, by = "worker_id") %>%
  mutate(rural_grp = ifelse(coalesce(rural_share, 0) > 0.5,
                            "Majority-rural", "Urban background"))
cat("analysis respondents:", nrow(resp), "| majority-rural:",
    sum(resp$rural_grp == "Majority-rural"), "\n")

pn_set <- list()
fill_rural <- scale_fill_manual(values = c("Majority-rural" = col_purple,
                                           "Urban background" = col_green),
                                name = NULL)

dem <- resp %>%
  group_by(rural_grp) %>%
  summarise(
    `Female (%)` = 100 * mean(resp_gender == "2"),
    `Age (years)` = mean(resp_age_n, na.rm = TRUE),
    `Married (%)` = 100 * mean(married),
    `Grade 10 or above (%)` = 100 * mean(educ_grade10, na.rm = TRUE),
    `Co-residents (n)` = mean(hh_size_n, na.rm = TRUE),
    `Years in Hawassa` = mean(yrs_hawassa, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(-rural_grp, names_to = "stat", values_to = "v") %>%
  mutate(stat = factor(stat, levels = unique(stat)))

### 2. FIGURES -------------------------------------------------------
# Each block below builds one Setting figure and saves it. Order:
# demographics, co-hire sizes, catchment map, language, housing,
# transfers, network size, generator coverage, inherited-tie ECDF,
# multiplexity, and layer co-occurrence.

# --- Demographics by rural / urban background ---
p <- ggplot(dem, aes(rural_grp, v, fill = rural_grp)) +
  geom_col(width = 0.65, alpha = fig_alpha) +
  geom_text(aes(label = sprintf("%.1f", v)), vjust = -0.4, size = 3) +
  facet_wrap(~stat, scales = "free_y", nrow = 2) +
  fill_rural +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_blank())
save_fig("fig_setting_demographics", p, w = 7.5, h = 4.6)

# --- Co-hire set size, full roster vs surveyed sample ---
ros <- read_csv(file_roster, show_col_types = FALSE)
sz <- bind_rows(
  ros %>% transmute(n = nb_cohires,
                    who = sprintf("All rostered workers (n = %s)",
                                  format(nrow(ros), big.mark = ","))),
  d %>% distinct(worker_id, .keep_all = TRUE) %>%
    transmute(n = nb_cohires_n_i, who = "Surveyed analysis sample"))
med <- sz %>% group_by(who) %>% summarise(m = median(n, na.rm = TRUE))
p <- ggplot(sz, aes(n, fill = who)) +
  geom_histogram(binwidth = 5, boundary = 0, color = "white",
                 linewidth = 0.2, alpha = fig_alpha) +
  geom_vline(data = med, aes(xintercept = m), linetype = "dashed",
             color = "grey30", linewidth = 0.4) +
  geom_text(data = med, aes(x = m, y = Inf, label = paste("median", m)),
            vjust = 1.6, hjust = -0.1, size = 3, color = "grey30") +
  facet_wrap(~who, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(col_purple, col_green), guide = "none") +
  labs(x = "Number of co-hires on the worker's roster", y = "Workers")
save_fig("fig_setting_cohires", p, w = 7, h = 4.6)

# --- Origin catchment map (worker birth woredas over southern Ethiopia) ---
library(sf)
gadm <- file.path(dir_data, "eth_admin_boundaries")
adm2 <- st_read(file.path(gadm, "eth_admin2_ocha.shp"), quiet = TRUE)
adm3 <- st_read(file.path(gadm, "eth_admin3_ocha.shp"), quiet = TRUE)
hip <- tibble(lon = 38.468, lat = 7.055)

# Survey woreda label -> admin-3 polygon name (NA where there is no polygon).
woreda_lookup <- tribble(
  ~survey_woreda,          ~adm3_name,
  "Hawela Tula",           "Hawassa town",
  "Tabor Sub-city",        "Hawassa town",
  "Bahil Adarash Sub-city", "Hawassa town",
  "Misrak Sub-city",       "Hawassa town",
  "Hawassa Zuria",         "Hawassa Zuria",
  "Hawela",                "Hawela",
  "Shebedino",             "Shebe Dino",
  "Borricha",              "Boricha",
  "Leku Town Admin",       "Leku town",
  "Bilate Zuria",          "Bilate Zuria",
  "Malga",                 "Malga",
  "Gorche",                "Gorche",
  "Wondo Genet",           "Wondo-Genet",
  "Harbagona",             "Arbegona",
  "Dalle",                 "Dale",
  "Darara",                "Darara",
  "Yirgalem Town Admin",   "Yirgalem town",
  "Lokka Abayya",          "Loka Abaya",
  "Wonsho",                "Wonosho",
  "Aleta Wondo",           "Aleta Wendo",
  "Aleta Cukko",           "Aleta Chuko",
  "Bursa",                 "Bursa",
  "Hula",                  "Hulla",
  "Xexicha",               "Teticha",
  "Cirone",                "Chirone",
  "Bansa",                 "Bensa",
  "Bona Zuria",            "Bona Zuria",
  "Hookko",                "Hokko",
  "Cirre",                 "Chire",
  "Cabbe Gambelto",        "Chabe Gambeltu",
  "Burra",                 "Bura",
  "Dayye Town Admin",      "Daye town",
  "Duguna Fango",          "Duguna Fango",
  "Sodo Town Admin",       "Sodo town",
  "Sodo Zuria",            "Sodo Zuria",
  "Humbo",                 "Humbo",
  "Kindo Kosha",           "Kindo Koyesha",
  "Areka Town Admin",      "Areka town",
  "Damot Weyde",           "Damot Woide",
  "Damot Pulasa",          "Damot Pullasa",
  "Boloso Bombe",          "Boloso Bombe",
  "Boloso Sore",           "Boloso Sore",
  "Bodit Town Admin",      "Boditi town",
  "Offa",                  "Ofa",
  "Durame Town",           "Durame town",
  "Gibe",                  "Gibe",
  "Kemba",                 "Kemba Zuria",
  "Oyda",                  "O'yida",
  "Bulki Town",            "Geze Gofa",
  "Adola",                 "Adola",
  "Shashemene",            "Shashemene",
  "Nekemte",               "Nekemte town",
  "Kobo",                  "Kobo town",
  "Hororessa",             NA,
  "Bochore",               NA)
lab_wor <- get_choice_labels("woredas")
counts <- resp %>%
  filter(is_valid_code(birth_woreda)) %>%
  mutate(wlab = lab_wor$label[match(birth_woreda, lab_wor$code)]) %>%
  count(wlab) %>%
  left_join(woreda_lookup, by = c("wlab" = "survey_woreda"))
counts <- counts %>%
  mutate(adm3_name = coalesce(adm3_name,
    ifelse(wlab %in% adm3$adm3_name, wlab, NA)))
unmatched <- counts %>% filter(is.na(adm3_name))
if (nrow(unmatched) > 0) {
  cat("NOTE: woreda labels not on map (grey), n =", sum(unmatched$n),
      "workers:\n")
  print(unmatched, n = 30)
}
adm3_counts <- counts %>%
  filter(!is.na(adm3_name)) %>%
  group_by(adm3_name) %>%
  summarise(n = sum(n), .groups = "drop")
map3 <- adm3 %>% left_join(adm3_counts, by = "adm3_name")
p <- ggplot() +
  geom_sf(data = map3, aes(fill = n), color = "white", linewidth = 0.08,
          alpha = fig_alpha) +
  geom_sf(data = adm2, fill = NA, color = "grey40", linewidth = 0.3) +
  geom_point(data = hip, aes(lon, lat), shape = 23, fill = "black",
             color = "white", size = 3) +
  annotate("text", x = 38.75, y = 7.1, label = "HIP", size = 3,
           fontface = "bold") +
  scale_fill_gradient(low = col_purple, high = col_wine, na.value = "grey93",
                      name = "Workers") +
  coord_sf(xlim = c(36.3, 39.6), ylim = c(5.6, 8.2)) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(), panel.grid = element_blank())
save_fig("fig_setting_catchment", p, w = 6.5, h = 5.8)

lab_reg <- get_choice_labels("regions")
lab_zon <- get_choice_labels("zones")
pn_set$BornSidamaPct <- pn_pct(mean(
  resp$birth_region == code_for("Sidama", lab_reg), na.rm = TRUE))
pn_set$BornWolaytaPct <- pn_pct(mean(
  resp$birth_zone == code_for("Wolayta", lab_zon), na.rm = TRUE))

# --- Language fluency (Sidamigna and Amharic) ---
lang <- bind_rows(
  resp %>% count(level = sid_speak_n) %>% mutate(lang = "Sidamigna"),
  resp %>% count(level = amh_speak_n) %>% mutate(lang = "Amharic")) %>%
  filter(!is.na(level)) %>%
  group_by(lang) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  mutate(lab = factor(c("Not at all", "A little", "Moderately", "Well",
                        "Very well")[level],
                      levels = c("Not at all", "A little", "Moderately",
                                 "Well", "Very well")))
p <- ggplot(lang, aes(lab, share, fill = lang)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65,
           alpha = fig_alpha) +
  scale_fill_manual(values = c(Sidamigna = col_purple, Amharic = col_green),
                    name = NULL) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "How well can you speak ...?", y = "Share of workers")
save_fig("fig_setting_language", p, w = 6.8, h = 3.8)

pn_set$SidamaSpeakPct <- pn_pct(mean(resp$sid_speak_n == 5, na.rm = TRUE))
pn_set$AmharicSpeakPct <- pn_pct(mean(resp$amh_speak_n >= 4, na.rm = TRUE))

# --- Household composition: who the worker lives with ---
hh_types <- c(
  hh_workers_nonfam = "HIP co-workers (non-family)",
  hh_workers_fam = "HIP co-workers (family)",
  hh_othnnfamily = "Other non-family",
  hh_othfamily = "Other family",
  hh_siblings = "Siblings",
  hh_spouse = "Spouse/partner",
  hh_children = "Children",
  hh_parents = "Parents")
hh <- resp %>%
  mutate(across(all_of(names(hh_types)),
                ~replace_na(suppressWarnings(as.integer(.)), 0L))) %>%
  pivot_longer(all_of(names(hh_types)), names_to = "type", values_to = "k") %>%
  group_by(rural_grp, type) %>%
  summarise(share = mean(k > 0), .groups = "drop") %>%
  mutate(type = hh_types[type])
p <- ggplot(hh, aes(share, reorder(type, share), fill = rural_grp)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7,
           alpha = fig_alpha) +
  fill_rural +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Share living with at least one person of this type", y = NULL)
save_fig("fig_setting_housing", p, w = 7, h = 4.2)

pn_set$LiveCoworkerPct <- pn_pct(mean(
  replace_na(suppressWarnings(as.integer(resp$hh_workers_nonfam)), 0L) +
  replace_na(suppressWarnings(as.integer(resp$hh_workers_fam)), 0L) > 0))

# --- Risk-sharing transfers received / provided, by counterpart group ---
tr_cols <- c("SubmissionDate", "worker_id", "consent_survey",
  paste0("_rs_cur_grp_", 1:6),
  paste0("rs_recv_freq_bs_", 1:6), paste0("rs_recv_freq_bf_", 1:6),
  paste0("rs_recv_val_bs_", 1:6), paste0("rs_recv_val_bf_", 1:6),
  paste0("rs_prov_freq_as_", 1:6), paste0("rs_prov_freq_af_", 1:6),
  paste0("rs_prov_val_as_", 1:6), paste0("rs_prov_val_af_", 1:6))
tr <- read_baseline_cols(tr_cols) %>%
  dedup_respondents() %>%
  filter(worker_id %in% resp$worker_id)
grp_labels <- c("1" = "Co-hires (same week)", "2" = "Other HIP workers",
  "3" = "Home village", "4" = "Household (non-HIP)", "5" = "Other non-HIP",
  "6" = "Other non-HIP")
num <- function(x) suppressWarnings(as.numeric(x))
tl <- lapply(1:6, function(k) {
  g <- tr[[paste0("_rs_cur_grp_", k)]]
  if (is.null(g)) return(NULL)
  tibble(worker_id = tr$worker_id, grp = g,
    recv_freq = num(coalesce(na_if(tr[[paste0("rs_recv_freq_bs_", k)]], ""),
                             na_if(tr[[paste0("rs_recv_freq_bf_", k)]], ""))),
    recv_val = num(coalesce(na_if(tr[[paste0("rs_recv_val_bs_", k)]], ""),
                            na_if(tr[[paste0("rs_recv_val_bf_", k)]], ""))),
    prov_freq = num(coalesce(na_if(tr[[paste0("rs_prov_freq_as_", k)]], ""),
                             na_if(tr[[paste0("rs_prov_freq_af_", k)]], ""))),
    prov_val = num(coalesce(na_if(tr[[paste0("rs_prov_val_as_", k)]], ""),
                            na_if(tr[[paste0("rs_prov_val_af_", k)]], ""))))
}) %>%
  bind_rows() %>%
  filter(!is.na(grp), grp != "") %>%
  mutate(grp_lab = grp_labels[grp],
         across(c(recv_val, prov_val), ~ifelse(. %in% c(-999, -888), NA, .)),
         across(c(recv_freq, prov_freq),
                ~ifelse(. %in% c(-999, -888), NA, .)))
cat("transfer rows:", nrow(tl), "over", n_distinct(tl$worker_id),
    "workers\n")
tsum <- tl %>%
  group_by(grp_lab) %>%
  summarise(
    `Receives` = mean(recv_freq != 6, na.rm = TRUE),
    `Provides` = mean(prov_freq != 6, na.rm = TRUE),
    recv_mean = mean(recv_val[recv_freq != 6], na.rm = TRUE),
    prov_mean = mean(prov_val[prov_freq != 6], na.rm = TRUE),
    .groups = "drop")
grp_order <- tsum %>% arrange(Receives) %>% pull(grp_lab)
tsum <- tsum %>% mutate(grp_lab = factor(grp_lab, levels = grp_order))
p1 <- tsum %>%
  pivot_longer(c(Receives, Provides)) %>%
  ggplot(aes(value, grp_lab, fill = name)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7,
           alpha = fig_alpha) +
  scale_fill_manual(values = c(Receives = col_purple, Provides = col_green),
                    name = NULL) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Share with any transfer, past 3 months", y = NULL)
p2 <- tsum %>%
  pivot_longer(c(recv_mean, prov_mean)) %>%
  mutate(name = ifelse(name == "recv_mean", "Receives", "Provides")) %>%
  ggplot(aes(value, grp_lab, fill = name)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7,
           alpha = fig_alpha) +
  scale_fill_manual(values = c(Receives = col_purple, Provides = col_green),
                    name = NULL) +
  labs(x = "Mean ETB conditional on any transfer", y = NULL) +
  theme(axis.text.y = element_blank())

library(patchwork)

lg_transfers <- theme(legend.position = "bottom",
                      legend.justification = "center",
                      legend.text = element_text(size = 13),
                      legend.key.size = grid::unit(0.7, "cm"))
save_fig("fig_setting_transfers",
         (p1 + lg_transfers) + (p2 + lg_transfers) +
           plot_layout(guides = "collect") +
           plot_annotation(theme = lg_transfers),
         w = 8.5, h = 3.8)

# --- Network size: co-hire ties and general-network alters ---
deg <- resp %>%
  select(worker_id) %>%
  left_join(d %>% group_by(worker_id) %>%
              summarise(coh = sum(tied_any), .groups = "drop"),
            by = "worker_id") %>%
  left_join(gn %>% count(worker_id, name = "gnn"), by = "worker_id") %>%
  mutate(coh = replace_na(coh, 0L), gnn = replace_na(gnn, 0L),
         tot = coh + gnn)

degl <- bind_rows(
  deg %>% count(k = pmin(gnn, 15)) %>% mutate(type = "General network"),
  deg %>% count(k = pmin(coh, 15)) %>% mutate(type = "Co-hires")) %>%
  complete(type, k = 0:15, fill = list(n = 0L))

degt <- deg %>%
  count(k = pmin(tot, 15)) %>%
  complete(k = 0:15, fill = list(n = 0L))
med_tot <- median(deg$tot)
scale_deg_x <- scale_x_continuous(breaks = seq(0, 15, 3),
                                  labels = c(seq(0, 12, 3), "15+"))

deg_ymax <- max(degl$n, degt$n)
scale_deg_y <- scale_y_continuous(limits = c(0, deg_ymax),
                                  expand = expansion(mult = c(0, 0.05)))
pa <- ggplot(degl, aes(k, n, fill = type)) +
  geom_col(position = position_dodge(width = 0.9, preserve = "single"),
           width = 0.85, alpha = fig_alpha) +
  scale_fill_manual(values = c(`Co-hires` = col_green,
    `General network` = col_purple), name = NULL) +
  scale_deg_x + scale_deg_y +
  labs(subtitle = "(a) By alter type", x = "Named network members",
       y = "Workers")

pb <- ggplot(degt, aes(k, n)) +
  geom_col(fill = col_wine, width = 0.85, alpha = fig_alpha) +
  geom_vline(xintercept = med_tot, linetype = "dashed", color = "grey40") +
  annotate("text", x = med_tot + 0.3, y = Inf, vjust = 1.6, hjust = 0,
           label = paste0("median = ", med_tot), size = 3.1,
           color = "grey40") +
  scale_deg_x + scale_deg_y +
  labs(subtitle = "(b) All alters", x = "Named network members", y = NULL)
save_fig("fig_setting_network_size", pa + pb, w = 9, h = 3.8)

pn_set$MedNetworkWord <- pn_word(median(deg$tot))
pn_set$MeanCohireTies <- pn_num(mean(deg$coh), 1)

# --- Generator coverage: share naming at least one co-hire per generator ---
egos_with_roster <- d %>% distinct(worker_id) %>% nrow()
cov <- lapply(1:12, function(g) {
  x <- d %>%
    group_by(worker_id) %>%
    summarise(any = as.integer(sum(.data[[paste0("gen_", g)]]) > 0),
              cnt = sum(.data[[paste0("gen_", g)]]), .groups = "drop")
  tibble(gen = g, label = generator_labels[as.character(g)],
         p_any = mean(x$any), mean_cnt = mean(x$cnt))
}) %>% bind_rows()

p <- ggplot(cov, aes(p_any, reorder(label, p_any))) +
  geom_col(fill = col_purple, width = 0.7, alpha = fig_alpha) +
  geom_text(aes(label = sprintf("mean = %.2f", mean_cnt)),
            hjust = -0.08, size = 2.8, color = "grey25") +
  scale_x_continuous(labels = scales::percent,
                     expand = expansion(mult = c(0, 0.25))) +
  labs(x = "Share of workers naming at least one co-hire", y = NULL)
save_fig("fig_setting_generators", p, w = 7, h = 4.4)

# --- Inherited ties: ECDF of the share of alters known before HIP ---
inh <- resp %>%
  select(worker_id, rural_grp) %>%
  left_join(d %>% filter(tied_any == 1) %>% group_by(worker_id) %>%
              summarise(c_pre = sum(tied_pre),
                        c_known = sum(tied_pre + tied_post), .groups = "drop"),
            by = "worker_id") %>%
  left_join(gn %>% mutate(pre = as.integer(pre_hip == "1")) %>%
              group_by(worker_id) %>%
              summarise(g_pre = sum(pre, na.rm = TRUE),
                        g_known = sum(!is.na(pre)), .groups = "drop"),
            by = "worker_id") %>%
  mutate(across(c(c_pre, c_known, g_pre, g_known), ~replace_na(., 0L)),
         known = c_known + g_known, share_pre = (c_pre + g_pre) / known) %>%
  filter(known > 0)
p <- ggplot(inh, aes(share_pre, color = rural_grp)) +
  stat_ecdf(linewidth = 0.9) +
  scale_color_manual(values = c(`Majority-rural` = col_purple,
                                `Urban background` = col_green), name = NULL) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Share of named alters known before joining HIP",
       y = "Cumulative share of workers")
save_fig("fig_setting_inherited", p, w = 6.8, h = 3.9)

pn_set$PreHipSharePct <- pn_pct(mean(inh$share_pre))
pn_set$PreHipHalfPct  <- pn_pct(mean(inh$share_pre >= 0.5))

# --- Multiplexity: how many layers a tie appears in ---
mux <- bind_rows(
  d %>% filter(tied_any == 1) %>% transmute(k = pmin(n_layers, 8),
                                            type = "Co-hire ties"),
  gn %>% transmute(k = pmin(n_layers, 8), type = "General network")) %>%
  count(type, k) %>%
  group_by(type) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  complete(type, k = 1:8, fill = list(n = 0L, share = 0))
p <- ggplot(mux, aes(k, share, fill = type)) +
  geom_col(position = position_dodge(width = 0.8, preserve = "single"),
           width = 0.75, alpha = fig_alpha) +
  scale_fill_manual(values = c(`Co-hire ties` = col_green,
                               `General network` = col_purple), name = NULL) +
  scale_x_continuous(breaks = 1:8, labels = c(1:7, "8+")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Number of relationship layers the tie appears in",
       y = "Share of ties")
save_fig("fig_setting_multiplex", p, w = 6.8, h = 3.8)

mux_share <- function(x, k) pn_pct(mean(x >= k))
pn_set$MuxCohireTwoPct <- mux_share(d$n_layers[d$tied_any == 1], 2)
pn_set$MuxCohireSixPct <- mux_share(d$n_layers[d$tied_any == 1], 6)
pn_set$MuxGnTwoPct     <- mux_share(gn$n_layers, 2)
pn_set$MuxGnSixPct     <- mux_share(gn$n_layers, 6)
cat("\nmedian layers per tie: co-hire",
    median(d$n_layers[d$tied_any == 1]), "| GN", median(gn$n_layers), "\n")
cat("median generators only:  co-hire",
    median(d$n_generators[d$tied_any == 1]), "| GN",
    median(gn$n_generators), "\n")

# --- Layer co-occurrence heatmaps (co-hire ties vs general network) ---
layer_short <- c("advice", "borrow (in)", "large exp (in)", "job search",
                 "navigate", "commute", "meet outside", "call/text", "breaks",
                 "friends", "assist (in)", "rosca",
                 "borrow (out)", "large exp (out)", "assist (out)")

layer_matrix <- function(tbl) {
  M <- as.matrix(tbl[, layer_cols])
  storage.mode(M) <- "numeric"
  M[M > 0] <- 1
  colnames(M) <- layer_short
  M
}

cooc_mats <- function(M) {
  both <- crossprod(M)
  marg <- diag(both)
  cond <- both / marg[row(both)]
  jac <- both / (outer(marg, marg, "+") - both)
  list(cond = cond, jac = jac, marg = marg, n = nrow(M))
}

layer_order <- function(cc) {
  hc <- hclust(as.dist(1 - cc$jac), method = "average")
  colnames(cc$jac)[hc$order]
}

cooc_plot <- function(cc, ord, what, ttl, lims) {
  m <- cc[[what]]
  diag(m) <- NA
  df <- as_tibble(as.table(m), .name_repair = "unique")
  names(df) <- c("row_lab", "col_lab", "v")
  df <- df %>% mutate(row_lab = factor(row_lab, levels = rev(ord)),
                      col_lab = factor(col_lab, levels = ord))
  ggplot(df, aes(col_lab, row_lab, fill = v)) +
    geom_tile(colour = "white", linewidth = 0.3) +
    scale_fill_gradient(low = "white", high = col_wine, na.value = "grey92",
                        limits = lims,
                        name = if (what == "cond")
                          "P(column layer | row layer)" else "Jaccard overlap") +
    coord_fixed() +
    labs(title = sprintf("%s (%d ties)", ttl, cc$n), x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5.6),
          axis.text.y = element_text(size = 5.6),
          panel.grid = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5),
          legend.position = "bottom",
          legend.key.height = unit(0.3, "cm"),
          legend.key.width = unit(0.8, "cm"),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 6))
}

cc_coh <- cooc_mats(layer_matrix(d %>% filter(tied_any == 1)))
cc_gn <- cooc_mats(layer_matrix(gn))
ord <- layer_order(cc_coh)

for (what in "cond") {
  offdiag <- function(m) {
    diag(m) <- NA
    m
  }
  lims <- c(0, max(offdiag(cc_coh[[what]]), offdiag(cc_gn[[what]]),
                   na.rm = TRUE))
  save_fig(paste0("fig_setting_layer_cooc_", what),
           cooc_plot(cc_coh, ord, what, "Co-hire ties", lims) +
             cooc_plot(cc_gn, ord, what, "General-network ties", lims) +
             patchwork::plot_layout(guides = "collect"),
           w = 10, h = 5.4)
}

# Companionship-block and outflow-block conditional probabilities.
comp_set <- c("commute", "breaks", "friends", "meet outside", "call/text")
ins_set <- c("borrow (out)", "large exp (out)", "assist (out)")
blk <- function(cc, a, b) {
  v <- cc$cond[a, b, drop = FALSE]
  if (identical(a, b)) v <- v[lower.tri(v) | upper.tri(v)]
  mean(v, na.rm = TRUE)
}

pn_prob <- function(x) pn_num(x, if (x < 0.1) 3 else 2)
for (nm in c("co-hire", "GN")) {
  cc <- if (nm == "co-hire") cc_coh else cc_gn
  tag <- if (nm == "co-hire") "Cohire" else "Gn"
  M <- layer_matrix(if (nm == "co-hire") d %>% filter(tied_any == 1) else gn)
  cat("\n", nm, " ties: n = ", cc$n, "\n", sep = "")
  cat("  layer prevalence:\n")
  print(sort(cc$marg, decreasing = TRUE))
  cat("  mean P(other companionship | companionship):",
      round(blk(cc, comp_set, comp_set), 3), "\n")
  cat("  mean P(any given outflow | companionship): ",
      round(blk(cc, comp_set, ins_set), 3), "\n")
  cat("  mean P(other outflow | outflow):           ",
      round(blk(cc, ins_set, ins_set), 3), "\n")
  anyout <- rowSums(M[, ins_set, drop = FALSE]) > 0
  comm <- M[, "commute"] > 0
  cat("  P(any outflow) =", round(mean(anyout), 3),
      "| P(any outflow | commute) =", round(mean(anyout[comm]), 3), "\n")
  pn_set[[paste0("CondComp", tag)]] <- pn_prob(blk(cc, comp_set, comp_set))
  pn_set[[paste0("CondOut", tag)]]  <- pn_prob(blk(cc, comp_set, ins_set))
  pn_set[[paste0("AnyOut", tag, "Pct")]] <- pn_pct(mean(anyout), 1)
  pn_set[[paste0("AnyOutCommute", tag, "Pct")]] <- pn_pct(mean(anyout[comm]), 1)
}

pn_set$ThinLargeExpOut     <- pn_int(cc_coh$marg[["large exp (out)"]])
pn_set$ThinLargeExpOutWord <- pn_word(cc_coh$marg[["large exp (out)"]])
pn_set$ThinRosca           <- pn_int(cc_coh$marg[["rosca"]])
pn_set$ThinLargeExpIn      <- pn_int(cc_coh$marg[["large exp (in)"]])
cat("\nclustered layer order:", paste(ord, collapse = " < "), "\n")

### 3. LATEX ---------------------------------------------------------
# The fifteen-layer question table, with the share of workers naming at
# least one person on each, for co-hires and for the general network.

gen_q <- c(
  "If you had to make a personal decision, whom would you ask for advice?",
  "If you suddenly needed to borrow a small amount of money for a week, whom would you ask? \\emph{(in)}",
  "If you needed help to pay large expenses (medical bills, school fees), whom would you ask? \\emph{(in)}",
  "If you were searching for another job, whom would you ask for advice or references?",
  "If you needed advice to navigate life in Hawassa City, whom would you ask?",
  "With whom do you commute, even if infrequently?",
  "In your free time, whom do you meet outside the workplace?",
  "In your free time, whom do you call or text with?",
  "With whom do you spend time during breaks in the workplace?",
  "Whom do you consider to be your friends?",
  "Who provided you with assistance (financial or in-kind) over the past three months? \\emph{(in)}",
  "Who are you in a ROSCA (ekub) with?")
rs_q <- c(
  "Who would ask you if they need to borrow a small amount of money for a week? \\emph{(out)}",
  "Who would ask you if they need help to pay large expenses, such as medical bills or school fees? \\emph{(out)}",
  "Over the past three months, whom did you provide some assistance (financial or in-kind) to? \\emph{(out)}")

share_any <- function(tbl, col) {
  x <- tbl %>%
    group_by(worker_id) %>%
    summarise(a = as.integer(sum(.data[[col]]) > 0), .groups = "drop") %>%
    right_join(resp["worker_id"], by = "worker_id") %>%
    mutate(a = replace_na(a, 0L))
  mean(x$a)
}
d_tied <- d %>% filter(tied_any == 1)
rs_keys <- c("rs_out_borrow", "rs_out_large_exp", "rs_out_assisted_3m")
coh_share <- c(cov$p_any, sapply(rs_keys, function(k) share_any(d_tied, k)))
gn_share <- c(sapply(1:12, function(g) share_any(gn, paste0("gen_", g))),
              sapply(rs_keys, function(k) share_any(gn, k)))
tab <- tibble(n = 1:15, q = c(gen_q, rs_q),
              coh = sprintf("%.0f\\%%", 100 * coh_share),
              gnp = sprintf("%.0f\\%%", 100 * gn_share))
writeLines(c(
  tex_stamp(),
  "\\begin{table}[h!]\\centering",
  paste0("\\caption{The ", pn_word(length(layer_cols)),
         " relationship layers}\\label{tab:generators}"),
  "\\small\\begin{tabular}{clcc}",
  " & \\textit{question} & \\multicolumn{2}{c}{\\textit{named $\\geq 1$ person}} \\\\",
  "\\cmidrule(lr){3-4}",
  " & & \\textit{co-hires} & \\textit{general network} \\\\",
  "\\dmidrule",
  "\\multicolumn{4}{l}{\\textit{Network generators (whom the worker turns to)}} \\\\[3pt]",
  sprintf("%d & \\parbox[t]{8.6cm}{%s} & %s & %s \\\\[7pt]",
          tab$n[1:12], tab$q[1:12], tab$coh[1:12], tab$gnp[1:12]),
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{Risk sharing (who turns to the worker)}} \\\\[3pt]",
  sprintf("%d & \\parbox[t]{8.6cm}{%s} & %s & %s \\\\[7pt]",
          tab$n[13:15], tab$q[13:15], tab$coh[13:15], tab$gnp[13:15]),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.92\\textwidth}\\vspace{4pt}\\footnotesize",
  "\\emph{Notes:} The twelve generators are asked in randomized order, once over",
  "the preloaded co-hire roster and once as free nominations; the three",
  "risk-sharing questions over the accumulated list of names. Rows marked",
  "\\emph{(in)} and \\emph{(out)} are the same exchange in both directions.",
  "\\end{minipage}",
  "\\end{table}"), file.path(dir_tex, "table_generators.tex"))
cat("wrote table_generators.tex\n")

### 4. MACROS -------------------------------------------------------
# The figures the Setting prose quotes, as \pn macros.

write_pn(pn_set, file.path(dir_pn, "pn_setting.tex"))

cat("\n== CONTEXT, NOT QUOTED IN THE PROSE ==\n")
cat("firms in cases file: 6 (plus 1 JP Garment merged with JP Textile? see cases)\n")
cat("median co-hires, analysis egos:",
    median((d %>% distinct(worker_id, .keep_all = TRUE))$nb_cohires_n_i), "\n")
cat("median co-hires, full roster:", median(ros$nb_cohires, na.rm = TRUE), "\n")
cat("median co-hire ties:", median(deg$coh), "| median GN:",
    median(deg$gnn), "\n")
cat("mean named:", round(mean(deg$tot), 1), "= coh", round(mean(deg$coh), 1),
    "+ gn", round(mean(deg$gnn), 1), "\n")
cat("median generators per co-hire tie:",
    median(d$n_generators[d$tied_any == 1]),
    "| per GN alter:", median(gn$n_generators), "\n")
cat("top generator for co-hires:", cov$label[which.max(cov$p_any)],
    "| lowest:", cov$label[which.min(cov$p_any)], "\n")
