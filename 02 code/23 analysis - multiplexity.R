# 23 analysis - multiplexity.R
#
# Analysis (Specifications 6 and 7). How many layers a tie carries
# (depth), and how that depends on tie vintage, shared origin, language,
# co-residence and kinship. Poisson depth regressions under an ego fixed
# effect (Specification 6), then one linear model per layer for the
# effect of the tie predating HIP (Specification 7).
#
# Feeds: Section 6 Results (Specifications 6 and 7).
#
# Input:  dyads.parquet, gn_edgelist.parquet
# Output: 20 analysis/table_depth_regressions.csv, table_layer_by_vintage.csv,
#         04 latex/tables/table_depth_cohire.tex, table_depth_gn.tex,
#         table_vintage_layers.tex, 04 latex/figures/fig_layer_vintage.pdf,
#         04 latex/pn/pn_mux.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# The two tie-level frames, the regression subsets, and the plot theme.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "23 analysis - multiplexity.R"

col_green <- "#a8d5a2"
col_purple <- "#e0b0FF"
fig_alpha <- 0.7
theme_set(theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom"))

d_all <- read_parquet(file.path(dir_derived, "dyads.parquet"))
gn_all <- read_parquet(file.path(dir_derived, "gn_edgelist.parquet"))

d <- tie_frame_cohire(d_all)
gn <- tie_frame_gn(gn_all)

ego_gn <- gn %>%
  group_by(worker_id) %>%
  summarise(n_ties = n(), depth = mean(n_layers))
ego_ch <- d %>%
  group_by(worker_id) %>%
  summarise(n_ties = n(), depth = mean(n_layers))
cor_gn <- cor(ego_gn$n_ties, ego_gn$depth)
cor_ch <- cor(ego_ch$n_ties, ego_ch$depth)
cat("breadth-depth correlation: co-hire", round(cor_ch, 3),
    "| general network", round(cor_gn, 3), "\n")

d_reg <- d %>% filter(origin_valid, pu_known == 1)
gn_reg <- gn %>% filter(!is.na(same_woreda))
stopifnot(!anyNA(d_reg$shared_lang), !anyNA(d_reg$same_pu),
          !anyNA(gn_reg$co_resident), !anyNA(gn_reg$inherited))

# The vintage-identifying subset: workers with both an inherited and a
# post-HIP tie, so the ego FE can compare them.
vint_set <- function(tbl) {
  w <- tbl %>%
    group_by(worker_id) %>%
    summarise(pre = sum(inherited), post = sum(!inherited), .groups = "drop") %>%
    filter(pre > 0, post > 0)
  tbl %>% filter(worker_id %in% w$worker_id)
}
vs_ch <- vint_set(d_reg)
vs_gn <- vint_set(gn_reg)
cat("\nvintage identifying set -- co-hire:", nrow(vs_ch), "ties over",
    n_distinct(vs_ch$worker_id), "workers of", n_distinct(d_reg$worker_id),
    "| gn:", nrow(vs_gn), "ties over", n_distinct(vs_gn$worker_id),
    "workers of", n_distinct(gn_reg$worker_id), "\n")

### 2. ESTIMATION -----------------------------------------------------
# Poisson depth regressions (with / without ego FE, +kin/friend,
# +origin x vintage interaction), then one linear model per layer for
# the effect of the tie predating HIP.

m1 <- fepois(n_layers ~ inherited + same_woreda + shared_lang + same_pu,
             data = d_reg, cluster = ~ worker_id + alter_id)
m2 <- fepois(n_layers ~ inherited + same_woreda + shared_lang + same_pu |
               worker_id,
             data = d_reg, cluster = ~ worker_id + alter_id, fixef.rm = "both")
m3 <- fepois(n_layers ~ inherited + same_woreda + shared_lang + same_pu +
               kin + friend | worker_id,
             data = d_reg, cluster = ~ worker_id + alter_id, fixef.rm = "both")
m4 <- fepois(n_layers ~ inherited + same_woreda + co_resident,
             data = gn_reg, cluster = ~ worker_id)
m5 <- fepois(n_layers ~ inherited + same_woreda + co_resident | worker_id,
             data = gn_reg, cluster = ~ worker_id, fixef.rm = "both")
m6 <- fepois(n_layers ~ inherited + same_woreda + co_resident + kin + friend |
               worker_id,
             data = gn_reg, cluster = ~ worker_id, fixef.rm = "both")

m3i <- fepois(n_layers ~ inherited * same_woreda + shared_lang + same_pu +
                kin + friend | worker_id,
              data = d_reg, cluster = ~ worker_id + alter_id,
              fixef.rm = "both")
m6i <- fepois(n_layers ~ inherited * same_woreda + co_resident + kin + friend |
                worker_id,
              data = gn_reg, cluster = ~ worker_id, fixef.rm = "both")
cat("\ninteraction cell counts, co-hire: inherited x same-woreda =",
    sum(d_reg$inherited & d_reg$same_woreda), "| inherited x other =",
    sum(d_reg$inherited & !d_reg$same_woreda), "| new x same-woreda =",
    sum(!d_reg$inherited & d_reg$same_woreda), "| new x other =",
    sum(!d_reg$inherited & !d_reg$same_woreda), "\n")
cat("interaction cell counts, general network: inherited x same-woreda =",
    sum(gn_reg$inherited & gn_reg$same_woreda), "| inherited x other =",
    sum(gn_reg$inherited & !gn_reg$same_woreda), "| new x same-woreda =",
    sum(!gn_reg$inherited & gn_reg$same_woreda), "| new x other =",
    sum(!gn_reg$inherited & !gn_reg$same_woreda), "\n")

mods <- list(m1, m2, m3, m3i, m4, m5, m6, m6i)
for (i in seq_along(mods)) {
  cat("\n--- column", i, "---\n")
  print(summary(mods[[i]]))
}

grab <- function(m, term) {
  ct <- as.data.frame(coeftable(m))
  if (!term %in% rownames(ct)) return(c(b = NA, se = NA, p = NA))
  c(b = ct[term, 1], se = ct[term, 2], p = ct[term, 4])
}
reg_terms <- c("inheritedTRUE", "same_woredaTRUE",
               "inheritedTRUE:same_woredaTRUE",
               "shared_lang", "same_pu", "co_residentTRUE", "kinTRUE",
               "friendTRUE")
reg_tbl <- do.call(rbind, lapply(seq_along(mods), function(i)
  data.frame(column = i, term = reg_terms,
             t(sapply(reg_terms, function(tm) grab(mods[[i]], tm))),
             row.names = NULL)))
reg_tbl$n <- rep(sapply(mods, nobs), each = length(reg_terms))
write.csv(reg_tbl, file.path(dir_analysis, "table_depth_regressions.csv"),
          row.names = FALSE)

# One linear model per layer: effect of the tie predating HIP on carrying it.
layer_fit <- function(tbl, roster) {
  do.call(rbind, lapply(seq_along(layer_cols), function(k) {
    tbl$y <- as.numeric(tbl[[layer_cols[k]]] > 0)
    m <- feols(y ~ inherited | worker_id, data = tbl, cluster = ~ worker_id,
               fixef.rm = "both")
    ct <- as.data.frame(coeftable(m))
    data.frame(roster = roster, layer = layer_labs[k],
               prevalence = mean(tbl$y),
               b = ct["inheritedTRUE", 1], se = ct["inheritedTRUE", 2],
               p = ct["inheritedTRUE", 4], n = nobs(m))
  }))
}
lay <- rbind(layer_fit(d, "Co-hire ties"), layer_fit(gn, "General network"))
lay <- lay %>% mutate(lo = b - 1.96 * se, hi = b + 1.96 * se)
write.csv(lay, file.path(dir_analysis, "table_layer_by_vintage.csv"),
          row.names = FALSE)
cat("\nper-layer vintage coefficients:\n")
print(as.data.frame(lay), digits = 3)

ord <- lay %>% filter(roster == "Co-hire ties") %>% arrange(b) %>% pull(layer)
lay$layer <- factor(lay$layer, levels = ord)

### 3. FIGURE -------------------------------------------------------
# The per-layer vintage coefficients, both rosters on one axis.

p <- ggplot(lay, aes(b, layer, colour = roster)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_pointrange(aes(xmin = lo, xmax = hi),
                  position = position_dodge(width = 0.6), size = 0.3) +
  scale_colour_manual(values = c(`Co-hire ties` = col_green,
                                 `General network` = col_purple), name = NULL) +
  labs(x = "Effect of the tie predating HIP on carrying the layer", y = NULL)
ggsave(file.path(dir_fig, "fig_layer_vintage.pdf"), p, width = 6.8, height = 5.2)
cat("wrote fig_setting_layer_vintage\n")

### 4. LATEX ---------------------------------------------------------
# The depth table per roster, and the per-layer vintage table.

cell <- function(m, term) {
  v <- grab(m, term)
  if (is.na(v["b"])) return(c("---", ""))
  c(paste0(pn_num(v["b"], 2), star(v["p"])), paste0("(", pn_num(v["se"], 2), ")"))
}
row_tex <- function(mods, label, term) {
  cs <- lapply(mods, cell, term = term)
  if (all(sapply(cs, `[`, 1) == "---")) return(character(0))
  c(sprintf("%s & %s \\\\", label, paste(sapply(cs, `[`, 1), collapse = " & ")),
    sprintf(" & %s \\\\", paste(sapply(cs, `[`, 2), collapse = " & ")))
}

hds <- c("6, no worker FE", "6, worker FE", "6, + kin and friend",
         "6, + origin~$\\times$~vintage")
emit_depth <- function(mods, origin_lab, tail, file) {
  write_tabular(file, paste0("l", strrep("c", length(mods))),
    tex_header(hds),
    c(row_tex(mods, "Tie predates HIP", "inheritedTRUE"),
      row_tex(mods, origin_lab, "same_woredaTRUE"),
      row_tex(mods, "Same woreda $\\times$ pre-HIP",
              "inheritedTRUE:same_woredaTRUE"),
      row_tex(mods, "Shared language", "shared_lang"),
      row_tex(mods, "Same production unit", "same_pu"),
      row_tex(mods, "Living together", "co_residentTRUE"),
      row_tex(mods, "Kin", "kinTRUE"),
      row_tex(mods, "Friends", "friendTRUE")),
    tail)
}
tail_of <- function(mods) c(
  span("Worker fixed effects", c("no", "yes", "yes", "yes")),
  span("Ties", sapply(mods, function(m) pn_int(nobs(m)))),
  span("Workers", sapply(mods, function(m) {
    fe <- m$fixef_sizes
    if (is.null(fe)) "---" else pn_int(fe[["worker_id"]])
  })))

ch_mods <- list(m1, m2, m3, m3i)
emit_depth(ch_mods, "Shared birth woreda", tail_of(ch_mods),
           "table_depth_cohire.tex")

gn_mods <- list(m4, m5, m6, m6i)
emit_depth(gn_mods, "Alter from same woreda", tail_of(gn_mods),
           "table_depth_gn.tex")

lay_tab <- function(r) {
  x <- lay[lay$roster == r, ]
  x[order(match(as.character(x$layer), levels(lay$layer))), ]
}
ch7 <- lay_tab("Co-hire ties")
gn7 <- lay_tab("General network")
stopifnot(identical(as.character(ch7$layer), as.character(gn7$layer)))
cell7 <- function(x, k) {
  sprintf("%s%s (%s)", pn_num(x$b[k], 3), star(x$p[k]), pn_num(x$se[k], 3))
}
rng <- function(x) {
  if (min(x$n) == max(x$n)) pn_int(max(x$n))
  else sprintf("%s--%s", pn_int(min(x$n)), pn_int(max(x$n)))
}
write_tabular("table_vintage_layers.tex", "lcc",
  tex_header(c("7, co-hire ties", "7, general network"),
             lead = "\\textit{layer} ", width = 40),
  sapply(seq_len(nrow(ch7)), function(k)
    sprintf("%s & %s & %s \\\\", as.character(ch7$layer[k]),
            cell7(ch7, k), cell7(gn7, k))),
  sprintf("Ties & %s & %s \\\\", rng(ch7), rng(gn7)))

mean_depth <- function(tbl, keep) pn_num(mean(tbl$n_layers[keep]), 2)
cv <- function(m, term, what) {
  v <- grab(m, term)
  switch(what, b = pn_num(v["b"], 2), se = pn_num(v["se"], 2),
         p = pn_num(v["p"], 3))
}
n_pos <- function(r) sum(lay$roster == r & lay$b > 0 & lay$p < 0.05)
cv_pct <- function(m, term) {
  v <- grab(m, term)
  pn_num(100 * (exp(v["b"]) - 1), 0)
}
lay_pp <- function(r, gen) {
  lab <- unname(generator_labels[as.character(gen)])
  v <- lay$b[lay$roster == r & lay$layer == lab]
  stopifnot(length(v) == 1)
  pn_num(100 * v, 1)
}
lay_b <- function(r, gen) {
  lab <- unname(generator_labels[as.character(gen)])
  v <- lay$b[lay$roster == r & lay$layer == lab]
  stopifnot(length(v) == 1)
  pn_num(v, 2)
}
pn <- list(
  MuxDepthCohireInh = mean_depth(d, d$inherited),
  MuxDepthCohireNew = mean_depth(d, !d$inherited),
  MuxDepthGnInh = mean_depth(gn, gn$inherited),
  MuxDepthGnNew = mean_depth(gn, !gn$inherited),
  MuxInhCohire = cv(m2, "inheritedTRUE", "b"),
  MuxInhCohireSe = cv(m2, "inheritedTRUE", "se"),
  MuxInhGn = cv(m5, "inheritedTRUE", "b"),
  MuxInhGnSe = cv(m5, "inheritedTRUE", "se"),
  MuxOriginCohire = cv(m2, "same_woredaTRUE", "b"),
  MuxOriginCohireP = cv(m2, "same_woredaTRUE", "p"),
  MuxOriginGn = cv(m5, "same_woredaTRUE", "b"),
  MuxOriginGnP = cv(m5, "same_woredaTRUE", "p"),
  MuxOriginInhCohire = cv(m3i, "inheritedTRUE:same_woredaTRUE", "b"),
  MuxOriginInhCohireSe = cv(m3i, "inheritedTRUE:same_woredaTRUE", "se"),
  MuxOriginInhGn = cv(m6i, "inheritedTRUE:same_woredaTRUE", "b"),
  MuxOriginInhGnSe = cv(m6i, "inheritedTRUE:same_woredaTRUE", "se"),
  MuxOriginInhCellCohire = pn_int(sum(!d_reg$inherited & d_reg$same_woreda)),
  MuxResidGn = cv(m5, "co_residentTRUE", "b"),
  MuxKinCohire = cv(m3, "kinTRUE", "b"),
  MuxKinGn = cv(m6, "kinTRUE", "b"),
  MuxNCohire = pn_int(nobs(m2)),
  MuxNCohireEgos = pn_int(m2$fixef_sizes[["worker_id"]]),
  MuxNGn = pn_int(nobs(m5)),
  MuxNGnEgos = pn_int(m5$fixef_sizes[["worker_id"]]),
  MuxDroppedCohire = pn_int(nobs(m1) - nobs(m2)),
  MuxLayersCohire = pn_int(n_pos("Co-hire ties")),
  MuxLayersGn = pn_int(n_pos("General network")),
  MuxBreaksCohire = lay_b("Co-hire ties", 9),
  MuxBreaksGn = lay_b("General network", 9),
  MuxCommuteCohire = lay_b("Co-hire ties", 6),
  MuxLargeExpGn = lay_b("General network", 3),
  MuxBreaksPrevCohire = pn_pct(lay$prevalence[lay$roster == "Co-hire ties" &
                        lay$layer == unname(generator_labels["9"])], 0),
  MuxVintEgosGn = pn_int(n_distinct(vs_gn$worker_id)),
  MuxVintTiesGn = pn_int(nrow(vs_gn)),
  MuxPostGn = pn_int(sum(!gn_reg$inherited)),
  MuxVintEgosCohire = pn_int(n_distinct(vs_ch$worker_id)),
  MuxVintTiesCohire = pn_int(nrow(vs_ch)),
  MuxPostCohire = pn_int(sum(!d_reg$inherited)),
  MuxBreadthDepthGn = pn_num(cor_gn, 2),
  MuxBreadthDepthCohire = pn_num(cor_ch, 2),
  MuxInhCohirePct = cv_pct(m2, "inheritedTRUE"),
  MuxInhGnPct = cv_pct(m5, "inheritedTRUE"),
  MuxOriginCohirePct = cv_pct(m2, "same_woredaTRUE"),
  MuxOriginGnPct = cv_pct(m5, "same_woredaTRUE"),
  MuxResidGnPct = cv_pct(m5, "co_residentTRUE"),
  MuxKinCohirePct = cv_pct(m3, "kinTRUE"),
  MuxKinGnPct = cv_pct(m6, "kinTRUE"),
  MuxBreaksCohirePp = lay_pp("Co-hire ties", 9),
  MuxBreaksGnPp = lay_pp("General network", 9),
  MuxCommuteCohirePp = lay_pp("Co-hire ties", 6),
  MuxLargeExpGnPp = lay_pp("General network", 3))

### 5. MACROS -------------------------------------------------------
# The Specification 6 and 7 figures, as \pn macros.

write_pn(pn, file.path(dir_pn, "pn_mux.tex"))
