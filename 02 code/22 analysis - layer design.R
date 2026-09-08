# 22 analysis - layer design.R
#
# Analysis (Specification 8). Does shared origin change WHICH of the
# fifteen relationship layers a tie carries? For each roster, the layer
# composition is regressed on shared origin under a pair fixed effect,
# once on its own and once holding kinship, tie vintage and co-residence
# fixed; the coefficients are centred so they sum to zero across layers.
#
# Feeds: Section 6 Results (Specification 8) and the Conclusions.
#
# Input:  dyads.parquet, gn_edgelist.parquet
# Output: 20 analysis/table_layer_by_origin.csv, table_joint_tests.csv,
#         table_kinship_profile.csv,
#         04 latex/tables/table_layers_cohire.tex, table_layers_gn.tex,
#         04 latex/figures/fig_layer_origin.pdf, 04 latex/pn/pn_layer.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# The two tie-level frames, tied pairs only, usable origin on both sides.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "22 analysis - layer design.R"

col_green <- "#a8d5a2"
col_purple <- "#e0b0FF"
theme_set(theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom"))

d_all <- read_parquet(file.path(dir_derived, "dyads.parquet"))
gn_all <- read_parquet(file.path(dir_derived, "gn_edgelist.parquet"))

d  <- tie_frame_cohire(d_all) %>% filter(!is.na(same_woreda))
gn <- tie_frame_gn(gn_all)     %>% filter(!is.na(same_woreda))

stopifnot(max(d$n_layers) < n_layer, max(gn$n_layers) < n_layer,
          !anyNA(d$kin), !anyNA(d$inherited),
          !anyNA(gn$kin), !anyNA(gn$inherited), !anyNA(gn$co_resident))

cat("co-hire ties", nrow(d), "| shared origin", sum(d$same_woreda),
    "| kin among them", sum(d$kin & d$same_woreda), "\n")
cat("general network ties", nrow(gn), "| shared origin", sum(gn$same_woreda),
    "| kin among them", sum(gn$kin & gn$same_woreda), "\n")

### 2. ESTIMATION -----------------------------------------------------
# Stack the data to one row per (tie, layer); fit y ~ layer-specific
# origin dummies | pair + layer; centre the coefficients so they sum to
# zero across the fifteen layers, and joint-test them.

# One row per (tie, layer), with an origin dummy that is on only for that
# layer (H01..H15), and the same for each extra control.
stack_layers <- function(tbl, extra = character()) {
  n <- nrow(tbl)
  out <- data.frame(
    pair_id = rep(seq_len(n), times = n_layer),
    layer = factor(rep(layer_labs, each = n), levels = layer_labs),
    y = as.vector(sapply(layer_cols, function(k) as.numeric(tbl[[k]] > 0))),
    h = rep(as.numeric(tbl$same_woreda), times = n_layer))
  for (v in extra) out[[v]] <- rep(as.numeric(tbl[[v]]), times = n_layer)
  for (k in seq_len(n_layer)) {
    sel <- as.integer(out$layer) == k
    out[[sprintf("H%02d", k)]] <- out$h * sel
    for (v in extra) out[[sprintf("%s_%02d", v, k)]] <- out[[v]] * sel
  }
  out
}
sd_ch <- stack_layers(d, c("kin", "inherited"))
sd_gn <- stack_layers(gn, c("kin", "inherited", "co_resident"))
stopifnot(nrow(sd_gn) == nrow(gn) * n_layer,
          nrow(sd_ch) == nrow(d) * n_layer)

fit_layers <- function(sdat, extra = character(), ref = 1) {
  keep <- setdiff(seq_len(n_layer), ref)
  terms <- sprintf("H%02d", keep)
  for (v in extra) terms <- c(terms, sprintf("%s_%02d", v, keep))
  m <- feols(as.formula(paste("y ~", paste(terms, collapse = " + "),
                              "| pair_id + layer")),
             data = sdat, cluster = ~ pair_id)
  list(m = m, keep = keep, ref = ref)
}

blk <- function(fit, pre) {
  sprintf(if (pre == "H") "H%02d" else paste0(pre, "_%02d"), fit$keep)
}

# Recover all fifteen coefficients and centre them to sum to zero.
centre <- function(fit, pre = "H") {
  nm <- blk(fit, pre)
  b_full <- numeric(n_layer)
  V_full <- matrix(0, n_layer, n_layer)
  b_full[fit$keep] <- coef(fit$m)[nm]
  V_full[fit$keep, fit$keep] <- vcov(fit$m)[nm, nm]
  C <- diag(n_layer) - matrix(1 / n_layer, n_layer, n_layer)
  b_c <- as.vector(C %*% b_full)
  se_c <- sqrt(pmax(diag(C %*% V_full %*% t(C)), 0))
  data.frame(layer = layer_labs, b = b_c, se = se_c,
             p = 2 * pnorm(abs(b_c / se_c), lower.tail = FALSE))
}

joint_test <- function(fit, pre = "H") {
  nm <- blk(fit, pre)
  b <- coef(fit$m)[nm]
  V <- vcov(fit$m)[nm, nm]
  W <- as.numeric(t(b) %*% solve(V) %*% b)
  c(stat = W, df = length(b), p = pchisq(W, df = length(b), lower.tail = FALSE))
}

x_ch <- c("kin", "inherited")
x_gn <- c("kin", "inherited", "co_resident")
f_ch1 <- fit_layers(sd_ch)
f_ch2 <- fit_layers(sd_ch, x_ch)
f_gn1 <- fit_layers(sd_gn)
f_gn2 <- fit_layers(sd_gn, x_gn)
fits <- list(f_ch1, f_ch2, f_gn1, f_gn2)
est <- lapply(fits, centre)
jt <- lapply(fits, joint_test)
jt_kin_gn <- joint_test(f_gn2, "kin")
jt_kin_ch <- joint_test(f_ch2, "kin")
kin_prof_gn <- centre(f_gn2, "kin")

# The centred estimates should not depend on which layer is the reference.
for (i in seq_along(fits)) {
  sdat <- if (i <= 2) sd_ch else sd_gn
  xtra <- if (i == 2) x_ch else if (i == 4) x_gn else character()
  a <- centre(fit_layers(sdat, xtra, ref = n_layer))
  stopifnot(max(abs(a$b - est[[i]]$b)) < 1e-8,
            max(abs(a$se - est[[i]]$se)) < 1e-8)
}
cat("reference-layer invariance: OK\n")

corr_gn <- cor(est[[3]]$b, kin_prof_gn$b)
cat("cor(uncontrolled origin profile, kinship profile), GN:",
    round(corr_gn, 3), "\n")

cols <- c("Co-hire, origin alone", "Co-hire, controlled",
          "General network, origin alone", "General network, controlled")
for (i in seq_along(fits)) {
  cat("\n---", cols[i], "--- joint chi2(", jt[[i]]["df"], ") =",
      round(jt[[i]]["stat"], 2), ", p =", signif(jt[[i]]["p"], 3), "\n")
  print(est[[i]][order(-est[[i]]$b), ], digits = 3, row.names = FALSE)
}

out <- do.call(rbind, lapply(seq_along(fits), function(i)
  cbind(spec = cols[i], est[[i]])))
write.csv(out, file.path(dir_analysis, "table_layer_by_origin.csv"),
          row.names = FALSE)
write.csv(cbind(spec = c(cols, "Kinship profile, GN", "Kinship profile, co-hire"),
                do.call(rbind, c(jt, list(jt_kin_gn, jt_kin_ch)))),
          file.path(dir_analysis, "table_joint_tests.csv"), row.names = FALSE)
write.csv(cbind(layer = layer_labs, origin_raw = est[[3]]$b,
                kinship = kin_prof_gn$b),
          file.path(dir_analysis, "table_kinship_profile.csv"),
          row.names = FALSE)

fig <- rbind(
  cbind(roster = "Co-hire ties", spec = "Shared origin alone", est[[1]]),
  cbind(roster = "Co-hire ties",
        spec = "Holding kinship, vintage and co-residence", est[[2]]),
  cbind(roster = "General network", spec = "Shared origin alone", est[[3]]),
  cbind(roster = "General network",
        spec = "Holding kinship, vintage and co-residence", est[[4]]))
ord <- est[[3]]$layer[order(est[[3]]$b)]
fig$layer <- factor(fig$layer, levels = ord)
fig$spec <- factor(fig$spec, levels = c("Shared origin alone",
                                        "Holding kinship, vintage and co-residence"))
fig$lo <- fig$b - 1.96 * fig$se
fig$hi <- fig$b + 1.96 * fig$se

### 3. FIGURE -------------------------------------------------------
# Both specifications on one axis per roster, so the collapse is visible.

p <- ggplot(fig, aes(b, layer, colour = spec)) +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
  geom_pointrange(aes(xmin = lo, xmax = hi),
                  position = position_dodge(width = 0.6), size = 0.25) +
  facet_wrap(~ roster) +
  scale_colour_manual(values = c(`Shared origin alone` = col_purple,
    `Holding kinship, vintage and co-residence` = col_green), name = NULL) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(x = "Shift in the layer's share of the tie, shared origin against not",
       y = NULL)
ggsave(file.path(dir_fig, "fig_layer_origin.pdf"), p, width = 7.4, height = 5.4)
cat("wrote fig_layer_origin\n")

pn_p <- function(p, digits = 3) {
  lim <- 10^(-digits)
  if (p < lim) sprintf("$<$%s", formatC(lim, format = "f", digits = digits))
  else pn_num(p, digits)
}

### 4. LATEX ---------------------------------------------------------
# One table per roster; coefficient and standard error share a cell.

hds8 <- c("8, no controls", "8, + kinship, vintage, co-residence")
emit_layers <- function(idx, ties, origin_ties, obs, file) {
  ee <- est[idx]
  jj <- jt[idx]
  row_for <- function(k) {
    cells <- sapply(ee, function(e)
      sprintf("%s%s (%s)", pn_num(e$b[k], 3), star(e$p[k]),
              pn_num(e$se[k], 3)))
    sprintf("%s & %s \\\\", layer_labs[k], paste(cells, collapse = " & "))
  }
  write_tabular(file, "lcc",
    tex_header(hds8, lead = "\\textit{layer} ", width = 40),
    sapply(seq_len(n_layer), row_for),
    c(span(sprintf("Joint test, $\\chi^2(%s)$", pn_int(jj[[1]]["df"])),
           sapply(jj, function(j) paste0(pn_num(j["stat"], 2), star(j["p"])))),
      span("Ties", rep(pn_int(ties), 2)),
      span("\\hspace{1em}of which shared origin", rep(pn_int(origin_ties), 2)),
      span("Tie-by-layer observations", rep(pn_int(obs), 2))))
}
emit_layers(1:2, nrow(d),  sum(d$same_woreda),  nrow(sd_ch),
            "table_layers_cohire.tex")
emit_layers(3:4, nrow(gn), sum(gn$same_woreda), nrow(sd_gn),
            "table_layers_gn.tex")

lb <- function(e, gen) {
  lab <- if (is.character(gen)) unname(rs_out_labels[gen]) else
         unname(generator_labels[as.character(gen)])
  stopifnot(sum(e$layer == lab) == 1)
  e$b[e$layer == lab]
}
maxabs <- function(e) max(abs(e$b))
lyr_fit <- lapply(list(f_ch1, f_ch2, f_gn1, f_gn2), function(f) predict(f$m))
lyr_out <- max(sapply(lyr_fit, function(v) mean(v < 0 | v > 1)))
lyr_dev <- max(sapply(lyr_fit, function(v) max(pmax(-v, v - 1, 0))))
lyr_var <- sapply(list(sd_ch, sd_gn), function(sd)
  mean(tapply(sd$y, sd$pair_id, function(z) length(unique(z)) > 1)))

pn <- list(
  LayerLpmOutsidePct = pn_pct(lyr_out),
  LayerLpmMaxDevPp = pn_num(100 * lyr_dev, 0),
  LayerPairsVaryPct = pn_pct(min(lyr_var)),
  LayerJointGn = pn_num(jt[[3]]["stat"], 2),
  LayerJointGnDf = pn_int(jt[[3]]["df"]),
  LayerJointGnCtrl = pn_num(jt[[4]]["stat"], 2),
  LayerJointGnCtrlP = pn_num(jt[[4]]["p"], 3),
  LayerJointCohireDf = pn_int(jt[[1]]["df"]),
  LayerJointCohire = pn_num(jt[[1]]["stat"], 2),
  LayerJointCohireP = pn_num(jt[[1]]["p"], 3),
  LayerJointCohireCtrl = pn_num(jt[[2]]["stat"], 2),
  LayerJointCohireCtrlP = pn_num(jt[[2]]["p"], 3),
  LayerKinJointGn = pn_num(jt_kin_gn["stat"], 2),
  LayerProfileCorrGn = pn_num(corr_gn, 2),
  LayerLargeExpGn = pn_num(lb(est[[3]], 3), 3),
  LayerLargeExpGnCtrl = pn_num(lb(est[[4]], 3), 3),
  LayerAssistGn = pn_num(lb(est[[3]], 11), 3),
  LayerBreaksGn = pn_num(lb(est[[3]], 9), 3),
  LayerBreaksGnCtrl = pn_num(lb(est[[4]], 9), 3),
  LayerNavigateGnCtrl = pn_num(lb(est[[4]], 5), 3),
  LayerCommuteGn = pn_num(lb(est[[3]], 6), 3),
  LayerFriendsGn = pn_num(lb(est[[3]], 10), 3),
  LayerFriendsGnCtrl = pn_num(lb(est[[4]], 10), 3),
  LayerKinLargeExpGn = pn_num(kin_prof_gn$b[kin_prof_gn$layer ==
                              unname(generator_labels["3"])], 3),
  LayerKinBreaksGn = pn_num(kin_prof_gn$b[kin_prof_gn$layer ==
                            unname(generator_labels["9"])], 3),
  LayerMaxAbsGnCtrl = pn_num(maxabs(est[[4]]), 3),
  LayerMaxAbsCohireCtrl = pn_num(maxabs(est[[2]]), 3),
  LayerLargeExpGnPp = pn_num(100 * lb(est[[3]], 3), 1),
  LayerLargeExpGnCtrlPp = pn_num(100 * lb(est[[4]], 3), 1),
  LayerBreaksGnPp = pn_num(100 * lb(est[[3]], 9), 1),
  LayerMaxAbsGnCtrlPp = pn_num(100 * maxabs(est[[4]]), 1),
  LayerMaxAbsCohireCtrlPp = pn_num(100 * maxabs(est[[2]]), 1),
  LayerSigGn = pn_int(sum(est[[3]]$p < 0.05)),
  LayerSigGnCtrl = pn_int(sum(est[[4]]$p < 0.05)),
  LayerTiesGn = pn_int(nrow(gn)),
  LayerTiesCohire = pn_int(nrow(d)),
  LayerOriginTiesGn = pn_int(sum(gn$same_woreda)),
  LayerOriginTiesCohire = pn_int(sum(d$same_woreda)),
  LayerKinShareGn = pn_pct(mean(gn$kin[gn$same_woreda]), 0),
  LayerKinShareGnNon = pn_pct(mean(gn$kin[!gn$same_woreda]), 0),
  LayerKinShareCohire = pn_pct(mean(d$kin[d$same_woreda]), 0),
  LayerKinCohireTies = pn_int(sum(d$kin & d$same_woreda)),
  LayerObsGn = pn_int(nrow(sd_gn)),
  LayerObsCohire = pn_int(nrow(sd_ch)))

### 5. MACROS -------------------------------------------------------
# The Specification 8 figures, as \pn macros.

write_pn(pn, file.path(dir_pn, "pn_layer.tex"))
