# 16 descriptives - ties and dyads.R
#
# Descriptives. Composition of inherited ties (kin / friends / acquaintances)
# on each roster, how the worker heard about the job, and the LaTeX
# tie/dyad-composition tables.
#
# Feeds: Empirical Setting (inherited-tie composition, job informants).
# Reads back table_dyad_composition.csv written by script 10.
#
# Input:  respondents.parquet, dyads.parquet, gn_edgelist.parquet,
#         10 descriptives/table_dyad_composition.csv
# Output: 10 descriptives/table_inherited_tie_composition.csv,
#         table_inherited_rate.csv, table_job_informants.csv
#         04 latex/tables/table_tie_composition.tex,
#         table_dyad_composition.tex, 04 latex/pn/pn_ties.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# Both rosters (co-hire dyads and general-network alters).

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "16 descriptives - ties and dyads.R"

d <- read_parquet(file.path(dir_derived, "dyads.parquet"))
gn <- read_parquet(file.path(dir_derived, "gn_edgelist.parquet"))

kin_codes <- as.character(1:6)

### 2. TABLES ---------------------------------------------------------
# Group the relationship code, then, per roster, the kin/friend/other
# split of inherited ties and the share of each that is co-villager;
# then the overall inherited rate and the job-informant composition.

rel_group <- function(code) {
  factor(case_when(code %in% kin_codes ~ "Kin",
                   code == "7" ~ "Friends",
                   code == "8" ~ "Acquaintances",
                   TRUE ~ "Other or unknown"),
         levels = c("Kin", "Friends", "Acquaintances", "Other or unknown"))
}

gn <- gn %>%
  mutate(rel = rel_group(relationship),
         same_woreda = case_when(
           origin == "2" ~ TRUE,
           origin == "3" ~ orig_woreda == birth_woreda_i,
           origin == "1" ~ FALSE,
           TRUE ~ NA),
         inherited = pre_hip == "1")

d <- d %>%
  mutate(rel = rel_group(relation),
         same_woreda = if_else(origin_valid, same_woreda == 1, NA),
         inherited = tied_pre == 1)

compose <- function(x, roster) {
  inh <- x %>% filter(inherited, rel != "Other or unknown")
  inh %>%
    group_by(rel) %>%
    summarise(n = n(),
              n_origin = sum(!is.na(same_woreda)),
              n_cov = sum(same_woreda, na.rm = TRUE), .groups = "drop") %>%
    complete(rel, fill = list(n = 0L, n_origin = 0L, n_cov = 0L)) %>%
    mutate(roster = roster,
           share = n / sum(n),
           share_cov = if_else(n_origin > 0, n_cov / n_origin, NA_real_))
}
comp <- bind_rows(compose(d, "Co-hire ties"), compose(gn, "General network"))
cat("inherited ties dropped for an invalid relation code:",
    sum(d$inherited & d$rel == "Other or unknown", na.rm = TRUE) +
    sum(gn$inherited & gn$rel == "Other or unknown", na.rm = TRUE), "\n")

stopifnot(all(comp$n[comp$rel == "Other or unknown"] == 0))

inh_rate <- bind_rows(
  d %>% filter(tied_any == 1) %>%
    summarise(roster = "Co-hire ties",
              n_ties = n(), n_inherited = sum(inherited),
              n_cov_known = sum(!is.na(same_woreda))),
  gn %>%
    summarise(roster = "General network",
              n_ties = n(), n_inherited = sum(inherited),
              n_cov_known = sum(!is.na(same_woreda)))) %>%
  mutate(share_inherited = n_inherited / n_ties)

write.csv(comp, file.path(dir_desc, "table_inherited_tie_composition.csv"),
          row.names = FALSE)
write.csv(inh_rate, file.path(dir_desc, "table_inherited_rate.csv"),
          row.names = FALSE)

resp <- read_parquet(file.path(dir_derived, "respondents.parquet")) %>%
  filter(!in_problem)
info <- gn %>% filter(job_informant == 1)
n_info_egos <- n_distinct(info$worker_id)
stopifnot(all(info$worker_id %in% resp$worker_id))
n_info_none <- nrow(resp) - n_info_egos
info_comp <- info %>% count(rel) %>% mutate(share = n / sum(n))
ic <- function(g_) {
  s <- info_comp$share[info_comp$rel == g_]
  if (length(s) == 0) 0 else s
}
write.csv(info_comp, file.path(dir_desc, "table_job_informants.csv"),
          row.names = FALSE)
cat("\njob informants by relationship:\n")
print(as.data.frame(info_comp))
cat("inherited informants:", sum(info$inherited), "of", nrow(info), "\n")

### 3. LATEX ---------------------------------------------------------
# The tie-composition table, and the dyad-composition table (which reads
# back the CSV script 10 wrote).

tex_rows <- function(r) {
  x <- comp %>% filter(roster == r, n > 0)
  sprintf("%s & %s & %s & %s \\\\", x$rel, pn_int(x$n), pn_pct(x$share, 1),
          ifelse(is.na(x$share_cov), "---", pn_pct(x$share_cov, 1)))
}
write_tabular("table_tie_composition.tex", "lrrr",
  "& \\textit{ties} & \\textit{\\% of inherited} & \\textit{\\% co-villager} \\\\",
  c("\\multicolumn{4}{l}{\\textit{Co-hire ties}} \\\\",
    tex_rows("Co-hire ties"),
    "\\midrule",
    "\\multicolumn{4}{l}{\\textit{General network}} \\\\",
    tex_rows("General network")))

t0d <- read.csv(file.path(dir_desc, "table_dyad_composition.csv"),
                stringsAsFactors = FALSE)

t0d <- t0d[order(match(t0d$status, c("no tie", "tie, formed post-HIP",
                                     "tie, pre-HIP or family",
                                     "tie, timing unknown"))), ]
stopifnot(!any(is.na(t0d$share_same_pu)))
write_tabular("table_dyad_composition.tex", "lrrrrr",
  paste("& \\textit{pairs} & \\textit{same woreda} & \\textit{same zone} &",
        "\\textit{shared language} & \\textit{same unit} \\\\"),
  sprintf("%s & %s & %s & %s & %s & %s \\\\",
          c("No tie", "Tie, formed post-HIP", "Tie, pre-HIP or family",
            "Tie, timing unknown")[match(t0d$status,
            c("no tie", "tie, formed post-HIP", "tie, pre-HIP or family",
              "tie, timing unknown"))],
          pn_int(t0d$n), pn_pct(t0d$share_same_woreda, 1),
          pn_pct(t0d$share_same_zone, 1), pn_pct(t0d$share_shared_lang, 1),
          pn_pct(t0d$share_same_pu, 1)))

### 4. MACROS -------------------------------------------------------
# The tie figures the Setting prose cites, as \pn macros.

g <- function(r, g_) comp$share[comp$roster == r & comp$rel == g_]
gc_ <- function(r, g_) comp$share_cov[comp$roster == r & comp$rel == g_]
nt <- function(r, col) inh_rate[[col]][inh_rate$roster == r]
pn <- list(
  DyadNoTie = pn_int(t0d$n[t0d$status == "no tie"]),
  DyadTiePre = pn_int(t0d$n[t0d$status == "tie, pre-HIP or family"]),
  DyadTiePost = pn_int(t0d$n[t0d$status == "tie, formed post-HIP"]),
  DyadPreSameWoredaPct = pn_pct(t0d$share_same_woreda[t0d$status ==
                                "tie, pre-HIP or family"], 1),
  DyadNoTieSameWoredaPct = pn_pct(t0d$share_same_woreda[t0d$status ==
                                  "no tie"], 1),
  DyadPreSharedLangPct = pn_pct(t0d$share_shared_lang[t0d$status ==
                                "tie, pre-HIP or family"], 1),
  DyadNoTieSharedLangPct = pn_pct(t0d$share_shared_lang[t0d$status ==
                                  "no tie"], 1),
  DyadPuKnown = pn_int(t0d$n_pu_known[t0d$status == "no tie"] +
                       t0d$n_pu_known[t0d$status == "tie, formed post-HIP"] +
                       t0d$n_pu_known[t0d$status == "tie, pre-HIP or family"]),
  TieCohireInherited = pn_int(nt("Co-hire ties", "n_inherited")),
  TieCohireInheritedPct = pn_pct(inh_rate$share_inherited[inh_rate$roster ==
                                 "Co-hire ties"], 0),
  TieGnInherited = pn_int(nt("General network", "n_inherited")),
  TieGnInheritedPct = pn_pct(inh_rate$share_inherited[inh_rate$roster ==
                             "General network"], 0),
  TieCohireKinPct = pn_pct(g("Co-hire ties", "Kin"), 1),
  TieGnKinPct = pn_pct(g("General network", "Kin"), 1),
  TieCohireFriendPct = pn_pct(g("Co-hire ties", "Friends"), 1),
  TieGnFriendPct = pn_pct(g("General network", "Friends"), 1),
  TieCohireKinCovPct = pn_pct(gc_("Co-hire ties", "Kin"), 1),
  TieGnKinCovPct = pn_pct(gc_("General network", "Kin"), 1),
  TieCohireAcqCovPct = pn_pct(gc_("Co-hire ties", "Acquaintances"), 1),
  TieGnAcqCovPct = pn_pct(gc_("General network", "Acquaintances"), 1),
  JobInfoNone = pn_int(n_info_none),
  JobInfoNonePct = pn_pct(n_info_none / nrow(resp), 0),
  JobInfoEgos = pn_int(n_info_egos),
  JobInfoTies = pn_int(nrow(info)),
  JobInfoKinPct = pn_pct(ic("Kin"), 0),
  JobInfoFriendPct = pn_pct(ic("Friends"), 0),
  JobInfoInherited = pn_int(sum(info$inherited)),
  JobInfoNotPre = pn_int(sum(!info$inherited)))
write_pn(pn, file.path(dir_pn, "pn_ties.tex"))

cat("\ninherited-tie composition:\n")
print(as.data.frame(comp))
cat("\ninherited rate:\n")
print(as.data.frame(inh_rate))
cat("\ndyad sample:\n")
print(t0d)
