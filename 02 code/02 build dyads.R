# 02 build dyads.R
#
# Builder. Constructs the co-hire dyad dataset: one row per ordered pair of
# workers who could have named each other on the preloaded co-hire roster,
# with the tie outcomes (named on any layer, recalled, inherited vs formed
# after joining), the pair's shared attributes (origin, language, religion,
# gender, production unit), and the hiring "batch" each pair sits in.
#
# It also writes the two anonymising crosswalks (batch and firm), which
# replace real hiring-set and firm identifiers with batch_NN / "Firm A".
#
# Input:  raw baseline, hiring roster, cases file, respondents.parquet
# Output: 00 derived/dyads.parquet
#         00 derived/batch_crosswalk.csv, 00 derived/firm_crosswalk.csv
#         04 latex/pn/pn_build.tex
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# The respondent frame, and the accumulator for this script's \pn macros.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))
SCRIPT <- "02 build dyads.R"

resp <- read_parquet(file.path(dir_derived, "respondents.parquet"))

pn_build <- list()

### 2. BUILD -----------------------------------------------------------
# Reconstruct hiring sets from the roster, match nominated names to worker
# ids, read the tie outcomes off the joint-hire module, and assemble one
# row per ordered (ego, alter) pair with all the derived pair attributes.

# Group workers into connected hiring components. Two workers are linked
# when one's roster names the other; components are the transitive closure.
build_components <- function(file_ros = file_roster, file_cas = file_cases) {
  ros <- read_csv(file_ros, show_col_types = FALSE)
  cas <- read_csv(file_cas, show_col_types = FALSE)
  namecols <- grep("^cohire_name_", names(ros), value = TRUE)

  ros <- ros %>%
    left_join(cas %>% select(worker_id_key, cs_name = worker_name, firm_name,
                             wave, production_unit),
              by = "worker_id_key") %>%
    mutate(self_name = coalesce(na_if(norm_name(worker_name), ""),
                                norm_name(cs_name)))

  # Backfill the firm from the worker-id prefix where the cases file is blank.
  ros <- ros %>%
    mutate(firm_name = if_else(str_starts(firm_name, "JP "), "JP", firm_name),
           firm_name = coalesce(firm_name, case_when(
             str_starts(worker_id_key, "EP") ~ "EPIC",
             str_starts(worker_id_key, "EV") ~ "Everest",
             str_starts(worker_id_key, "ET") ~ "Everest",
             str_starts(worker_id_key, "JG") ~ "JP",
             str_starts(worker_id_key, "JT") ~ "JP",
             str_starts(worker_id_key, "NA") ~ "NASSA",
             str_starts(worker_id_key, "SI") ~ "Silver",
             str_starts(worker_id_key, "TA") ~ "TAL")))
  cat("firm still missing after backfill:", sum(is.na(ros$firm_name)),
      "(expected 7, the 6-digit 26xxxx ids)\n")

  cn <- ros %>%
    select(worker_id_key, all_of(namecols)) %>%
    pivot_longer(-worker_id_key, values_to = "cn") %>%
    filter(!is.na(cn), cn != "") %>%
    mutate(cn = norm_name(cn))

  name2id <- ros %>%
    filter(!is.na(self_name)) %>%
    select(self_name, id2 = worker_id_key, firm2 = firm_name)
  firm_of <- ros %>% select(worker_id_key, firm1 = firm_name)

  cand <- cn %>%
    left_join(firm_of, by = "worker_id_key") %>%
    inner_join(name2id, by = c("cn" = "self_name"),
               relationship = "many-to-many") %>%
    filter(worker_id_key != id2)

  pn_build$CrossFirmBlocked <<- pn_int(sum(!is.na(cand$firm1) &
                                           !is.na(cand$firm2) &
                                           cand$firm1 != cand$firm2))
  pn_build$EscapeEdges <<- pn_int(sum(is.na(cand$firm1) | is.na(cand$firm2)))

  edges <- cand %>% filter(is.na(firm1) | is.na(firm2) | firm1 == firm2)

  # Union-find over the roster edges.
  parent <- setNames(ros$worker_id_key, ros$worker_id_key)
  find <- function(x) {
    while (parent[[x]] != x) {
      parent[[x]] <<- parent[[parent[[x]]]]
      x <- parent[[x]]
    }
    x
  }
  for (k in seq_len(nrow(edges))) {
    a <- find(edges$worker_id_key[k])
    b <- find(edges$id2[k])
    if (a != b) parent[[a]] <- b
  }
  ros$comp <- vapply(ros$worker_id_key, find, character(1))

  # A worker's "row-set" is the exact set of ids on her own roster row,
  # kept only where every name matched unambiguously.
  amb_rs <- cand %>% count(worker_id_key, cn) %>% filter(n > 1)
  rs <- edges %>%
    anti_join(amb_rs, by = c("worker_id_key", "cn")) %>%
    group_by(worker_id_key) %>%
    summarise(rowset = paste(sort(unique(c(worker_id_key[1], id2))),
                             collapse = "|"),
              .groups = "drop")
  cat("workers with a resolved row-set:", nrow(rs), "of", nrow(ros),
      "| distinct row-sets:", n_distinct(rs$rowset), "\n")

  ros %>%
    left_join(rs, by = "worker_id_key") %>%
    mutate(rowset = coalesce(rowset, worker_id_key)) %>%
    select(worker_id_key, self_name, comp, rowset, firm_name, wave,
           production_unit, nb_cohires)
}
roster <- build_components()
cat("roster workers:", nrow(roster), "| components:",
    n_distinct(roster$comp), "\n")

cols_hdr <- baseline_names()

# The ego's choice set: the roster slots she was actually shown.
ch <- read_baseline_cols(c("SubmissionDate", "worker_id", "consent_survey",
  grep("^ch_name_[0-9]+$", cols_hdr, value = TRUE))) %>%
  dedup_respondents()
choice <- ch %>%
  pivot_longer(matches("^ch_name_"), names_to = "slot", values_to = "cn") %>%
  filter(!is.na(cn), cn != "") %>%
  mutate(K = as.integer(str_extract(slot, "[0-9]+$")), cn = norm_name(cn)) %>%
  select(worker_id, K, cn)
cat("preloaded choice-set entries:", nrow(choice),
    "over", n_distinct(choice$worker_id), "egos\n")

# Match each choice-set name to an alter worker id, within the same component.
name2id <- roster %>%
  filter(!is.na(self_name)) %>%
  select(self_name, alter_id = worker_id_key, alter_comp = comp)
dyads <- choice %>%
  left_join(roster %>% select(worker_id = worker_id_key, comp = comp),
            by = "worker_id") %>%
  left_join(name2id, by = c("cn" = "self_name"),
            relationship = "many-to-many") %>%
  filter(!is.na(alter_id), alter_comp == comp | is.na(comp))
amb <- dyads %>% count(worker_id, K) %>% filter(n > 1)
cat("ambiguous name matches dropped:", nrow(amb), "\n")
dyads <- dyads %>% anti_join(amb, by = c("worker_id", "K"))

# Which generator each roster slot was nominated on (the joint-hire module).
jh <- read_baseline_cols(c("SubmissionDate", "worker_id", "consent_survey",
  grep("^_jh_q_idx_[0-9]+$", cols_hdr, value = TRUE),
  grep("^jh_tie_sel_[0-9]+_[0-9]+$", cols_hdr, value = TRUE))) %>%
  dedup_respondents()

qidx <- jh %>%
  select(worker_id, matches("^_jh_q_idx_")) %>%
  pivot_longer(-worker_id, names_to = "pcol", values_to = "gen") %>%
  mutate(P = as.integer(str_extract(pcol, "[0-9]+$"))) %>%
  select(worker_id, P, gen)

ties <- jh %>%
  select(worker_id, matches("^jh_tie_sel_[0-9]+_[0-9]+$")) %>%
  pivot_longer(-worker_id, names_to = "col", values_to = "v") %>%
  filter(v == "1") %>%
  mutate(K = as.integer(str_match(col, "^jh_tie_sel_([0-9]+)_")[, 2]),
         P = as.integer(str_extract(col, "[0-9]+$"))) %>%
  filter(K >= 1, K <= 90) %>%
  left_join(qidx, by = c("worker_id", "P")) %>%
  select(worker_id, K, gen)

cat("generator nominations on roster slots:", nrow(ties), "\n")
tie_profile <- ties %>%
  mutate(gen = paste0("gen_", gen), val = 1L) %>%
  distinct(worker_id, K, gen, val) %>%
  pivot_wider(names_from = gen, values_from = val, values_fill = 0L) %>%
  mutate(n_generators = rowSums(across(starts_with("gen_"))), tied_any = 1L)

# Which roster slots the ego recalled at all.
recall_raw <- read_baseline_cols(c("SubmissionDate", "worker_id",
  "consent_survey",
  grep("^jh_recalled_[0-9]+$", cols_hdr, value = TRUE))) %>%
  dedup_respondents()

recall_profile <- recall_raw %>%
  select(worker_id, matches("^jh_recalled_[0-9]+$")) %>%
  pivot_longer(-worker_id, names_to = "col", values_to = "v") %>%
  filter(v == "1") %>%
  mutate(K = as.integer(str_extract(col, "[0-9]+$"))) %>%
  filter(K >= 1, K <= 90) %>%
  distinct(worker_id, K) %>%
  mutate(recalled = 1L)
cat("recall nominations on roster slots:", nrow(recall_profile),
    "over", n_distinct(recall_profile$worker_id), "egos\n")

# Risk-sharing outflow: which co-hires would turn to the ego for help.
rs_dims <- c(borrow = "rs_who_borrow", large_exp = "rs_who_large_exp",
             assisted_3m = "rs_who_assisted_3m")

rs_raw <- read_baseline_cols(c("SubmissionDate", "worker_id", "consent_survey",
  grep("^rs_who_(borrow|large_exp|assisted_3m)_[0-9]+$", cols_hdr,
       value = TRUE))) %>%
  dedup_respondents()

rs_long <- rs_raw %>%
  select(worker_id, matches("^rs_who_")) %>%
  pivot_longer(-worker_id, names_to = "col", values_to = "v") %>%
  filter(v == "1") %>%
  mutate(dim = str_match(col, "^rs_who_(borrow|large_exp|assisted_3m)_")[, 2],
         idx = as.integer(str_extract(col, "[0-9]+$"))) %>%
  filter(idx >= 201, idx <= 290) %>%
  transmute(worker_id, K = idx - 200L, dim = paste0("rs_out_", dim),
            val = 1L) %>%
  distinct()

cat("risk-sharing outflow nominations on co-hire slots:", nrow(rs_long), "\n")
rs_profile <- rs_long %>%
  pivot_wider(names_from = dim, values_from = val, values_fill = 0L)
for (nm in paste0("rs_out_", names(rs_dims))) {
  if (!nm %in% names(rs_profile)) rs_profile[[nm]] <- 0L
}

# Follow-up module: pre-HIP status and relationship for a subset of ties.
fu <- read_baseline_cols(c("SubmissionDate", "worker_id", "consent_survey",
  grep("^_jh_fu_name_[0-9]+$", cols_hdr, value = TRUE),
  grep("^jh_fu_(pre_hip|pre_hip_yes|pre_hip_no|relation)_[0-9]+$", cols_hdr,
       value = TRUE))) %>%
  dedup_respondents()

fu_long <- function(df, pat, val) {
  df %>%
    select(worker_id, matches(pat)) %>%
    pivot_longer(-worker_id, names_to = "rep", values_to = val) %>%
    mutate(rep = as.integer(str_extract(rep, "[0-9]+$"))) %>%
    filter(!is.na(.data[[val]]), .data[[val]] != "")
}
fum <- fu_long(fu, "^_jh_fu_name_[0-9]+$", "fname") %>%
  mutate(fname = norm_name(str_remove(fname, "^\\[J?[0-9]+\\]\\s*"))) %>%
  left_join(fu_long(fu, "^jh_fu_pre_hip_[0-9]+$", "pre_hip"),
            by = c("worker_id", "rep")) %>%
  left_join(fu_long(fu, "^jh_fu_relation_[0-9]+$", "relation"),
            by = c("worker_id", "rep")) %>%
  inner_join(choice, by = c("worker_id", "fname" = "cn"))

fu_amb <- fum %>% count(worker_id, rep) %>% filter(n > 1)

fum <- fum %>%
  anti_join(fu_amb, by = c("worker_id", "rep")) %>%
  distinct(worker_id, K, .keep_all = TRUE)
cat("follow-up reps mapped to roster slots:", nrow(fum),
    "| ambiguous dropped:", nrow(fu_amb), "\n")

# Assemble the dyad table: keep pairs where both sides are surveyed and
# not excluded, then join on every outcome and attribute profile.
surveyed <- resp %>% filter(!in_problem)

d <- dyads %>%
  inner_join(surveyed %>% select(worker_id), by = "worker_id") %>%
  inner_join(surveyed %>% select(alter_id = worker_id) %>% mutate(alt_ok = TRUE),
             by = "alter_id") %>%
  left_join(recall_profile, by = c("worker_id", "K")) %>%
  left_join(tie_profile, by = c("worker_id", "K")) %>%
  left_join(rs_profile, by = c("worker_id", "K")) %>%
  left_join(fum %>% select(worker_id, K, pre_hip, relation),
            by = c("worker_id", "K")) %>%
  mutate(across(c(starts_with("gen_"), starts_with("rs_out_"),
                  n_generators, tied_any, recalled), ~replace_na(., 0L)),
         n_layers = n_generators + rowSums(across(starts_with("rs_out_"))))

chars <- resp %>%
  select(worker_id, birth_region, birth_zone, birth_woreda,
         lived_rural, rural_share, sid_speak_n, amh_speak_n, religion,
         resp_gender, training_complete, nb_cohires_n, enum_name, pu_name,
         resurveyed, resurvey_gap_days_n) %>%
  mutate(pu = norm_pu(pu_name), pu_name = NULL)

d <- d %>%
  left_join(chars %>% rename_with(~paste0(., "_i"), -worker_id),
            by = "worker_id") %>%
  left_join(chars %>% rename_with(~paste0(., "_j"), -worker_id) %>%
              rename(alter_id = worker_id), by = "alter_id") %>%
  left_join(roster %>% transmute(worker_id_key,
                                 pu_cases_i = norm_pu(production_unit)),
            by = c("worker_id" = "worker_id_key")) %>%
  left_join(roster %>% transmute(worker_id_key,
                                 pu_cases_j = norm_pu(production_unit)),
            by = c("alter_id" = "worker_id_key"))

# Pair-level derived variables: shared origin at each level, shared
# language / religion / gender, same production unit, and the tie's timing.
d <- d %>% mutate(
  same_woreda = as.integer(is_valid_code(birth_woreda_i) &
                             is_valid_code(birth_woreda_j) &
                             birth_woreda_i == birth_woreda_j),
  same_zone = as.integer(is_valid_code(birth_zone_i) &
                           is_valid_code(birth_zone_j) &
                           birth_zone_i == birth_zone_j),
  same_region = as.integer(is_valid_code(birth_region_i) &
                             is_valid_code(birth_region_j) &
                             birth_region_i == birth_region_j),
  origin_valid = is_valid_code(birth_woreda_i) & is_valid_code(birth_woreda_j),
  conv_sid = as.integer(pmin(sid_speak_n_i, sid_speak_n_j) >= 4),
  conv_amh = as.integer(pmin(amh_speak_n_i, amh_speak_n_j) >= 4),
  lang_barrier = as.integer(pmin(sid_speak_n_i, sid_speak_n_j) < 3 &
                              pmin(amh_speak_n_i, amh_speak_n_j) < 3),
  shared_lang = 1L - lang_barrier,
  both_rural = as.integer(lived_rural_i == "1" & lived_rural_j == "1"),
  same_relig = as.integer(is_valid_code(religion_i) & religion_i == religion_j),
  same_gender = as.integer(resp_gender_i == resp_gender_j),
  pu_known = as.integer(!is.na(pu_i) & !is.na(pu_j)),
  same_pu = if_else(pu_known == 1L, as.integer(pu_i == pu_j), NA_integer_),
  same_pu_mi = coalesce(same_pu, 0L),
  same_pu_cases = if_else(!is.na(pu_cases_i) & !is.na(pu_cases_j),
                          as.integer(pu_cases_i == pu_cases_j), NA_integer_),
  is_family = tied_any == 1L & relation %in% as.character(1:5),
  tied_pre = as.integer(tied_any == 1L &
                          (coalesce(pre_hip, "") == "1" | is_family)),
  tied_post = as.integer(tied_any == 1L & !is_family &
                           coalesce(pre_hip, "") == "0"),
  tied_unknown = as.integer(tied_any == 1L & tied_pre == 0L & tied_post == 0L))

stopifnot(!anyNA(d$tied_pre), !anyNA(d$tied_post),
          all(d$tied_any == d$tied_pre + d$tied_post + d$tied_unknown),
          all(d$tied_any[d$recalled == 0L] == 0L))

cat("recalled dyads:", sum(d$recalled), "of", nrow(d),
    sprintf("(%.1f%%) | tied given recalled: %.1f%%\n",
            100 * mean(d$recalled),
            100 * mean(d$tied_any[d$recalled == 1L])))

# The estimation "batch": one arm per distinct row-set, largest first.
rs_of <- roster %>% select(worker_id_key, rowset)
d <- d %>%
  left_join(rs_of %>% rename(worker_id = worker_id_key, rowset_i = rowset),
            by = "worker_id") %>%
  left_join(rs_of %>% rename(alter_id = worker_id_key, rowset_j = rowset),
            by = "alter_id")

cell_size <- roster %>% count(rowset, name = "rostered")

sample_rs <- tibble(rowset = union(d$rowset_i, d$rowset_j)) %>%
  left_join(cell_size, by = "rowset") %>%
  arrange(desc(rostered), rowset) %>%
  mutate(batch = sprintf("batch_%02d", row_number())) %>%
  select(rowset, batch)

d <- d %>%
  left_join(sample_rs %>% rename(rowset_i = rowset), by = "rowset_i") %>%
  left_join(sample_rs %>% rename(rowset_j = rowset, alter_batch = batch),
            by = "rowset_j") %>%
  mutate(same_batch = as.integer(batch == alter_batch)) %>%
  select(-comp, -alter_comp, -rowset_i, -rowset_j)
stopifnot(!anyNA(d$batch), !anyNA(d$alter_batch))
cat("arms in the estimation sample:", nrow(sample_rs),
    "| dyads within one arm:", sum(d$same_batch),
    sprintf("(%.1f%%)\n", 100 * mean(d$same_batch)))

# Two superseded batch numberings, kept in the crosswalk for reference.
old_rank <- roster %>%
  count(comp, name = "rostered_comp") %>%
  arrange(desc(rostered_comp), comp) %>%
  transmute(comp, batch_pre_20260807 = sprintf("batch_%02d", row_number()))

old_rank_0819 <- roster %>%
  filter(comp %in% roster$comp[roster$rowset %in% sample_rs$rowset]) %>%
  count(comp, name = "rostered_comp") %>%
  arrange(desc(rostered_comp), comp) %>%
  transmute(comp, batch_pre_20260819 = sprintf("batch_%02d", row_number()))

# Firm crosswalk: real firm name -> "Firm A", "Firm B", ... by size.
firm_map <- roster %>%
  filter(!is.na(firm_name)) %>%
  count(firm_name, name = "rostered") %>%
  arrange(desc(rostered), firm_name) %>%
  mutate(firm = paste("Firm", LETTERS[row_number()]))
stopifnot(nrow(firm_map) <= length(LETTERS))
write.csv(firm_map, file.path(dir_derived, "firm_crosswalk.csv"),
          row.names = FALSE)
cat("firm labels:",
    paste(firm_map$firm, "=", firm_map$firm_name, collapse = ", "), "\n")

arm_members <- roster %>%
  distinct(rowset) %>%
  mutate(member = strsplit(rowset, "|", fixed = TRUE)) %>%
  tidyr::unnest(member)

status <- roster %>%
  transmute(member = worker_id_key,
            is_surveyed = toupper(worker_id_key) %in% toupper(resp$worker_id),
            is_problem = toupper(worker_id_key) %in% problem_ids) %>%
  left_join(resp %>% transmute(member = worker_id,
                               has_origin = is_valid_code(birth_woreda)),
            by = "member") %>%
  mutate(has_origin = coalesce(has_origin, FALSE))

### 3. BATCH LABELS -----------------------------------------------------
# Build the batch crosswalk: one row per hiring set, with its coverage
# (rostered / interviewed / usable / typed) and its anonymised firm label.

cell <- roster %>% count(rowset, name = "cell")
xw <- arm_members %>%
  left_join(status, by = "member") %>%
  left_join(roster %>% select(member = worker_id_key, comp, firm_name),
            by = "member") %>%
  group_by(rowset) %>%
  summarise(comp = na.omit(comp)[1],
            firm_name = if (all(is.na(firm_name))) NA_character_
                        else na.omit(firm_name)[1],
            rostered = n(),
            interviewed = sum(is_surveyed),
            excluded = sum(is_surveyed & is_problem),
            usable = sum(is_surveyed & !is_problem),
            typed = sum(is_surveyed & !is_problem & has_origin),
            .groups = "drop") %>%
  left_join(cell, by = "rowset") %>%
  mutate(is_line = cell == rostered,
         coverage = round(typed / rostered, 2)) %>%
  left_join(sample_rs, by = "rowset") %>%
  left_join(old_rank, by = "comp") %>%
  left_join(old_rank_0819, by = "comp") %>%
  left_join(firm_map %>% select(firm_name, firm), by = "firm_name") %>%
  mutate(in_sample = !is.na(batch)) %>%
  arrange(!in_sample, batch, desc(rostered)) %>%
  select(batch, rowset, comp, firm, firm_name, rostered, cell, is_line,
         interviewed, excluded, usable, typed, coverage, in_sample,
         batch_pre_20260807, batch_pre_20260819)
stopifnot(nrow(xw) == n_distinct(roster$rowset))
write.csv(xw, file.path(dir_derived, "batch_crosswalk.csv"), row.names = FALSE)
cat("arms that are closed hiring lines:", sum(xw$is_line), "of", nrow(xw),
    "| in sample:", sum(xw$is_line & xw$in_sample), "of", sum(xw$in_sample),
    "\n")

cat("\nestimation dyads:", nrow(d), "| egos:", n_distinct(d$worker_id),
    "| batches:", n_distinct(d$batch), "of", n_distinct(roster$rowset),
    "row-sets", "| spanning", n_distinct(roster$comp), "components\n")
cat("production unit known on both sides:", sum(d$pu_known),
    sprintf("(%.1f%%)\n", 100 * mean(d$pu_known)))
cat("ties:", sum(d$tied_any), "= pre", sum(d$tied_pre), "+ post",
    sum(d$tied_post), "+ unknown", sum(d$tied_unknown), "\n")
cat("with valid woreda both sides:", sum(d$origin_valid), "\n")

write_parquet(d, file.path(dir_derived, "dyads.parquet"))
cat("wrote", file.path(dir_derived, "dyads.parquet"), "\n")

### 4. MACROS ---------------------------------------------------------
# The build figures the thesis cites (closed hiring lines, cross-batch
# dyads, resurvey shares), as \pn macros.

csz <- roster %>% count(comp, name = "csize")
clo <- roster %>%
  left_join(csz, by = "comp") %>%
  mutate(nb = suppressWarnings(as.integer(nb_cohires))) %>%
  filter(!is.na(nb))
pn_build$ClosedSetPct <- pn_pct(mean(clo$csize == clo$nb + 1))

pn_build$BatchesLine <- pn_int(sum(xw$is_line & xw$in_sample))

pn_build$CrossBatchDyadPct <- pn_pct(mean(d$same_batch == 0L))

bres <- d %>%
  group_by(batch) %>%
  summarise(sr = mean(resurveyed_i), .groups = "drop")
stopifnot(all(bres$sr %in% c(0, 1)))
cat("batches pure on resurvey status:", nrow(bres), "of", nrow(bres),
    "- the batch fixed effect absorbs the recall horizon\n")
pn_build$ResurveyedDyadPct <- pn_pct(mean(d$resurveyed_i))
pn_build$ResurveyRecallGapPp <- pn_pct(
  mean(d$recalled[!d$resurveyed_i]) - mean(d$recalled[d$resurveyed_i]), 1)
pn_build$ResurveyRosterMed <- pn_int(median(d$nb_cohires_n_i[d$resurveyed_i],
                                            na.rm = TRUE))
pn_build$BaselineRosterMed <- pn_int(median(d$nb_cohires_n_i[!d$resurveyed_i],
                                            na.rm = TRUE))

ws <- roster %>%
  group_by(rowset) %>%
  summarise(waves = n_distinct(wave[!is.na(wave)]), .groups = "drop") %>%
  left_join(sample_rs, by = "rowset")
span_batches <- ws$batch[ws$waves > 1 & !is.na(ws$batch)]
if (sum(d$batch %in% span_batches) > 100) {
  warning("wave-spanning arms now reach ", sum(d$batch %in% span_batches),
          " dyads; Appendix C dropped this passage when it was 6")
}
write_pn(pn_build, file.path(dir_pn, "pn_build.tex"))
