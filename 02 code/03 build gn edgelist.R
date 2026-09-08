# 03 build gn edgelist.R
#
# Builder. Constructs the general-network edgelist: one row per
# (respondent, named alter) from the free-nomination network module, with
# the alter's fifteen-layer profile, whether the tie predates HIP, the
# alter's origin and relationship, whether they live together, and whether
# the alter was named as a job informant.
#
# Input:  raw baseline, respondents.parquet
# Output: 00 derived/gn_edgelist.parquet
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# The baseline network columns and the respondent frame.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))

resp <- read_parquet(file.path(dir_derived, "respondents.parquet"))
cols_hdr <- baseline_names()

nw <- read_baseline_cols(c("SubmissionDate", "worker_id", "consent_survey",
  grep("^_nw_q_idx_[0-9]+$", cols_hdr, value = TRUE),
  grep("^nw_(fname|lname|cat)_p[0-9]+_[0-9]+$", cols_hdr, value = TRUE),
  grep("^nw_all_name_[0-9]+$", cols_hdr, value = TRUE),
  grep("^nw_dedup_[0-9]+_[0-9]+$", cols_hdr, value = TRUE),
  grep("^_md[123]_nw_idx_[0-9]+$", cols_hdr, value = TRUE),
  grep(paste0("^md[123]_(origin|orig_region|orig_zone|orig_woreda|orig_kebele|",
              "pre_hip|pre_hip_yes|pre_hip_no|relationship|living_together)_",
              "[0-9]+$"),
       cols_hdr, value = TRUE))) %>%
  dedup_respondents() %>%
  filter(!worker_id %in% problem_ids)
cat("respondents (problem people excluded):", nrow(nw), "\n")

# Which generator each network-question position corresponds to.
qidx <- nw %>%
  select(worker_id, matches("^_nw_q_idx_")) %>%
  pivot_longer(-worker_id, names_to = "pcol", values_to = "gen") %>%
  mutate(P = as.integer(str_extract(pcol, "[0-9]+$"))) %>%
  select(worker_id, P, gen)

### 2. BUILD -----------------------------------------------------------
# One row per (respondent, alter). Collect the names, attach the
# fifteen-layer profile, risk-sharing outflow, job-informant flag, and
# the member-detail fields (origin, pre-HIP, relationship, co-residence).

long_field <- function(df, pat) {
  df %>%
    select(worker_id, matches(pat)) %>%
    pivot_longer(-worker_id, names_to = "col", values_to = "v") %>%
    filter(!is.na(v), v != "")
}

# Alter first / last name / category, keyed by question position P and slot i.
alters <- long_field(nw, "^nw_fname_p[0-9]+_[0-9]+$") %>%
  mutate(P = as.integer(str_match(col, "_p([0-9]+)_")[, 2]),
         i = as.integer(str_extract(col, "[0-9]+$")),
         fname = v) %>%
  select(worker_id, P, i, fname) %>%
  left_join(
    long_field(nw, "^nw_lname_p[0-9]+_[0-9]+$") %>%
      mutate(P = as.integer(str_match(col, "_p([0-9]+)_")[, 2]),
             i = as.integer(str_extract(col, "[0-9]+$")),
             lname = v) %>%
      select(worker_id, P, i, lname),
    by = c("worker_id", "P", "i")) %>%
  left_join(
    long_field(nw, "^nw_cat_p[0-9]+_[0-9]+$") %>%
      mutate(P = as.integer(str_match(col, "_p([0-9]+)_")[, 2]),
             i = as.integer(str_extract(col, "[0-9]+$")),
             cat = v) %>%
      select(worker_id, P, i, cat),
    by = c("worker_id", "P", "i")) %>%
  mutate(S = (P - 1L) * 10L + i,
         full_name = norm_name(paste(fname, coalesce(lname, ""))))
cat("GN alters collected:", nrow(alters), "\n")

# Cross-check the slot formula S against the survey's accumulated name list.
acc <- long_field(nw, "^nw_all_name_[0-9]+$") %>%
  mutate(S = as.integer(str_extract(col, "[0-9]+$")),
         acc_name = norm_name(str_remove(v, "^\\[J?[0-9]+\\]\\s*"))) %>%
  select(worker_id, S, acc_name)
chk <- alters %>% inner_join(acc, by = c("worker_id", "S"))
cat("slot-formula check: matched", nrow(chk), "of", nrow(alters),
    "| name agrees:", sum(chk$full_name == chk$acc_name), "\n")
stopifnot(nrow(chk) == nrow(alters), all(chk$full_name == chk$acc_name))

# The alter's layer profile: the generator they were first named on, plus
# any generators they were tagged on via the dedup grid.
first_gen <- alters %>%
  left_join(qidx, by = c("worker_id", "P")) %>%
  select(worker_id, S, gen)
dedup_gen <- long_field(nw, "^nw_dedup_[0-9]+_[0-9]+$") %>%
  filter(v == "1") %>%
  mutate(P = as.integer(str_match(col, "^nw_dedup_([0-9]+)_")[, 2]),
         S = as.integer(str_extract(col, "[0-9]+$"))) %>%
  filter(S >= 1, S <= 120) %>%
  left_join(qidx, by = c("worker_id", "P")) %>%
  select(worker_id, S, gen)
profile <- bind_rows(first_gen, dedup_gen) %>%
  distinct(worker_id, S, gen) %>%
  mutate(gen = paste0("gen_", gen), val = 1L) %>%
  pivot_wider(names_from = gen, values_from = val, values_fill = 0L)
alters <- alters %>%
  left_join(profile, by = c("worker_id", "S")) %>%
  mutate(n_generators = rowSums(across(starts_with("gen_"))))
cat("multiplexity (n generators per alter):\n")
print(summary(alters$n_generators))

# Risk-sharing outflow: which alters would turn to the respondent for help.
rs_raw <- read_baseline_cols(c("SubmissionDate", "worker_id", "consent_survey",
  grep("^rs_who_(borrow|large_exp|assisted_3m)_[0-9]+$", cols_hdr,
       value = TRUE))) %>%
  dedup_respondents() %>%
  filter(!worker_id %in% problem_ids)

rs_profile <- rs_raw %>%
  select(worker_id, matches("^rs_who_")) %>%
  pivot_longer(-worker_id, names_to = "col", values_to = "v") %>%
  filter(v == "1") %>%
  mutate(dim = str_match(col, "^rs_who_(borrow|large_exp|assisted_3m)_")[, 2],
         S = as.integer(str_extract(col, "[0-9]+$"))) %>%
  filter(S >= 1, S <= 120) %>%
  transmute(worker_id, S, dim = paste0("rs_out_", dim), val = 1L) %>%
  distinct() %>%
  pivot_wider(names_from = dim, values_from = val, values_fill = 0L)
for (nm in paste0("rs_out_", c("borrow", "large_exp", "assisted_3m"))) {
  if (!nm %in% names(rs_profile)) rs_profile[[nm]] <- 0L
}
alters <- alters %>%
  left_join(rs_profile, by = c("worker_id", "S")) %>%
  mutate(across(starts_with("rs_out_"), ~replace_na(., 0L)),
         n_layers = n_generators + rowSums(across(starts_with("rs_out_"))))
cat("risk-sharing outflow nominations on GN alters:",
    sum(alters$rs_out_borrow + alters$rs_out_large_exp +
        alters$rs_out_assisted_3m),
    "| alters with >=1:", sum(alters$n_layers > alters$n_generators), "\n")

# Job informants: alters the respondent named as a source of job information.
info_raw <- read_baseline_cols(c("SubmissionDate", "worker_id", "consent_survey",
  grep("^info_source_[0-9]+$", cols_hdr, value = TRUE))) %>%
  dedup_respondents() %>%
  filter(!worker_id %in% problem_ids)
info_sel <- info_raw %>%
  select(worker_id, matches("^info_source_[0-9]+$")) %>%
  pivot_longer(-worker_id, names_to = "col", values_to = "v") %>%
  filter(v == "1") %>%
  mutate(S = as.integer(str_extract(col, "[0-9]+$"))) %>%
  distinct(worker_id, S)
stopifnot(!any(info_sel$S >= 121))
info_gn <- info_sel %>% filter(S >= 1, S <= 120) %>% mutate(job_informant = 1L)

stopifnot(nrow(anti_join(info_gn, alters, by = c("worker_id", "S"))) == 0)
alters <- alters %>%
  left_join(info_gn, by = c("worker_id", "S")) %>%
  mutate(job_informant = replace_na(job_informant, 0L))
cat("job informants:", sum(alters$job_informant),
    "| egos naming one:", n_distinct(info_gn$worker_id),
    "| egos answering 'none of them':", sum(info_sel$S == 0), "\n")

# Member-detail fields, collected across the three member-detail blocks.
md_attach <- function(prefix) {
  idx <- long_field(nw, paste0("^_", prefix, "_nw_idx_[0-9]+$")) %>%
    mutate(rep = as.integer(str_extract(col, "[0-9]+$")),
           S = suppressWarnings(as.integer(v))) %>%
    select(worker_id, rep, S)
  grab <- function(field) {
    long_field(nw, paste0("^", prefix, "_", field, "_[0-9]+$")) %>%
      mutate(rep = as.integer(str_extract(col, "[0-9]+$"))) %>%
      select(worker_id, rep, !!field := v)
  }
  out <- idx
  for (f in c("origin", "orig_region", "orig_zone", "orig_woreda",
              "orig_kebele", "pre_hip", "pre_hip_yes", "pre_hip_no",
              "relationship", "living_together")) {
    out <- out %>% left_join(grab(f), by = c("worker_id", "rep"))
  }
  out %>% select(-rep)
}
md_all <- bind_rows(md_attach("md1"), md_attach("md2"), md_attach("md3")) %>%
  distinct(worker_id, S, .keep_all = TRUE)
cat("member-detail rows attached:", nrow(md_all), "\n")
alters <- alters %>% left_join(md_all, by = c("worker_id", "S"))
cat("alters with origin info:", sum(!is.na(alters$origin)),
    "| with pre_hip:", sum(!is.na(alters$pre_hip)), "\n")

# Flag alters that look like a duplicate of an earlier-numbered slot.
earlier <- alters %>% select(worker_id, S_early = S, name_early = full_name)
susp <- alters %>%
  inner_join(earlier, by = "worker_id", relationship = "many-to-many") %>%
  filter(S_early < S, full_name == name_early) %>%
  group_by(worker_id, S) %>%
  summarise(merge_into_S = min(S_early), .groups = "drop")
alters <- alters %>%
  left_join(susp, by = c("worker_id", "S")) %>%
  mutate(dup_suspect = !is.na(merge_into_S))
cat("dup_suspect rows:", sum(alters$dup_suspect), "\n")

# Attach the respondent's own characteristics (the "_i" side).
alters <- alters %>%
  left_join(resp %>% select(worker_id, birth_region_i = birth_region,
    birth_zone_i = birth_zone, birth_woreda_i = birth_woreda,
    lived_rural_i = lived_rural, sid_speak_n_i = sid_speak_n,
    amh_speak_n_i = amh_speak_n, resp_gender_i = resp_gender,
    enum_name_i = enum_name), by = "worker_id")

### 3. WRITE ---------------------------------------------------------
# gn_edgelist.parquet.

write_parquet(alters, file.path(dir_derived, "gn_edgelist.parquet"))
cat("wrote", file.path(dir_derived, "gn_edgelist.parquet"),
    "|", nrow(alters), "rows\n")
