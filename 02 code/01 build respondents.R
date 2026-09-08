# 01 build respondents.R
#
# Builder. Turns the raw baseline export into one row per consented
# respondent, with the derived worker-level variables the rest of the
# pipeline uses (ages, durations, fluency, education, roster length, and
# the hire date recovered from the cases file).
#
# Input:  raw baseline survey, cases file, problem_people.csv  (see 00 config.R)
# Output: 00 derived/respondents.parquet
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA ----------------------------------------------------------------
# Read the baseline columns needed for one row per consented respondent.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))

resp_cols <- c(
  "SubmissionDate", "starttime", "worker_id", "caseid", "enum_name",
  "consent_survey",
  "resp_gender", "resp_age",
  "birth_region", "birth_zone", "birth_woreda",
  "lived_rural", "rural_years", "rural_months",
  "curr_residence", "res_kebele",
  "lived_hawassa", "hawassa_years", "hawassa_months",
  "amh_understand", "amh_speak", "sid_understand", "sid_speak",
  "religion",
  "education", "marital_status", "hh_size", "has_children",
  "training_complete", "pu_name",
  "nb_cohires", "jh_miss_any", "jh_miss_count",
  "jh_data_status", "resurvey_gap_days")

r <- read_baseline_cols(resp_cols) %>% dedup_respondents()
cat("consented respondents:", nrow(r), "\n")

### 2. BUILD -------------------------------------------------------------
# Derive the worker-level variables: numeric versions of the survey codes,
# rural / Hawassa durations as shares and years, fluency, an education
# threshold, marital status, and household composition.

r <- r %>%
  mutate(
    in_problem = worker_id %in% problem_ids,
    resurveyed = jh_data_status == "fixed",
    resurvey_gap_days_n = suppressWarnings(as.numeric(resurvey_gap_days)),
    resp_age_n = suppressWarnings(as.integer(resp_age)),
    nb_cohires_n = suppressWarnings(as.integer(nb_cohires)),
    rural_years_n = suppressWarnings(as.numeric(rural_years)),
    rural_months_n = suppressWarnings(as.numeric(rural_months)),
    rural_years_n = ifelse(rural_years_n < 0, NA, rural_years_n),
    rural_months_n = ifelse(rural_months_n < 0, NA, rural_months_n),
    rural_share = ifelse(lived_rural == "0", 0,
      (coalesce(rural_years_n, 0) + coalesce(rural_months_n, 0) / 12) / resp_age_n),
    rural_share = pmin(rural_share, 1),
    hawassa_years_n = suppressWarnings(as.numeric(hawassa_years)),
    hawassa_months_n = suppressWarnings(as.numeric(hawassa_months)),
    hawassa_years_n = ifelse(hawassa_years_n < 0, NA, hawassa_years_n),
    hawassa_months_n = ifelse(hawassa_months_n < 0, NA, hawassa_months_n),
    yrs_hawassa = ifelse(lived_hawassa == "0", 0,
      coalesce(hawassa_years_n, 0) + coalesce(hawassa_months_n, 0) / 12),
    sid_speak_n = suppressWarnings(as.integer(sid_speak)),
    amh_speak_n = suppressWarnings(as.integer(amh_speak)),
    sid_speak_n = ifelse(sid_speak_n < 0, NA, sid_speak_n),
    amh_speak_n = ifelse(amh_speak_n < 0, NA, amh_speak_n),
    education_n = suppressWarnings(as.integer(education)),
    education_n = ifelse(education_n < 0, NA, education_n),
    educ_grade10 = as.integer(education_n >= 4),
    marital_n = suppressWarnings(as.integer(marital_status)),
    marital_n = ifelse(marital_n < 0, NA, marital_n),
    married = as.integer(marital_n %in% c(1, 6)),
    hh_size_n = suppressWarnings(as.integer(hh_size)),
    hh_size_n = ifelse(hh_size_n < 0, NA, hh_size_n),
    has_children_n = ifelse(is_valid_code(has_children),
                            as.integer(has_children == "1"), NA_integer_))

# Count how many co-hire names the worker's preloaded roster listed.
roster_n <- read_baseline_cols(c("SubmissionDate", "worker_id", "consent_survey",
    grep("^ch_name_[0-9]+$", baseline_names(), value = TRUE))) %>%
  dedup_respondents() %>%
  select(worker_id, matches("^ch_name_")) %>%
  pivot_longer(-worker_id, names_to = "slot", values_to = "cn") %>%
  filter(!is.na(cn), cn != "") %>%
  count(worker_id, name = "n_roster")

r <- r %>% left_join(roster_n, by = "worker_id")
cat("roster shown: median", median(r$n_roster[!r$in_problem], na.rm = TRUE),
    "| range", paste(range(r$n_roster[!r$in_problem], na.rm = TRUE),
                     collapse = "-"),
    "| egos with no preload:", sum(is.na(r$n_roster[!r$in_problem])), "\n")

# Attach the hire date from the cases file (see clean_join_date in
# 00 config.R). Where the recovered date sits after the interview but the
# rejected reading does not, swap them and flag the row as "bounded";
# where it still sits after the interview, drop it as "implausible".
r <- r %>%
  mutate(interview_date = as.Date(as.POSIXct(starttime,
                                             format = "%b %d, %Y %I:%M:%S %p",
                                             tz = "UTC"))) %>%
  left_join(clean_join_date(), by = "worker_id") %>%
  mutate(
    join_date_status = coalesce(join_date_status, "not in cases"),
    .swap = join_date_status == "inferred" & !is.na(join_date) &
            join_date > interview_date & !is.na(join_date_alt) &
            join_date_alt <= interview_date,
    .jd_swapped = if_else(.swap, join_date_alt, join_date),
    .alt_swapped = if_else(.swap, join_date, join_date_alt),
    join_date = .jd_swapped,
    join_date_alt = .alt_swapped,
    join_date_status = if_else(.swap, "bounded", join_date_status),
    join_date_status = if_else(!is.na(join_date) & join_date > interview_date,
                               "implausible", join_date_status),
    join_date = if_else(join_date_status == "implausible", as.Date(NA), join_date),
    days_to_interview = as.numeric(interview_date - join_date),
    days_to_interview_alt = as.numeric(interview_date - join_date_alt)) %>%
  select(-.swap, -.jd_swapped, -.alt_swapped)

cat("\njoin dates, analysis sample:\n")
print(table(r$join_date_status[!r$in_problem]))
cat(sprintf("dated: %d of %d | median days hire to interview: %.0f\n",
            sum(!is.na(r$join_date[!r$in_problem])), sum(!r$in_problem),
            median(r$days_to_interview[!r$in_problem], na.rm = TRUE)))

cat("excluded (jh_data_status invalid_not_resurveyed):", sum(r$in_problem), "\n")
cat("recovered by the networking fix-up (jh_data_status fixed):",
    sum(r$resurveyed), "\n")
cat("of the", length(problem_csv_ids), "ids in problem_people.csv,",
    sum(problem_csv_ids %in% r$worker_id[r$resurveyed]),
    "are back in the sample\n")
cat("resurvey gap, days: median",
    median(r$resurvey_gap_days_n[r$resurveyed], na.rm = TRUE), "| range",
    paste(range(r$resurvey_gap_days_n[r$resurveyed], na.rm = TRUE),
          collapse = "-"), "\n")
cat("roster shown, resurveyed vs not: median",
    median(r$n_roster[r$resurveyed], na.rm = TRUE), "vs",
    median(r$n_roster[!r$in_problem & !r$resurveyed], na.rm = TRUE), "\n")
cat("analysis sample:", sum(!r$in_problem), "\n")

for (v in c("birth_region", "birth_zone", "birth_woreda")) {
  cat(sprintf("%s: valid %d of %d\n", v, sum(is_valid_code(r[[v]])), nrow(r)))
}

### 3. WRITE -----------------------------------------------------------
# respondents.parquet, the frame every downstream script starts from.

write_parquet(r, file.path(dir_derived, "respondents.parquet"))
cat("wrote", file.path(dir_derived, "respondents.parquet"), "\n")
