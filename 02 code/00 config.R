# 00 config.R
#
# Shared setup for the whole pipeline. Every other script starts with
# source("00 config.R"), run with "02 code/" as the working directory.
#
# What lives here:
#   - the one path to set (dir_root), and every derived path
#   - the reader for the raw baseline export and the respondent deduplicator
#   - number formatting and the writer behind every \pn macro the thesis cites
#   - one writer for every generated LaTeX table
#   - the join-date parser, the code-to-label crosswalks, and the two
#     tie-level frames the analysis scripts estimate on
#
# ---------------------------------------------------------------------------
# DATA IS NOT INCLUDED IN THIS REPOSITORY.
#
# dir_root <- "<INSERT PATH TO THE DECRYPTED PROJECT FOLDER HERE>"
# ---------------------------------------------------------------------------

### 1. PACKAGES --------------------------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(arrow)
library(fixest)
library(ggplot2)

### 2. PATHS ---------------------------------------------------------------
# dir_root is the only path to set.

dir_root <- "<INSERT PATH TO THE DECRYPTED PROJECT FOLDER HERE>"

if (identical(dir_root, "<INSERT PATH TO THE DECRYPTED PROJECT FOLDER HERE>") ||
    !dir.exists(dir_root)) {
  stop("Set dir_root at the top of \"00 config.R\" to your local copy of the ",
       "decrypted project folder (the folder that contains \"00 data/\"). ",
       "The raw survey data is confidential and is not included in this ",
       "repository - see the header of this file. Without it the pipeline ",
       "cannot run.", call. = FALSE)
}

dir_data <- file.path(dir_root, "00 data")
dir_out <- file.path(dir_root, "03 output", "02 output")

dir_derived <- file.path(dir_out, "00 derived")
dir_desc <- file.path(dir_out, "10 descriptives")
dir_analysis <- file.path(dir_out, "20 analysis")

dir_latex <- file.path(dir_root, "04 latex")
dir_pn <- file.path(dir_latex, "pn")
dir_tex <- file.path(dir_latex, "tables")
dir_fig <- file.path(dir_latex, "figures")

for (d in c(dir_derived, dir_desc, dir_analysis, dir_pn, dir_tex, dir_fig)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# The specific raw files the builders read. 

file_baseline <- file.path(dir_data, "new_autho", "03_clean_data", "baseline_survey_clean.rda")
file_roster <- file.path(dir_data, "hiring_roster", "hiring_roster 071126.csv")
file_cases <- file.path(dir_data, "cases", "cases 071126.csv")
file_problem <- file.path(dir_data, "problem_people.csv")
file_form <- file.path(dir_data, "hip_baseline_v12_amharic_bothtranslations.xlsx")

### 3. HELPERS -------------------------------------------------------------
# Name matching, the survey's special codes, the baseline reader, and the
# rule for keeping one row per respondent.

# Upper-case and collapse whitespace, for matching names across files.
norm_name <- function(x) str_squish(toupper(x))

# TRUE for a real answer. -999 / -888 / -777 are the survey's
# "does not know" / "refuses" / "other" codes, not ordinary missings.
is_valid_code <- function(x) !is.na(x) & !x %in% c("", "-999", "-888", "-777")

# Normalise a free-text production-unit string to a canonical label
# (a line number, or one of the standard departments).
norm_pu <- function(x) {
  u <- str_squish(toupper(x))
  u[u == ""] <- NA
  n <- str_extract(u, "[0-9]+")
  ifelse(!is.na(n), paste0("LINE ", as.integer(n)),
  ifelse(str_detect(u, "CUT"), "CUTTING",
  ifelse(str_detect(u, "QUALIT|QC"), "QUALITY",
  ifelse(str_detect(u, "FINISH"), "FINISHING",
  ifelse(str_detect(u, "PACK"), "PACKING", u)))))
}

# The raw baseline is a very wide export (~8,600 columns) so lazy load
delayedAssign("baseline_raw", local({
  ev <- new.env()
  load(file_baseline, envir = ev)
  d <- ev$df_combined

  # nw_count_1..12 exist twice (two survey arms); take whichever is populated.
  nw_derand <- paste0("nw_count_", 1:12, "...", c(2325:2336))
  nw_wavetwo <- paste0("nw_count_", 1:12, "...", c(8748:8759))
  stopifnot(all(c(nw_derand, nw_wavetwo) %in% names(d)))
  nw <- lapply(1:12, function(k) coalesce(as.numeric(d[[nw_derand[k]]]),
                                          as.numeric(d[[nw_wavetwo[k]]])))
  names(nw) <- paste0("nw_count_", 1:12)
  d <- bind_cols(d[, !grepl("[.]{3}[0-9]+$", names(d)), drop = FALSE],
                 as_tibble(nw))

  # Force C locale so month abbreviations are English regardless of the host.
  lc <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  for (p in list(c("SubmissionDate", "submission_dt"),
                 c("starttime", "start_dt"),
                 c("endtime", "end_dt"))) {
    d[[p[1]]] <- format(d[[p[2]]], "%b %d, %Y %I:%M:%S %p")
  }

  # Coerce every column to a plain character string.
  as_chr <- function(v) {
    if (inherits(v, "POSIXct")) return(format(v, "%b %d, %Y %I:%M:%S %p"))
    if (inherits(v, "Date")) return(format(v, "%b %d, %Y"))
    if (is.logical(v)) {
      return(ifelse(is.na(v), NA_character_, ifelse(v, "1", "0")))
    }
    if (!is.numeric(v)) return(as.character(v))
    o <- rep(NA_character_, length(v))
    k <- !is.na(v)
    i <- k & v == round(v)
    o[i] <- sprintf("%.0f", v[i])
    o[k & !i] <- format(v[k & !i], scientific = FALSE, trim = TRUE)
    o
  }
  d <- as_tibble(lapply(d, as_chr))

  Sys.setlocale("LC_TIME", lc)
  d
}))

baseline_names <- function() names(baseline_raw)

# Pull a set of columns from the baseline, skipping any that do not exist.
read_baseline_cols <- function(cols) {
  as_tibble(baseline_raw[unique(cols[cols %in% names(baseline_raw)])])
}

# One row per worker: consented submissions only, latest submission kept.
dedup_respondents <- function(df) {
  df %>%
    mutate(worker_id = toupper(worker_id),
           .subm_dt = as.POSIXct(SubmissionDate,
                                 format = "%b %d, %Y %I:%M:%S %p",
                                 tz = "UTC")) %>%
    filter(consent_survey == "1") %>%
    arrange(coalesce(.subm_dt,
                     as.POSIXct(0, origin = "1970-01-01", tz = "UTC"))) %>%
    group_by(worker_id) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(-.subm_dt)
}

# Workers flagged for manual resolution, from the two sources that flag them.
problem_csv_ids <- toupper(read_csv(file_problem, show_col_types = FALSE)$worker_id)

delayedAssign("problem_ids", toupper(
  read_baseline_cols(c("worker_id", "jh_data_status")) %>%
    filter(jh_data_status == "invalid_not_resurveyed") %>%
    pull(worker_id)))

# Permutation-test settings used by several scripts.
n_perm_draws <- 2000
perm_p <- function(null, obs) (1 + sum(null >= obs)) / (length(null) + 1)

### 4. MACROS --------------------------------------------------------------
# Number formatting, and write_pn(), which writes every \pnFoo macro file
# the thesis prose cites so that no number is ever typed into the text.

pn_int <- function(x) formatC(round(x), format = "d", big.mark = "{,}")
pn_pct <- function(x, digits = 0) formatC(100 * x, format = "f", digits = digits)
pn_num <- function(x, digits = 3) formatC(x, format = "f", digits = digits)

# Spell small integers as words, otherwise fall back to digits.
pn_word <- function(x, cap = FALSE) {
  w <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
         "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
         "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
  x <- round(x)
  s <- if (x >= 0 && x <= 20) w[x + 1] else pn_int(x)
  if (cap) sub("^(.)", "\\U\\1", s, perl = TRUE) else s
}

# Write a named list of values as \newcommand{\pnName}{...} lines.
# Names in `plain` are written bare; everything else is wrapped in \pn{}
# so it renders red while the thesis is in draft.
write_pn <- function(x, file, plain = character(), script = SCRIPT) {
  stopifnot(is.list(x), !is.null(names(x)), all(names(x) != ""))
  if (anyDuplicated(names(x))) {
    stop("duplicate macro name: ",
         paste(unique(names(x)[duplicated(names(x))]), collapse = ", "))
  }
  bad <- grep("^[A-Za-z]+$", names(x), invert = TRUE, value = TRUE)
  if (length(bad)) {
    stop("macro names must be letters only: ", paste(bad, collapse = ", "))
  }
  if (!all(plain %in% names(x))) {
    stop("plain names not among the macros: ",
         paste(setdiff(plain, names(x)), collapse = ", "))
  }
  body <- ifelse(names(x) %in% plain, unlist(x), sprintf("\\pn{%s}", unlist(x)))
  writeLines(c(
    "% GENERATED FILE",
    sprintf("%% Written by \"%s\" on %s.", script, format(Sys.Date())),
    "% Values are wrapped in \\pn{} so they render as pipeline numbers, except",
    "% design constants, which are guarded but not marked.",
    "% Regenerate with \"99 run all.R\".",
    sprintf("\\newcommand{\\pn%s}{%s}", names(x), body)), file)
  cat("wrote", file, "-", length(x), "macros\n")
}

### 5. TABLES AND FIGURES ------------------------------------------------
# One writer for every generated LaTeX table (write_tabular) and one for
# every figure (save_fig), so each script names itself once and never
# hand-types its own "% GENERATED by" stamp.

tex_esc <- function(x) gsub("([&%$#_])", "\\\\\\1", x)

star <- function(p) {
  if (is.na(p)) ""
  else if (p < 0.01) "***"
  else if (p < 0.05) "**"
  else if (p < 0.1) "*"
  else ""
}

stars <- function(p) {
  ifelse(p < 0.01, "***",
  ifelse(p < 0.05, "**",
  ifelse(p < 0.1, "*", "")))
}

span <- function(lab, vals) {
  sprintf("%s & %s \\\\", lab, paste(vals, collapse = " & "))
}

# Wrap a column header onto several italic lines no wider than `width`.
tex_colhead <- function(h, width = 16) {
  ws <- strsplit(h, " ", fixed = TRUE)[[1]]
  lines <- character(0)
  cur <- ""
  for (w in ws) {
    cand <- if (nchar(cur)) paste(cur, w) else w
    if (nchar(cand) > width && nchar(cur)) {
      lines <- c(lines, cur)
      cur <- w
    } else {
      cur <- cand
    }
  }
  lines <- c(lines, cur)
  sprintf("\\shortstack{%s}",
          paste(sprintf("\\textit{%s}", lines), collapse = "\\\\"))
}

tex_header <- function(labs, lead = "", width = 16) {
  sprintf("%s& %s \\\\", lead,
          paste(sapply(labs, function(z)
            tex_colhead(sprintf("(%s)", z), width = width)), collapse = " & "))
}

tex_stamp <- function(script = SCRIPT) {
  sprintf("%% GENERATED by \"%s\" - do not edit by hand.", script)
}

# Write one tabular. If caption is given it is wrapped in a table float
# with that caption and label; otherwise it is a bare tabular the citing
# .tex file wraps itself.
write_tabular <- function(file, align, header, rows, tail = NULL,
                          caption = NULL, label = NULL, note = NULL,
                          script = SCRIPT) {
  float <- !is.null(caption)
  writeLines(c(
    tex_stamp(script),
    if (float) c("\\begin{table}[h!]", "\\centering",
                 sprintf("\\caption{%s}", caption),
                 sprintf("\\label{%s}", label)),
    sprintf("\\begin{tabular}{%s}", align),
    header, "\\dmidrule",
    rows,
    if (!is.null(tail)) c("\\midrule", tail),
    "\\bottomrule", "\\end{tabular}",
    if (!is.null(note)) sprintf("\\fignote{%s}", note),
    if (float) "\\end{table}"),
    file.path(dir_tex, file))
  cat("wrote", file, "\n")
}

save_fig <- function(name, p, w = 7, h = 4.2) {
  ggsave(file.path(dir_fig, paste0(name, ".pdf")), p, width = w, height = h)
  cat("wrote", name, "\n")
}

### 6. JOIN DATES --------------------------------------------------------
# The hire date lives in a single column that mixes four encodings
# (#dd/mm/yyyy, #m/d/yyyy, Excel serials, #yyyy/mm/dd). clean_join_date()
# resolves it and returns a status flag plus the rejected reading. The
# 555 ambiguous slash dates are split by a fieldwork changeover at row 941.

join_date_cut <- 941

clean_join_date <- function(file = file_cases) {
  cs <- read_csv(file, show_col_types = FALSE, col_types = cols(.default = "c"))
  if (!all(c("worker_id", "Join_date2", "join_date") %in% names(cs))) {
    stop("clean_join_date(): expected worker_id, Join_date2 and join_date in ",
         file)
  }
  excel <- function(x) {
    as.Date(suppressWarnings(as.integer(x)), origin = "1899-12-30")
  }
  cs %>%
    mutate(
      .row = row_number(),
      worker_id = toupper(worker_id),
      raw = str_trim(Join_date2),
      .ddmm = as.Date(sub("^#", "", raw), format = "%d/%m/%Y"),
      .mmdd = as.Date(sub("^#", "", raw), format = "%m/%d/%Y"),
      .slash = str_detect(raw, "^#?\\d{1,2}/\\d{1,2}/\\d{4}$"),
      join_date_status = case_when(
        is.na(raw) | raw == "" ~ "missing",
        !is.na(join_date) & join_date != "" ~ "exact",
        str_detect(raw, "^\\d{5}$") ~ "exact",
        str_detect(raw, "^#?\\d{4}/\\d{1,2}/\\d{1,2}$") ~ "exact",
        .slash & (is.na(.ddmm) | is.na(.mmdd)) ~ "forced",
        .slash ~ "inferred",
        TRUE ~ "unparsed"),
      join_date = case_when(
        join_date_status == "missing" ~ as.Date(NA),
        !is.na(join_date) & join_date != "" ~ excel(join_date),
        str_detect(raw, "^\\d{5}$") ~ excel(raw),
        str_detect(raw, "^#?\\d{4}/\\d{1,2}/\\d{1,2}$") ~
          as.Date(sub("^#", "", raw), format = "%Y/%m/%d"),
        .slash & is.na(.mmdd) ~ .ddmm,
        .slash & is.na(.ddmm) ~ .mmdd,
        .slash & .row <= join_date_cut ~ .ddmm,
        .slash ~ .mmdd,
        TRUE ~ as.Date(NA)),
      join_date_alt = if_else(join_date_status == "inferred",
                              if_else(.row <= join_date_cut, .mmdd, .ddmm),
                              as.Date(NA))) %>%
    select(worker_id, join_date, join_date_status, join_date_alt,
           join_date_raw = raw)
}

### 7. LABELS ------------------------------------------------------------
# Code-to-label crosswalks from the XLSForm "choices" sheet, and the
# fifteen relationship layers named once so every script agrees on them.

get_choice_labels <- function(list_nm) {
  readxl::read_excel(file_form, sheet = "choices") %>%
    filter(list_name == list_nm) %>%
    transmute(code = as.character(value), label = `label::English`) %>%
    filter(!is.na(code), !is.na(label))
}

lab_relationship <- data.frame(
  code = as.character(c(1:8, -999, -888, -777)),
  label = c("Partner/spouse", "Parents", "Siblings", "Aunts and uncles",
            "Children", "Other family members", "Friends", "Acquaintances",
            "Does not know", "Refuses to answer", "Other (specify)"))

relationship_family_codes <- as.character(1:5)

stopifnot(nrow(get_choice_labels("nm_relationship")) == 10)

code_for <- function(lab, tbl) {
  hit <- tbl$code[tbl$label == lab]
  if (length(hit) != 1) {
    stop("code_for('", lab, "'): matched ", length(hit),
         " codes, expected exactly 1")
  }
  hit
}

# The twelve network generators (whom the worker turns to).
generator_labels <- c(
  "1" = "advice personal decision",
  "2" = "borrow money (in)",
  "3" = "large expenses (in)",
  "4" = "job search",
  "5" = "navigate hawassa",
  "6" = "commute",
  "7" = "meet outside work",
  "8" = "call or text",
  "9" = "workplace breaks",
  "10" = "friends",
  "11" = "assistance (in)",
  "12" = "rosca ekub")

# The three risk-sharing questions (who turns to the worker).
rs_out_labels <- c(
  rs_out_borrow = "borrow money (out)",
  rs_out_large_exp = "large expenses (out)",
  rs_out_assisted_3m = "assistance (out)")

generators_expressive <- c(1, 2, 3, 7, 10, 11, 12)
generators_workplace <- c(4, 5, 6, 8, 9)

# The fifteen layers = twelve generators + three risk-sharing directions.
layer_cols <- c(paste0("gen_", 1:12),
                "rs_out_borrow", "rs_out_large_exp", "rs_out_assisted_3m")
layer_labs <- c(unname(generator_labels[as.character(1:12)]),
                unname(rs_out_labels[c("rs_out_borrow", "rs_out_large_exp",
                                       "rs_out_assisted_3m")]))
n_layer <- length(layer_cols)
stopifnot(n_layer == 15, !anyNA(layer_labs))

### 8. TIE FRAMES ------------------------------------------------------
# The two tie-level frames scripts 22 and 23 both estimate on: one row per
# co-hire tie, and one row per general-network alter, with the shared
# derived fields (inherited, same woreda, kin, friend, co-resident).

tie_frame_cohire <- function(tbl) {
  tbl %>%
    filter(tied_any == 1, tied_unknown == 0) %>%
    mutate(inherited = tied_pre == 1,
           same_woreda = if_else(origin_valid, same_woreda == 1, NA),
           kin = relation %in% as.character(1:6),
           friend = relation == "7")
}

tie_frame_gn <- function(tbl) {
  tbl %>%
    mutate(inherited = pre_hip == "1",
           same_woreda = case_when(origin == "2" ~ TRUE,
                                   origin == "3" ~ orig_woreda == birth_woreda_i,
                                   origin == "1" ~ FALSE,
                                   TRUE ~ NA),
           co_resident = living_together == "1",
           kin = relationship %in% as.character(1:6),
           friend = relationship == "7")
}
