# 10 descriptives - origins and labels.R
#
# Descriptives. Turns the numeric geography and language codes into
# readable labels and produces the origin / language / dyad-composition
# tables the Empirical Setting section reports.
#
# Feeds: Empirical Setting (worker origins, language, dyad composition);
#        table_dyad_composition.csv is read back by script 16.
#
# Input:  respondents.parquet, dyads.parquet, the XLSForm choices sheet
# Output: 10 descriptives/labels_geo.csv, table_origins_region_zone.csv,
#         table_top_woredas.csv, table_language_fluency.csv,
#         table_dyad_composition.csv
#
# Run this from the repository root, or from inside "02 code/".

### 1. DATA --------------------------------------------------------------
# Respondents, and the dyads with a usable birth woreda on both sides.

if (file.exists("00 config.R")) source("00 config.R") else
  source(file.path("02 code", "00 config.R"))

resp <- read_parquet(file.path(dir_derived, "respondents.parquet")) %>%
  filter(!in_problem)
d <- read_parquet(file.path(dir_derived, "dyads.parquet")) %>%
  filter(origin_valid)

### 2. LABELS -------------------------------------------------------------
# Region, zone and woreda code -> name, from the XLSForm choices sheet.

lab_region <- get_choice_labels("regions")
lab_zone <- get_choice_labels("zones")
lab_woreda <- get_choice_labels("woredas")
lab <- function(x, l) {
  coalesce(l$label[match(as.character(x), l$code)], as.character(x))
}
write.csv(bind_rows(mutate(lab_region, list = "region"),
                    mutate(lab_zone, list = "zone"),
                    mutate(lab_woreda, list = "woreda")),
          file.path(dir_desc, "labels_geo.csv"), row.names = FALSE)

### 3. TABLES ---------------------------------------------------------
# Origins by region and zone, the top woredas, language fluency, and the
# composition of dyads by tie status.

t0a <- resp %>%
  count(birth_region, birth_zone) %>%
  mutate(region = lab(birth_region, lab_region),
         zone = lab(birth_zone, lab_zone),
         share = round(n / sum(n), 3)) %>%
  arrange(desc(n)) %>%
  select(region, zone, n, share)
write.csv(t0a, file.path(dir_desc, "table_origins_region_zone.csv"),
          row.names = FALSE)

t0b <- resp %>%
  filter(is_valid_code(birth_woreda)) %>%
  count(birth_region, birth_woreda) %>%
  mutate(region = lab(birth_region, lab_region),
         woreda = lab(birth_woreda, lab_woreda),
         share = round(n / sum(n), 3)) %>%
  arrange(desc(n)) %>%
  select(region, woreda, n, share)
write.csv(t0b, file.path(dir_desc, "table_top_woredas.csv"), row.names = FALSE)

t0c <- bind_rows(
  resp %>% count(level = sid_speak_n) %>% mutate(language = "Sidamigna"),
  resp %>% count(level = amh_speak_n) %>% mutate(language = "Amharic")) %>%
  mutate(label = c("Not at all", "A little", "Moderately", "Well",
                   "Very well")[level],
         share = round(n / nrow(resp), 3)) %>%
  select(language, level, label, n, share)
write.csv(t0c, file.path(dir_desc, "table_language_fluency.csv"),
          row.names = FALSE)

t0d <- d %>%
  mutate(status = case_when(tied_pre == 1 ~ "tie, pre-HIP or family",
                            tied_post == 1 ~ "tie, formed post-HIP",
                            tied_any == 1 ~ "tie, timing unknown",
                            TRUE ~ "no tie")) %>%
  group_by(status) %>%
  summarise(n = n(),
            share_same_woreda = round(mean(same_woreda), 3),
            share_same_zone = round(mean(same_zone), 3),
            share_shared_lang = round(mean(shared_lang), 3),
            n_pu_known = sum(pu_known == 1),
            share_same_pu = round(mean(same_pu, na.rm = TRUE), 3),
            .groups = "drop")
write.csv(t0d, file.path(dir_desc, "table_dyad_composition.csv"),
          row.names = FALSE)

cat("labeled descriptives written\n")
print(head(t0a, 8))
print(head(t0b, 5))
print(t0d)

stopifnot(lab_region$label[lab_region$code == "10"] == "Sidama",
          lab_zone$label[lab_zone$code == "133"] ==
            "Hawassa City Administration")
