# About this repo

Master's thesis by Maria Romagnoli, supervised by Prof. Grosset-Touba.

This repository holds the analysis code, the generated tables and figures,
the LaTeX source of the thesis, and the reference literature. The data is not included.

---
## Layout

```
01 literature/     Reference papers .
02 code/           The R pipeline. Numbered in run order.
03 output/         The aggregate tables and figures the thesis reports.
                   02 output/10 descriptives/   Empirical Setting tables
                   02 output/20 analysis/       Strategy and Results estimates
04 latex/          The thesis document and its shared generated material.
                   00 thesis/     thesis.tex and one file per section
                   pn/            generated macro files (\pnFoo -> a number)
                   tables/        generated .tex tables
                   figures/       generated PDF figures
                   sources.bib    bibliography
```

---

## The pipeline

`02 code/` runs in ascending numerical order and `99 run all.R` asserts it.

| scripts | what they do |
|---|---|
| `00 config.R` | the one path to set, shared helpers, and every output destination. Sourced by every other script. |
| `01`–`03` | builders: raw survey exports → the derived worker, dyad and general-network datasets |
| `10`–`17` | descriptives: the Empirical Setting tables, figures and prose macros |
| `20`–`24` | analysis: balance, the five channels, the layer-design and multiplexity specifications, and the validity audit |
| `99 run all.R` | runs the lot |

Numbers quoted in the thesis prose are never typed by hand. A script
computes each one and writes a `\pnFoo` macro into `04 latex/pn/`, which
the prose cites; regenerating the pipeline regenerates the macros, so the
text cannot silently drift from the estimates.

### Running it

Set `dir_root` in `02 code/00 config.R`, then, from inside `02 code/`:

```bash
Rscript "99 run all.R"
```

Individual scripts can be run on their own and will locate `00 config.R`
whether they are started from the repository root or from `02 code/`.

**R packages used:** `dplyr`, `tidyr`, `stringr`, `readr`, `arrow`,
`fixest`, `ggplot2`, `readxl`, `sf`, `patchwork`, `scales`.

---

## The thesis document

`04 latex/00 thesis/thesis.tex` is the skeleton; each section is its own
numbered file, numbered in document order. It reads the generated `pn/`,
`tables/` and `figures/` with `../`. `thesis.pdf` is the compiled output.

