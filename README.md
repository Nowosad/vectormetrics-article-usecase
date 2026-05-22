## vectormetrics: use case

This repository contains the code and data supporting the companion use‑case for the vectormetrics publication.

Structure:

- `R/` -- analysis scripts:
  - `01-data_processing.R`: compute built-up area metrics across cities; writes processed `.RData` files into `data/`.
  - `02-extracting_pop_dens.R`: overlay building metrics with population grid; writes `data/correlation_data.RData`.
  - `03-selecting_metrics.R`: exploratory analyses (PCA) used to select metrics for reporting.
  - `04-article_chunks.qmd`: Quarto document containing the tables and figures used in the article.
- `data/` -- raw inputs (footprints, population grids) and the generated `.RData` outputs consumed by the Quarto document.
