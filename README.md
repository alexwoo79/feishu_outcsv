(Minimal R Shiny app for Feishu Bitable ETL)

This repository contains a translation of `feishu.py` into R and a minimal Shiny app to run the same ETL.

Files added:

- `R/feishu.R`: R functions translated from `feishu.py` (load_config, get_tenant_access_token, fetch_records, records_to_csv, etl_pipeline).
- `app.R`: Minimal Shiny app to upload `config.json`, run the ETL, preview and download the CSV.


Dependencies:

- R packages: jsonlite, httr, dplyr, purrr, readr, tibble, shiny

Run the app locally:

1. Install dependencies in R

```r
install.packages(c("jsonlite", "httr", "dplyr", "purrr", "readr", "tibble", "shiny"))
```

2. From the project root run:

```r
shiny::runApp(".")
```

or open `app.R` in RStudio and click "Run App".

