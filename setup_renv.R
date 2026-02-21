# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::init(bare = TRUE)

# Install all dependencies
renv::install(c(
  "targets",
  "tarchetypes",
  "tidyverse",
  "httr2",
  "jsonlite",
  "blscrapeR",
  "bbmle",
  "ggplot2",
  "patchwork",
  "scales",
  "fs",
  "cli"
))
