# setup project structure
# Path: Projects/global_tools/


# Creates a complete project scaffold following William S. Noble
###      Noble WS (2009) A Quick Guide to Organizing Computational Biology Projects. 
###         PLoS Comput Biol 5(7): e1000424. doi:10.1371/journal.pcbi.1000424


# Packages ----
if (!require("fs")) install.packages("fs")
if (!require("here")) install.packages("here")
if (!require("renv")) install.packages("renv")
if (!require("yaml")) install.packages("yaml")
if (!require("withr")) install.packages("withr")

library(fs)
library(here)
library(yaml)
library(renv)
library(withr)
library(dplyr)





# create_new_project() ----

#' `create_new_project()` creates a new project in the given folder (`getwd()` by default).
#' It generates various folders and files:
#'
#' `(data/, doc/, notebooks/, renv/, reports/, scripts/, tests/, .gitignore, .Renviron, .Rproj, README.qmd)`
#'
#' @param project_name       String. Name of the project.
#' @param base_path          Default: uses the set path.
#' @param include_renv       Default: TRUE. Sets up an isolated renv environment. 
#'                           If FALSE, renv is not used and package consistency cannot be guaranteed over time.
#' @param include_gitignore  Logical. If TRUE: creates a .gitignore file to define which files should not be pushed to GitHub.
#' @param create_readme      Logical. If TRUE: creates a README file for the project.
#'
#' @returns                  Creates the complete folder structure, relevant files, and (optionally) environments.
#'
#' @examples                 `create_new_project("Project1", include_renv = FALSE)`
#'
create_new_project <- function(project_name, 
                               base_path = here::here("Projects"),
                               include_renv = TRUE,
                               include_gitignore = TRUE,
                               create_readme = TRUE) {
  project_path <- file.path(base_path, project_name)
  if (dir_exists(project_path)) {
    stop("❌ Project folder already exists: ", project_path)
  }
  
  today <- format(Sys.Date(), "%Y-%m-%d")
  
  ## directories ----
  dir_create(project_path, recurse = TRUE)

  dirs_to_create <- c(
    "data/raw",
    "data/cleaned",
    "scripts",
    "results",
    "reports",
    "notebooks",
    "doc",
    "tests"
  )
  
  for (d in dirs_to_create) {
    dir_create(path(project_path, d))
  }

  ## .Rproj file ----
  rproj_file <- path(project_path, paste0(project_name, ".Rproj"))
  if (!file_exists(rproj_file)) {
    file_create(rproj_file)
    writeLines(
      c(
        "Version: 1.0",
        "",
        "RestoreWorkspace: No",
        "SaveWorkspace: No",
        "AlwaysSaveHistory: Default",
        "",
        "EnableCodeIndexing: Yes",
        "UseSpacesForTab: Yes",
        "NumSpacesForTab: 2",
        "Encoding: UTF-8",
        "",
        "RnwWeave: knitr",
        "LaTeX: pdfLaTeX"
      ),
      con = rproj_file
    )
  }

  ## config.yaml ----
  # Configuration file with header info
  config <- list(
    project_name = project_name,
    created = today,
    user = "..."                              # Edit as needed
  )
  write_yaml(config, file.path(project_path, "reports", "config.yaml"))

  # read_config.R ----
  # Helper script to read config.yaml; used for the report header
  read_config_file <- path(project_path, "scripts", "read_config.R")
  if (!file_exists(read_config_file)) {
    writeLines(
      c(
        "# scripts/read_config.R",
        "",
        "read_project_config <- function(config_path = here('reports', 'config.yaml')) {",
        "  if (!file.exists(config_path)) {",
        "    warning('⚠️ config.yaml not found, using default values.')",
        "    return(list(project_name = 'Unknown Project', created = Sys.Date(), user = Sys.info()['user']))",
        "  }",
        "  yaml::read_yaml(config_path)",
        "}"
      ),
      con = read_config_file
    )
  }

  ## quarto.yml ----
  # For Quarto project structure
  quarto_config <- list(
    project = list(
      type = "default",
      `execute-dir` = "project"
    )
  )
  write_yaml(quarto_config, file.path(project_path, "reports", "_quarto.yml")) 

  ## load_lib.R ----
  # Automatically created based on renv::dependencies()
  lib_file <- path(project_path, "scripts", "load_lib.R")
  if (!file_exists(lib_file)) {

    # Default packages regularly used (also relevant for renv snapshot)
    default_pkgs <- c(
      "boot", "car", "conflicted", "DataExplorer", 
      "fs", "ggdist", "ggforce", "gghalves", "ggpp", "ggpubr", "ggsurvfit", "gtsummary",   
      "here", "Hmisc", "httr",
      "kableExtra", "knitr", 
      "magrittr", "MASS", "openxlsx", 
      "pacman", "patchwork",
      "readr", "readxl", "REDCapR", "REDCapTidieR", "rmarkdown", 
      "skimr", "survival", "survminer",
      "testthat", "tidyverse", "tidymodels",
      "stringi", 
      "yaml" 
    )
    # Optional packages (can sometimes cause problems with renv initialization)
    optional_pkgs <- c("colorblindr")

    writeLines(
      c(
        "# scripts/load_lib.R",
        "# Loads all required packages",
        "",
        "required_packages <- c(",
        paste0('  "', default_pkgs, '"', collapse = ",\n"),
        ")",
        "",
        "optional_packages <- c(",
        paste0('  "', optional_pkgs, '"', collapse = ",\n"),
        ")",
        "",
        "install_if_missing <- function(pkg) {",
        "  if (!requireNamespace(pkg, quietly = TRUE)) {",
        "    tryCatch({",
        "      install.packages(pkg)",
        "    }, error = function(e) {",
        "      message('⚠️ Package could not be installed: ', pkg)",
        "    })",
        "  }",
        "}",
        "",
        "# Install and load main packages",
        "invisible(lapply(required_packages, install_if_missing))",
        "invisible(lapply(required_packages, function(pkg) library(pkg, character.only = TRUE)))",
        "",
        "# Install optional packages (no error if unavailable)",
        "invisible(lapply(optional_packages, install_if_missing))",
        "",
        "message('✅ All core packages loaded. Optional packages if available.')"
      ),
      con = lib_file
    )
  }

  ## Notebook ----
  results_today <- path(project_path, "results", paste0(today, "_initial"))
  notebook_today <- path(project_path, "notebooks", paste0(today, "_notes.qmd"))
  if (!dir_exists(results_today)) dir_create(results_today)
  if (!file_exists(notebook_today)) {
    writeLines(
      c(
        "---",
        paste0('title: "', project_name, ' – Notebook"'),
        "format:",
        "  html:",
        "    self-contained: true",
        "execute-dir: project",
        "---",
        paste0("# Notes – ", today),
        "",
        paste0("**Project:** ", project_name),
        paste0("**Date:** ", today),
        paste0("**User:** ", "..."),
        paste0("**Session:** ", R.version.string),
        "",
        "## Goal:",
        "",
        "## Steps:",
        "",
        "## Observations:",
        "",
        "## Problems / Next Steps:"
      ),
      con = notebook_today
    )
  }

  
  
  ## README ----
  # Project documentation for key points               (customize as needed)
  if (create_readme) {
    readme_file <- path(project_path, "README.qmd")
    writeLines(
      c(
        "---",
        paste0('title: "', project_name, ' – README"'),
        "format:",
        "  html:",
        "    toc: true",
        "    toc-depth: 2",
        "    number-sections: true",
        "    self-contained: true",
        "    output-file: README.html",
        "---",
        "",
        paste0("# Project: ", project_name),
        "",
        "## Author",
        paste0("- User: ..."),                         # Edit as needed
        paste0("- Created on: ", today),
        "",
        "## Overview",
        "- `data/`: Raw and cleaned data",
        "- `scripts/`: Analysis scripts",
        "- `results/`: Results & outputs",
        "- `reports/`: Reports",
        "- `notebooks/`: Notes",
        "- `tests/`: Tests",
        "",
        "## Documentation",
        "### Relevant formulas, etc.",
        "",
        "## Planning Contents",
        "",
        "### 01 – Background & Literature",
        "- Literature, publications",
        "",
        "### 02 – Research Questions",,
        "    Objectives and hypotheses",

        "",
        "",
        "",
        "### 03 – Statistical Methods",
        "```{r}",
        "",
        "```",
        "",
        "### 04 – Statistical Analysis Plan",
        "",
        "",
        "### 05 – Publication Plan",
        "- Target journals, authors, deadlines",
        ""
      ),
      con = readme_file
    )
    message("✅ README.qmd has been created.")
  }

  
  
  
  # renv ----
  # renv environment to freeze used packages
  # Only active if -- include_renv = TRUE --
  # If a new package is installed, renv will usually recognize it and update.
  # Otherwise use: renv::snapshot() for all, or renv::snapshot(packages = "ggpubr") for one.
  if (include_renv) {
    if (requireNamespace("renv", quietly = TRUE)) {
      message("🔒 Initializing renv for project: ", project_name)
      renv::scaffold(project = project_path)
      message("📦 Project structure created.")
      
      withr::with_dir(project_path, {
        if (file.exists("scripts/load_lib.R")) {
          source("scripts/load_lib.R")
        }
        renv::snapshot(prompt = FALSE)
      })
      
      message("📦 renv.lock updated for project: ", project_name)
      message("✅ Project created with renv.")
    } else {
      warning("❌ renv package not installed. Project created without renv.")
    }
  } else {
    message("✅ Project created without renv.")
  }
  
  message("✅ Project '", project_name, "' created at: ", project_path)

  # .Renviron ----
  # For API tokens or other sensitive keys, e.g. REDCap
  renviron_path <- file.path(project_path, ".Renviron")
  if (!file.exists(renviron_path)) {
    writeLines(
      c(
        "# Project-wide environment variables",
        "# Example:",
        "# API_TOKEN=your_token",
        "# api_url=https:// ..."
      ),
      con = renviron_path
    )
    message("✅ .Renviron file created for API keys and secrets.")
  }

  # Report Template ----
  # Default template for generating reports as HTML files.
  report_file <- path(project_path, "reports", paste0(project_name, "_report.qmd"))
  if (!file_exists(report_file)) {
    writeLines(
      c(
        "---",
        paste0('title: "', project_name, ' – Report"'),
        "format:",
        "  html:",
        "    self-contained: true",
        paste0('    output-file: "', project_name, '_report.html"'),
        "output-dir: null",
        "execute-dir: project",
        "---",
        "",
        "```{r}",
        "#| include: false",
        "library(here)",
        "library(yaml)",
        "source(here('scripts', 'read_config.R'))",
        "config <- read_project_config()",
        "",
        "```",
        "",
        "# Overview",
        "",
        "Project: `r config$project_name`  ",
        "Created on: `r config$created`  ",
        "Author: `r config$user`",
        "",
        "## Analysis",
        "",
        "```{r}",
        "#| include: false",
        "",
        "source('M:/Projects/global_tools/project_starter.R')",
        "source('M:/Projects/global_tools/stats_functions.R')",
        "source(here('scripts', 'load_lib.R'))",
        "",
        "```"
      ),
      con = report_file
    )
    message("✅ ", project_name, "_report.qmd has been created in ", report_file)
  }

  # runall.R Template ----
  runall_file <- path(project_path, "scripts", "runall.R")
  if (!file_exists(runall_file)) {
    get_last_date <- list.files(
      path = file.path(project_path, "data/cleaned"),
      pattern = "cleaned_data_\\d{4}-\\d{2}-\\d{2}\\.rds$",
      full.names = FALSE
    ) %>%
      sort(decreasing = TRUE) %>%
      first() %>%
      stringr::str_extract("\\d{4}-\\d{2}-\\d{2}")

    writeLines(
      c(
        "# runall.R – Central analysis pipeline",
        "",
        "",
        "# 1. Setup",
        "source('scripts/load_lib.R')",
        "source('M:/Projects/global_tools/project_starter.R')",
        "source('M:/Projects/global_tools/stats_functions.R')",          # Adjust function name if needed
        "",
        "",
        "# 2. Data cleaning",
        "source('scripts/cleaning_data.R')",
        "",
        "# Load cleaned data",
        "",
        "cleaned_data <- read_latest_cleaned_data()",
        "names(cleaned_data)",
        "",
        "data_1 <- cleaned_data$__NAME__",
        "data_2 <- cleaned_data$__NAME__",
        "",
        "",
        "# 3. Analysis scripts",
        "# source('scripts/run_descriptive.R')",
        "# source('scripts/run_survival.R')",
        "",
        "",
        "if (!requireNamespace('quarto', quietly = TRUE)) install.packages('quarto')",
        paste0("quarto::quarto_render(", project_name, "_report.qmd)"),
        "",
        "",
        "# 4. Save results",
        "# Results can be saved as needed,", 
        "# e.g. with openxlsx::write.xlsx(),",
        "# or with save_cleaned_result()",
        ""
      ),
      con = runall_file
    )
  }

  # Tests ----
  # Test file for checking/validating data quality, etc.
  validation_file <- path(project_path, "tests", "validate_input_data.R")
  writeLines(
    c(
      "# validate_input_data.R",
      "",
      "",
      "# Simple data checks",
      "# Example: data <- read.csv('data/raw/data.csv')",
      "# str(data)",
      "# summary(data)",
      "# anyDuplicated(data)",
      "# sapply(data, function(x) sum(is.na(x)))"
    ),
    con = validation_file
  )

  # Testthat ----
  # Integrate testthat test structure
  testthat_file <- path(project_path, "tests", "test_data_checks.R")  
  writeLines(
    c(
      "# tests/test_data_checks.R",
      "library(testthat)",
      "",
      "test_that('Data contains no missing values', {",
      "  skip_if_not(file.exists('data/raw/data.csv'))",
      "  data <- read.csv('data/raw/data.csv')",
      "  expect_true(all(complete.cases(data)))",
      "})"
    ),
    con = testthat_file
  )
  message("✅ testthat skeleton created.")

  # .gitignore ----
  if (include_gitignore) {
    gitignore_content <-
      c(
        "# R",
        ".Rhistory",
        ".RData",
        ".Rproj.user",
        "",
        "# renv",
        "renv/library/",
        "renv/python/",
        "renv/staging/",
        "",
        "# Data",
        "data/raw/",
        "*.xlsx",
        "*.csv",
        "*.rds",
        "",
        "# Results",
        "results/",
        "",
        "# HTML/PDF",
        "*.html",
        "*.pdf",
        "",
        "# Sensitive Files",
        "scripts/config.R",
        ".Renviron",         # R 
        ".env"               # Python 
      )
    writeLines(gitignore_content, file.path(project_path, ".gitignore"))
    message("✅ .gitignore has been created.")
  }
}
