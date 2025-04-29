

# setup_project_structure 
# Erstellt ein vollständiges Projektgerüst nach William S. Noble

###      Noble WS (2009) A Quick Guide to Organizing Computational Biology Projects. PLoS Comput
###      Biol 5(7): e1000424. doi:10.1371/journal.pcbi.1000424



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



#' `create_new_project()` erstellt ein neues Projekt im verwendeten Ordner (`getwd()`). 
#' Es werden Verschiedene Ordner und Dateien erstellt:
#'
#' `(data/, doc/, notebooks/, renv/, reports/,scripts/, tests/, .gitignore, .Renviron, .Rproj, README.qmd)`
#' 
#' 
#' @param project_name       String Objekt welches den Namen des Projektes enthält
#' @param base_path          Default; Übernimmt den gesetzten Pfad
#' @param include_renv       Default: TRUE; Erstellt eine Renv Umgebung. 
#'                           Bei FALSE: Erstell diese nicht. Packages können dann 
#'                           aber bei Aktualisierung Gültigkeit verlieren.
#' @param include_gitignore  Logical; Bei = TRUE: Erstellt eine .gitignore Datei. 
#'                           Definiert welche Files nicht  auf Github gepushed werden
#' @param create_readme      Logical; Bei = TRUE: Erstellt eine README Datei.
#'
#' @returns                  Erstellt alle Ordnerstrukturen und relevante Dateien, sowie Umgebungen.
#'
#' @examples                 `create_new_project("Projekt1", include_renv = FALSE)`
#' 
#'
create_new_project <- function(project_name, 
                               base_path = getwd(),
                               include_renv = TRUE,
                               include_gitignore = TRUE,
                               create_readme = TRUE) {
  project_path <- file.path(base_path, project_name)
  if (dir_exists(project_path)) {
    stop("❌ Projektordner existiert bereits: ", project_path)
  }
  
  today <- format(Sys.Date(), "%Y-%m-%d")
  
  # directories ----
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # .Rproj-Datei ----
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  # config.yaml ----
  # Konfigurationsdatei 
  # Erstellt Header
  config <- list(
    project_name = project_name,
    created = today,
    user = "UserName"                                                                  # Adjust here
  )
  write_yaml(config, file.path(project_path, "reports", "config.yaml"))
  
  
  
  
  
  
  
  # read_config.R ----
  # Helper Skript um config.yaml zu lesen
  # Header für Report Template
  read_config_file <- path(project_path, "scripts", "read_config.R")
  if (!file_exists(read_config_file)) {
    writeLines(
      c(
        "# scripts/read_config.R",
        "",
        "read_project_config <- function(config_path = here('reports', 'config.yaml')) {",
        "  if (!file.exists(config_path)) {",
        "    warning('⚠️ config.yaml nicht gefunden, Default-Werte werden genutzt.')",
        "    return(list(project_name = 'Unknown Project', created = Sys.Date(), user = Sys.info()['user']))",
        "  }",
        "  yaml::read_yaml(config_path)",
        "}"
      ),
      con = read_config_file
    )
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  # quarto.yml ----
  # für Quarto Projektstruktur
  quarto_config <- list(
    project = list(
      type = "default",
      `execute-dir` = "project"
      
    )
  )
  
  write_yaml(quarto_config, file.path(project_path, "reports", "_quarto.yml")) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # load_lib.R ----
  # automatisch erstellen basierend auf renv::dependencies()
  lib_file <- path(project_path, "scripts", "load_lib.R")
  if (!file_exists(lib_file)) {
    
    # Vorgeladene Packages, die derzeit regelmäßig verwednet werden. 
    # Relevant auch für die Renv Umgebung.
    default_pkgs <- c(                                                                 # Adjust here
      "car", "conflicted", 
      "fs",
      "ggdist", "ggforce", "gghalves", "ggpp", "ggpubr", "ggsurvfit", "gtsummary",   
      "here", "httr",
      "kableExtra", "knitr", 
      "magrittr", 
      "openxlsx", 
      "pacman", "patchwork",
      "readr", "readxl", "REDCapR", "REDCapTidieR", "rmarkdown", 
      "survival", "survminer",
      "testthat", "tidyverse", "tidymodels",
      "stringi", 
      "yaml" 
      
    )
    # Optional, da es hier Probleme gab bei der Renv Initialisierung
    optional_pkgs <- c(
      "colorblindr"
    )
    
    # Vorlage
    writeLines(
      c(
        "# scripts/load_lib.R",
        "# Lädt alle benötigten Pakete",
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
        "      message('⚠️ Paket konnte nicht installiert werden: ', pkg)",
        "    })",
        "  }",
        "}",
        "",
        "# Hauptpakete installieren und laden",
        "invisible(lapply(required_packages, install_if_missing))",
        "invisible(lapply(required_packages, function(pkg) library(pkg, character.only = TRUE)))",
        "",
        "# Optionale Pakete installieren (keine Fehlermeldung bei Problemen)",
        "invisible(lapply(optional_packages, install_if_missing))",
        "",
        "message('✅ Alle Kernpakete geladen. Optionale Pakete wenn möglich.')"
      ),
      con = lib_file
    )
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Notebook ----
  today <- format(Sys.Date(), "%Y-%m-%d")
  results_today <- path(project_path, "results", paste0(today, "_initial"))
  notebook_today <- path(project_path, "notebooks", paste0(today, "_notizen.qmd"))
  
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
        paste0("# Notizen – ", today),
        "",
        paste0("**Projekt:** ", project_name),
        paste0("**Datum:** ", today),
        paste0("**User:** ", "UserName"),                                 # Adjust here
        paste0("**Session:** ", R.version.string),
        "",
        "## Ziel:",
        "",
        "## Schritte:",
        "",
        "## Beobachtungen:",
        "",
        "## Probleme / Nächste Schritte:"
      ),
      con = notebook_today
    )
  }

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # README ----
  # README file zur Dokumentation von wichtigen Punkten für das Projekt.
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
        paste0("# Projekt: ", project_name),
        "",
        
        
        "## Autor",
        paste0("- Benutzer: UserName"),                                    # Adjust here
        paste0("- Erstellt am: ", today),
        "",
        
        
        "## Übersicht",
        "- `data/`: Roh- und bereinigte Daten",
        "- `scripts/`: Analyse-Skripte",
        "- `results/`: Ergebnisse & Outputs",
        "- `reports/`: Reports",
        "- `notebooks/`: Notizen",
        "- `tests/`: Tests",
        "",
        "",
        "## Dokumentation",
        "# Relevante Formeln etc.",
        "",
        "## R - Setup",
        "```{r}",
        "",
        "```"
      ),
      con = readme_file
    )
    message("✅ README.qmd wurde erstellt.")
  }

  
  
  
  
  
  
  
  
  
  
  
  
  
  # renv ----
  # Renv Umgebung um genutzte Packages einzufrieren. 
  # Nur aktiv bei: -- include_renv = TRUE --
  # Sollte ein neues Package installiert werden müssen,
  # erkennt es RStudio und aktualisiert es normalerweise.
  # Ansonsten mittels:     renv::snapshot() für alles
  #      oder gezielt:     renv::snapshot(packages = "ggpubr")
  if (include_renv) {
    if (requireNamespace("renv", quietly = TRUE)) {
      message("🔒 Initialisiere renv für Projekt: ", project_name)
      renv::scaffold(project = project_path)
      message("📦 Projektstruktur erstellt.")
      
      withr::with_dir(project_path, {
        
        if (file.exists("scripts/load_lib.R")) {
          source("scripts/load_lib.R")
        }
        renv::snapshot(prompt = FALSE)
      })
      
      message("📦 renv.lock aktualisiert für Projekt: ", project_name)
      message("✅ Projekt wurde mit renv erstellt.")
    } else {
      warning("❌ renv Paket nicht installiert. Projekt wird ohne renv erstellt.")
    }
  } else {
    message("✅ Projekt wurde ohne renv erstellt.")
  }
  
  message("✅ Projekt '", project_name, "' wurde erstellt unter: ", project_path)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # .Renviron ----
  # Für API tokens oder sensible andere Schlüssel z.B. REDCap
  renviron_path <- file.path(project_path, ".Renviron")
  if (!file.exists(renviron_path)) {
    writeLines(
      c(
        "# Projektweite Umgebungsvariablen",
        "# Beispiel:",
        "# API_TOKEN=dein_token",
        "# api_url=https:// ..."
      ),
      con = renviron_path
    )
    message("✅ .Renviron-Datei wurde erstellt für API-Keys und Schlüssel")
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Report Template ----
  # Default Template um Bereichte mittels HTML file zu erstellen.
  report_file <- path(project_path, "reports", paste0(project_name, "_report.qmd"))
  if (!file_exists(report_file)) {
    
    # Vorgabe anpassbar
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
        "# Übersicht",
        "",
        "Projekt: `r config$project_name`  ",
        "Erstellt am: `r config$created`  ",
        "Autor: `r config$user`",
        "",
        "## Analyse",
        "",
        "```{r}",
        "#| include: false",
        "",
        "source('M:/Projekte/global_tools/project_starter.R')",
        "source('M:/Projekte/global_tools/functions.R')",
        "source(here('scripts', 'load_lib.R'))",
        "",
        #paste0("cleaned_data <- readRDS(")
        "```"
      ),
      con = report_file
    )
    message("✅ ", project_name, "_report.qmd' wurde erstellt in ", report_file)
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
        "# runall.R – Zentrale Analysepipeline",
        "",
        "",
        "# 1. Setup",
        "source('scripts/load_lib.R')",
        "source('M:/Projekte/global_tools/project_starter.R')",
        "source('M:/Projekte/global_tools/functions.R')",
        "",
        "",
        "# 2. Daten cleanen",
        "source('scripts/cleaning_data.R')",
        "",
        "# cleane Daten einlesen",
        "",
        "cleaned_data <- read_latest_cleaned_data()",
        "names(cleaned_data)",
        "",
        "data_1 <- cleaned_data$__NAME__",
        "data_2 <- cleaned_data$__NAME__",
        "",
        "",
        "",
        "# 3. Analyse-Skripte",
        "# source('scripts/run_deskriptiv.R')",
        "# source('scripts/run_survival.R')",
        "",
        "",
        "if (!requireNamespace('quarto', quietly = TRUE)) install.packages('quarto')",
        paste0("quarto::quarto_render(", project_name, "_report.qmd)"),
        "",
        "",
        "# 4. Ergebnisse speichern",
        "# Ergebnisse können optional nach Bedarf gespeichert werden,", 
        "# z.B. mit openxlsx::write.xlsx()",
        "# oder mit save_cleaned_result()",
        ""
      ),
      con = runall_file
    )
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Tests ----
  # Test File zum Prüfen/ Validieren der Datenqualität, etc. 
  validation_file <- path(project_path, "tests", "validate_input_data.R")
  
  # Default, Anpassbar
  writeLines(
    c(
      "# validate_input_data.R",
      "",
      "",
      "# Einfache Datenprüfungen",
      "# Beispiel: data <- read.csv('data/raw/data.csv')",
      "# str(data)",
      "# summary(data)",
      "# anyDuplicated(data)",
      "# sapply(data, function(x) sum(is.na(x)))"
    ),
    con = validation_file
  )
  
  
  
  
  
  
  # Testthat ----
  # Teststruktur mit testthat integrieren
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
      message("✅ testthat-Skelett erstellt.")
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # .gitignore ----
  if (include_gitignore) {
    gitignore_content <-
      c(
        "# R",
        ".Rhistory",
        ".RData",
        ".Rproj.user",
        "",
        "# renv",           # renv Library
        "renv/library/",
        "renv/python/",
        "renv/staging/",
        "",
        "# Daten",
        "data/raw/",
        "*.xlsx",
        "*.csv",
        "*.rds",
        "",
        "# Ergebnisse",
        "results/",
        "",
        "# HTML/PDF",
        "*.html",
        "*.pdf",
        "",
        "# Sensible Files",
        "scripts/config.R",
        ".Renviron",         # R 
        ".env"               # Python 
      )
    
    writeLines(gitignore_content, file.path(project_path, ".gitignore"))
    
    message("✅ .gitignore wurde erstellt.")
  }


  
  
  
}



