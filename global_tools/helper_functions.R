

# Helper Functions ----




# Helper Funktion um zu global tools Pfad zu gelangen 
get_global_tool_path <- function(filename) {
  here("global_tools", filename)
}













# Um die Ergebnisse mit aktuellem Datum und 
# Benennung im richitgen Ordner results/ zu speichern.
create_results_wd <- function(analysis_type){
  today <- format(Sys.Date(), "%Y-%m-%d")
  results_dir <- here::here("results", paste0(today, "_", analysis_type))
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  return(results_dir)
}













# Erstellt eine neue Testdatei
# Nur bei Bedarf notwendig, Initiale Datei reicht auch vielleicht
create_tests_wd <- function(analysis_type){
  today <- format(Sys.Date(), "%Y-%m-%d")
  results_dir <- here::here("tests", paste0(today, "_", analysis_type))
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  return(results_dir)
}













# automatisch eine neue Notizdatei erstellen mit dem richtigen Pfad und Datum
create_notebook_wd <- function(topic = "notizen") {
  #Datum, Name und Pad definieren
  today <- format(Sys.Date(), "%Y-%m-%d")
  notebook_name <- paste0(today, "_", topic, ".qmd")
  notebook_path <- here("notebooks", notebook_name)
  
  # Check ob Ornder bereits existiert
  if (!dir.exists(here("notebooks"))) dir.create(here("notebooks"), recursive = TRUE)
  
  if (!file.exists(notebook_path)) {
    
    # Notebook Vorlage erstellen
    # Anpassbar bei Bedarf
    writeLines(
      c(
        paste0("# Notizen – ", today),
        "",
        paste0("**Thema:** ", topic),
        paste0("**Projekt:** ", basename(here::here())),
        paste0("**Datum:** ", today),
        paste0("**User:** ", Sys.info()["user"]),
        paste0("**Session:** ", R.version.string),
        "",
        "## Ziel(e)",
        "",
        "- ",
        "",
        "## Schritte",
        "",
        "- ",
        "",
        "## Beobachtungen",
        "",
        "- ",
        "",
        "## Probleme / Nächste Schritte",
        "",
        "- "
      ),
      con = notebook_path
    )
    message("✅ Notebook erstellt: ", notebook_path)
  } else {
    message("⚠️ Notebook existiert schon: ", notebook_path)
  }
  
  return(notebook_path)
}













#' `save_cleaned_result()` speichert Ergebnisse in 3 verschiednene Daten (csv, xlsx, rds) im Ordner
#' data/cleaned/ ab.
#' 
#' @param data                 Daten Objekt welches übergeben und gespeichert werden soll. 
#' @param filename_prefix      String Objekt, wobei dann dies den Namen der zu speichernden Datei darstellt.
#' @param cleaned_dir          Default: data/cleaned/ Pfad
#' @param write_csv2           Bei = TRUE: CSV mit seperator ";" 
#' @param create_latest_copy   Bei = TRUE: Erstellt eine Kopiedatei mit dem Namen `latest_NAME.rds`
#'
#' @returns                    Erstellt 3 Dateien (xlsx, csv, rds) + `latest_NAME.rds`
#'
#' @examples                   `save_cleaned_result(data, filename_prefix = "therapy_lines")`
#' 
save_cleaned_result <- function(data, 
                                filename_prefix = "cleaned_data", 
                                cleaned_dir = "data/cleaned", 
                                write_csv2 = FALSE,
                                create_latest_copy = TRUE) {
  
  # Sicherstellen, dass Ordner existiert
  if (!dir.exists(cleaned_dir)) dir.create(cleaned_dir, recursive = TRUE)
  
  # Datum
  today <- format(Sys.Date(), "%Y-%m-%d")
  
  # Dateinamen erzeugen
  base_name <- paste0(filename_prefix, "_", today)
  path_rds   <- file.path(cleaned_dir, paste0(base_name, "_data.rds"))
  path_csv   <- file.path(cleaned_dir, paste0(base_name, "_data.csv"))
  path_xlsx  <- file.path(cleaned_dir, paste0(base_name, "_data.xlsx"))
  
  # Speichern
  saveRDS(data, path_rds)
  
  if (write_csv2) {
    write.csv2(data, path_csv, row.names = FALSE)  # mit ; als Separator (z.B. für Excel unter Windows)
  } else {
    write.csv(data, path_csv, row.names = FALSE)   # mit , als Separator
  }
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  openxlsx::write.xlsx(data, path_xlsx)
  
  if (create_latest_copy) {
    latest_rds_path <- file.path(cleaned_dir, paste0("latest_", filename_prefix, "_data.rds"))
    file.copy(path_rds, latest_rds_path, overwrite = TRUE)
  }
  
  
  message("✅ Gespeichert als: ", basename(path_rds), ", ", basename(path_csv), ", ", basename(path_xlsx))
  
  invisible(list(rds = path_rds, csv = path_csv, xlsx = path_xlsx))
}













#' `read_latest_cleaned_data()` gibt eine Liste mit allen cleaned Datensätzen aus dem Ordner 
#' `data/cleaned/` zurück und erleichtert somit das einlesen im `runall.R` File.
#' 
#' @param path   Default Pfad: data/cleaned
#'
#' @returns      List mit allen Datensäten: Zugriff mittels `cleaned_data_list[["__NAME__"]]`
#'                                                     oder `cleaned_data_list$__NAME__`
#'
#' @examples
read_latest_cleaned_data <- function(path = file.path("data", "cleaned")) {
  
  if (!dir.exists(path)) {
    warning("⚠️ Verzeichnis nicht gefunden: ", path)
    return(list())
  }
  
  
  latest_files <- list.files(
    path = path,
    pattern = "^latest_.*_data\\.rds$",
    full.names = TRUE
  )
  
  
  if (length(latest_files) == 0) {
    warning("⚠️ Keine Dateien mit Muster 'latest_*.rds' gefunden in ", path)
    return(list())
  }
  
  
  data_list <- lapply(latest_files, readRDS)
  names(data_list) <- gsub("^latest_(.*)_data\\.rds$", "\\1", basename(latest_files))
  
  message("✅ Eingelesene Datensätze:", paste(names(data_list), collapse = ", "))
  return(data_list)
}
