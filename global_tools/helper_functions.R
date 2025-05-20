# Helper functions 
# Path: Projects/global_tools/



# get_global_tool_path() ----

#' Returns the full path to a file in the global_tools directory using the here package.
#'
#' @param filename  String. The filename (with extension) you want the path for.
#'
#' @return          String with the complete path to the file in global_tools.
#'
get_global_tool_path <- function(filename) {
  here("global_tools", filename)
}






# create_results_wd() ---- 

#' Creates (if necessary) and returns the path to a results subfolder,
#' named with the current date and a user-supplied label.
#'
#' @param analysis_type  String. Label for the analysis or result type (e.g., "cleaning", "descriptive").
#'
#' @return               String with the path to the results subfolder.
#'
#' @examples
#' results_dir <- create_results_wd("descriptive")
create_results_wd <- function(analysis_type){
  today <- format(Sys.Date(), "%Y-%m-%d")
  results_dir <- here::here("results", paste0(today, "_", analysis_type))
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  return(results_dir)
}







# create_tests_wd() ----

#' Creates (if necessary) and returns the path to a tests subfolder,
#' named with the current date and a user-supplied label.
#'
#' @param analysis_type  String. Label for the test or validation type.
#'
#' @return               String with the path to the test subfolder.
#'
#' @examples
#' tests_dir <- create_tests_wd("input_validation")
create_tests_wd <- function(analysis_type){
  today <- format(Sys.Date(), "%Y-%m-%d")
  results_dir <- here::here("tests", paste0(today, "_", analysis_type))
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
  return(results_dir)
}






# create_notebook_wd() ----

#' Creates a new Quarto notebook file in the notebooks directory,
#' named with the current date and an optional topic.
#'
#' @param topic   String. Topic or label for the notebook file. Default: "notes".
#'
#' @return        String with the path to the new notebook file.
#'
#' @examples
#' notebook_file <- create_notebook_wd("eda")
create_notebook_wd <- function(topic = "notes") {
  # Define date, name, and path
  today <- format(Sys.Date(), "%Y-%m-%d")
  notebook_name <- paste0(today, "_", topic, ".qmd")
  notebook_path <- here("notebooks", notebook_name)
  
  # Check if folder already exists
  if (!dir.exists(here("notebooks"))) dir.create(here("notebooks"), recursive = TRUE)
  
  if (!file.exists(notebook_path)) {
    # Create notebook template (customize if needed)
    writeLines(
      c(
        paste0("# Notes – ", today),
        "",
        paste0("**Topic:** ", topic),
        paste0("**Project:** ", basename(here::here())),
        paste0("**Date:** ", today),
        paste0("**User:** ", Sys.info()["user"]),
        paste0("**Session:** ", R.version.string),
        "",
        "## Goal(s)",
        "",
        "- ",
        "",
        "## Steps",
        "",
        "- ",
        "",
        "## Observations",
        "",
        "- ",
        "",
        "## Problems / Next Steps",
        "",
        "- "
      ),
      con = notebook_path
    )
    message("✅ Notebook created: ", notebook_path)
  } else {
    message("⚠️ Notebook already exists: ", notebook_path)
  }
  
  return(notebook_path)
}






# save_cleaned_result() ----

#' `save_cleaned_result()` saves a data object in three formats (csv, xlsx, rds)
#' in the folder data/cleaned/.
#'
#' @param data                The data object to save.
#' @param filename_prefix     String, used as the base name for the saved files.
#' @param cleaned_dir         Default: data/cleaned/ directory.
#' @param write_csv2          If TRUE: uses semicolon as CSV separator (write.csv2).
#' @param create_latest_copy  If TRUE: creates a copy named `latest_NAME.rds`.
#'
#' @returns                   Writes three files (xlsx, csv, rds) + `latest_NAME.rds`.
#'
#' @examples                  `save_cleaned_result(data, filename_prefix = "therapy_lines")`
#'
save_cleaned_result <- function(data, 
                                filename_prefix = "cleaned_data", 
                                cleaned_dir = "data/cleaned", 
                                write_csv2 = FALSE,
                                create_latest_copy = TRUE) {
  
  # Ensure directory exists
  if (!dir.exists(cleaned_dir)) dir.create(cleaned_dir, recursive = TRUE)
  
  # Date
  today <- format(Sys.Date(), "%Y-%m-%d")
  
  # File names
  base_name <- paste0(filename_prefix, "_", today)
  path_rds   <- file.path(cleaned_dir, paste0(base_name, "_data.rds"))
  path_csv   <- file.path(cleaned_dir, paste0(base_name, "_data.csv"))
  path_xlsx  <- file.path(cleaned_dir, paste0(base_name, "_data.xlsx"))
  
  # Save files
  saveRDS(data, path_rds)
  
  if (write_csv2) {
    write.csv2(data, path_csv, row.names = FALSE)  # with ; as separator (for Excel on Windows)
  } else {
    write.csv(data, path_csv, row.names = FALSE)   # with , as separator
  }
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
  openxlsx::write.xlsx(data, path_xlsx)
  
  if (create_latest_copy) {
    latest_rds_path <- file.path(cleaned_dir, paste0("latest_", filename_prefix, "_data.rds"))
    file.copy(path_rds, latest_rds_path, overwrite = TRUE)
  }
  
  message("✅ Saved as: ", basename(path_rds), ", ", basename(path_csv), ", ", basename(path_xlsx))
  
  invisible(list(rds = path_rds, csv = path_csv, xlsx = path_xlsx))
}








# read_latest_cleaned_data() ----

#' `read_latest_cleaned_data()` returns a list of all cleaned datasets from the
#' `data/cleaned/` directory, which makes it easier to read them in the `runall.R` file.
#'
#' @param path    Default path: data/cleaned
#'
#' @returns       List of all datasets: access via `cleaned_data_list[["__NAME__"]]`
#'                                           or `cleaned_data_list$__NAME__`
#'
#' @examples
read_latest_cleaned_data <- function(path = file.path("data", "cleaned")) {
  
  if (!dir.exists(path)) {
    warning("⚠️ Directory not found: ", path)
    return(list())
  }
  
  latest_files <- list.files(
    path = path,
    pattern = "^latest_.*_data\\.rds$",
    full.names = TRUE
  )
  
  if (length(latest_files) == 0) {
    warning("⚠️ No files matching 'latest_*.rds' found in ", path)
    return(list())
  }
  
  data_list <- lapply(latest_files, readRDS)
  names(data_list) <- gsub("^latest_(.*)_data\\.rds$", "\\1", basename(latest_files))
  
  message("✅ Datasets loaded:", paste(names(data_list), collapse = ", "))
  return(data_list)
}
