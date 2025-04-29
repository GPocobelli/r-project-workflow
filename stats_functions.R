

# Functions ----







## Data specific functions ----




### Register RedCap Funktionen ----


# Generalized function to parse the mapping string into a named vector
parse_mapping <- function(mapping_str) {
  setNames(
    sapply(strsplit(strsplit(mapping_str, "\\|")[[1]], ", "), `[`, 2),
    sapply(strsplit(strsplit(mapping_str, "\\|")[[1]], ", "), `[`, 1)
  )
}



# Generalized function to map variable values to their labels
values_to_labels <- function(df, var) {
  var <- enquo(var) # Capture the variable using tidy evaluation
  
  # Requires data dictionary to be loaded as dd
  mapping_str <- dd %>%
    filter(field_name == quo_name(var)) %>% # Use quo_name() to get the name of the variable
    pull(choices_calculations_or_slider_labels)
  
  mapping <- parse_mapping(mapping_str)
  mapping <- c(mapping, "nd" = "Not done") # Add "nd" = "Not done" to the mapping
  
  df <- df %>%
    mutate(!!var := ifelse(is.na(!!var), NA, mapping[as.character(!!var)])) # Use tidy evaluation to modify the variable in the df
  
  return(df)
}



# Define the var_label function for setting and getting labels
var_label <- function(x, value = NULL) {
  if (is.null(value)) {
    attr(x, "label")
  } else {
    attr(x, "label") <- value
    return(x)
  }
}



# Define function for piping and combine label setting and cleaning
add_labels <- function(df, dict) {
  for (var_name in names(df)) {
    if (var_name %in% dict$field_name) {
      # Extract and clean the label
      var_label_new <- clean_html_css(dict$field_label[dict$field_name == var_name])
      # Set the cleaned label to the variable and update the dataframe
      df[[var_name]] <- var_label(df[[var_name]], var_label_new)
    }
  }
  return(df)
}



# Remove html or css tags and attributes from the label
clean_html_css <- function(label) {
  # Remove HTML tags
  label <- gsub("<[^>]+>", "", label)
  # Optionally, you can add more cleaning rules here if needed
  return(label)
}



# Function to clean the dataframe names
clean_names <- function(df) {
  names(df) <- names(df) %>%
    tolower() %>% # Convert to lowercase
    gsub(" ", "_", .) %>% # Replace spaces with underscores
    gsub("[^[:alnum:]_]", "", .) # Remove special characters, keeping only alphanumeric and underscores
  return(df)
}




# convert Komma in Punkte und ändere in Numeric 
convert_to_numeric <- function(x){
  x <- gsub(",", ".", x)
  as.numeric(x)
  
}




# for easier reading
capitalize_words <- function(x) {
  sapply(x, function(row) {
    paste(sapply(strsplit(row, ",\\s*")[[1]], function(word) {
      ifelse(grepl("^[A-Z]", word), word, paste0(toupper(substring(word, 1, 1)), substring(word, 2)))
    }), collapse = ", ")
  })
}




# for reformatting, considering partial dates
reformat_dates <- function(df, base_names) {
  for (base_name in base_names) {
    month_var <- paste0(base_name, "_month")
    year_var <- paste0(base_name, "_year")
    date_var <- sym(base_name) # Convert to symbol
    
    df <- df %>%
      mutate(
        !!date_var := ifelse(
          !is.na(.data[[month_var]]) & !is.na(.data[[year_var]]) & is.na(.data[[base_name]]),
          paste("nk", .data[[month_var]], .data[[year_var]], sep = "/"),
          format(as.Date(.data[[base_name]], "%Y-%m-%d"), "%d/%m/%Y")
        )
      )
  }
  return(df)
}







# filter unique therapies in column
sort_unique_values <- function(x) {
  sapply(strsplit(x, ",\\s*"), function(y) paste(sort(unique(trimws(y))), collapse = ", "))
}





# combine without NA
paste_without_na <- function(x) {
  return(paste(x[!is.na(x)], collapse = ", "))
}















### Survival Analysis ----





calc_time <- function(date1, date2) {
  as.numeric(round(difftime(date1, date2, units = "days") / (365.25 / 12), digits = 2))
}














# define x-Scale
get_xscale <- function(xscale = "m_y"){
  # calculations for the right scale
  xtrans <- switch(xscale,
                   d_m = 12/365.25,
                   d_y = 1/365.25,
                   m_d = 365.25/12,
                   m_y = 1/12,
                   y_d = 365.25,
                   y_m = 12,
                   1
  )
  return(xtrans)
}














# Get the Survival Table
# Get the Median Years
get_surv_summary <- function(surv_fit_obj, time_unit = "Years") {
  
  # summary object
  tab <- summary(surv_fit_obj)$table
  
    # Get values
      records <- tab["records"]
      events  <- tab["events"]
      median  <- round(tab["median"] / 12, 2)
      lcl     <- round(tab["0.95LCL"] / 12, 2)
      ucl     <- round(tab["0.95UCL"] / 12, 2)
    
    # Output
    data.frame(
      #Records = records,
      #Events  = events,
      #Median  = median,
      #lcl     = lcl,
      #ucl     = ucl,
      Median_survival = paste0(median, "  [", lcl, "–", ucl, "]"),
      check.names = FALSE  # 👈 verhindert Umwandlung des Spaltennamens
    ) %>%
      setNames(paste0("Median\n Survival, ", time_unit,"\n[95%-CI]"))
}














# create table for survival values at specific times 
# Follow up Survival probability (in %)
get_surv_time <- function(surv_fit_obj, times = c(12, 24, 60), time_unit = "Years") {
  # summary object
  surv_summary <- summary(surv_fit_obj, times = times)
  
  time_months <- surv_summary$time
  surv_percent <- round(surv_summary$surv * 100, 2)
  ci_lower <- round(surv_summary$lower * 100, 2)
  ci_upper <- round(surv_summary$upper * 100, 2)
  
  
  # table
  tibble(
    FU_in_Years = time_months / 12,
    Survival =  paste0(surv_percent, "% [", ci_lower, "–", ci_upper, "]")) %>%
    
    rename_with(~ c(paste0("Follow up\nin ",time_unit), 
                    paste0("Survival probability (%),\n[95%-CI]")))
}














# create a Kaplan-Meier Curve
create_surv_plot <- function(data,
                             surv_fit_obj,
                             group_factor_formula = "1",
                             xscale = "m_y", 
                             scale_end, 
                             scale_break = 24,
                             title,
                             title_size = 11, 
                             text_size = 7,
                             risk_table_size = 2.7,
                             tbl2_x,
                             time_unit = "Years") {
  
  # Get the calc for the x-scale
  xtrans <- get_xscale(xscale)
  
  breaksX <- seq(0, scale_end, by = scale_break)
  breaksX2 <- seq(0, scale_end, by = scale_break)
  breaksY <- c(0, 25, 50, 75, 100)
  

  
  plot_obj <- ggsurvplot(surv_fit_obj,
                         data = data,
                         risk.table = "nrisk_cumevents", 
                         surv.median.line = "hv",
                         ggtheme = theme_pubr(),
                         palette = "black", 
                         censor.shape = "ǀ",
                         conf.int = TRUE,
                         xlab = "Years",
                         ylab = "Survival probability (in %)",
                         title = title,
                         risk.table.y.text = TRUE,
                         break.time.by = scale_break,
                         risk.table.fontsize = risk_table_size
  )
  
  
  tbl1 <- get_surv_time(surv_fit_obj, time_unit = time_unit)   # get the Median time
  tbl1 <- as.data.frame(tbl1)
  tbl2 <- get_surv_summary(surv_fit_obj, time_unit = time_unit) # get the Followup Survival Probability in %
  tbl2 <- as.data.frame(tbl2)
  
  
  plot_obj$plot <- plot_obj$plot + 
    scale_x_continuous(breaks = breaksX,
                       labels = round(breaksX * xtrans, 2), expand = c(0.05, 0.1)) + 
    scale_y_continuous(labels = breaksY) +
    theme(axis.text.x = element_text(size = title_size),
          axis.title.x = element_text(size = title_size, face = "bold"),
          axis.text.y = element_text(size = title_size, face = "bold"),
          axis.title.y = element_text(size = title_size, face = "bold"),
          title = element_text(size = title_size, face = "bold")
    ) + theme(legend.position = "none") + 
    ggpp::annotate(geom = "table", x = 0,  y = 0, label = tbl1) + 
    ggpp::annotate(geom = "table", x = tbl2_x,  y = 0, label = tbl2)
  
  plot_obj$table <- plot_obj$table +
    scale_x_continuous(breaks = breaksX2, labels = round(breaksX2 * xtrans, 2), expand = c(0.05, 0.1)) +
    xlab("") +
    theme(axis.text.x = element_text(size = title_size),
          axis.title.y = element_text(size = title_size),
          title = element_text(size = title_size, face = "bold"))
  

  return(plot_obj)
}














