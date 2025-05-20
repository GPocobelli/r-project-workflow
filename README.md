# R Project Workflow Guide

This document outlines a clear and efficient workflow for organizing R projects, inspired by William S. Noble's recommendations.

**Reference:**  
Noble WS (2009) A Quick Guide to Organizing Computational Biology Projects. PLoS Comput Biol 5(7): e1000424
<br>

## Main Folder Layout

### Structure of Projects

``` text
Projects/                             # Parent directory for all projects
├── Projectname_1/
│    └── Projectname_1.Rproj
├── Projectname_2/
├── ...
├── global_tools/                     # Important!
│    ├── project_starter.R
│    ├── helper_functions.R           # Important!
│    └── stats_functions.R            # other global functions
```

<br>

### Detailed Project Subfolder Structure

``` text
Projectname_1/
├── data/
│     ├── raw/                        # Original data (do not modify)
│     └── cleaned/                    # Processed data ready for analysis
├── notebooks/
│     └── YYYY-MM-DD_notes.md         # Project notes and observations
├── results/
│     ├── YYYY-MM-DD_cleaning/
│     └── YYYY-MM-DD_analysis/
├── reports/
├── scripts/
│     ├── runall_script.R             # Central analysis pipeline
│     ├── load_lib.R                  # Load required libraries
│     ├── read_config.R               # Project-specific configurations
│     ├── cleaning_script.R
│     ├── descriptive_stats.R
│     └── other_scripts.R
├── tests/
│     └── validate_input_data.R       # Data validation scripts
├── Projectname_1_report.qmd
├── Projectname_1_report.html
├── .gitignore
├── Projectname_1.Rproj
├── _quarto.yml
├── README.qmd
└── renv.lock                         # Dependency management (if renv is used)
```


# Workflow

## Preparation  
**Ensure the following structure exists:**  
- Parent folder called **`"Path/to/Projects"`**
- Subfolder **`Path/to/Projects/global_tools/`**
- `"global_tools/"` conains the files `"project_starter.R"` & `"helper_functions.R"` (and other relevent files e.g.:`"stats_functions.R"`)
<br>
Set your working directory to the main project folder:  
Working directory should be `"path/to/Projects"`  

```{r}
getwd()                         # current working directory
setwd("your_path_to/Projects")   # if needed
```

<br>
Load global scripts:  

```{r}
source("path_to_folder/global_tools/helper_functions.R")
source("path_to_folder/global_tools/stats_functions.R")
source("path_to_folder/global_tools/project_starter.R") 
```

<br>

## Creating a new Project

Automatically set up a structured project:  
```{r}
create_new_project("Projectname_1")
```
  
Example output: 
✅ README.qmd has been created.  
✅ Project created with renv. or ✅ Project created without renv."  
✅ Project 'Projectname_1' created at: path/to/Projects/Projectname_1  
✅ .Renviron file created for API keys and secrets."  
✅ Projectname_1_report.qmd has been created in /Projects/Projectname_1/reports/Projectname_1_report.qmd  
✅ testthat skeleton created.  
✅ .gitignore has been created.  

<br>

## R Files  
  
### Loading Libraries  
`load_lib.R` will be created at **`Projects/Projectname_1/scripts`**. Should be inserted in the respective project, not globally, to avoid unnecessary loading of multiple packages.  
Load `load_lib.R` script:  
```{r}
source("path/to/Projects/Projectname_1/load.lib.R") 
```

<br>

### Managing Results
Clearly organize result outputs:  
If you use a "**`cleaning_script.R`**", before exporting results, use the following function: **`create_results_wd()`**  
This will automatically create a new new date-stamped folder under "**`Projectname_1/results`**" and save exports there. The file will use your specified label, e.g. "**cleaning**".  

```{r}
# cleaning Data
results_dir <- create_results_wd("cleaning") 

# or analysis: 
results_dir <- create_results_wd("descriptive_analysis") 

# or other names 
results_dir <- create_results_wd("other_label") 
```

<br><br>

**Suggestion for exporting interim datasets:**

```{r}
# results_dir as defined above
openxlsx::write.xlsx(data, file = file.path(results_dir, "result_1_data.csv")) 

openxlsx::write.xlsx(data, file = file.path(results_dir, "data_check_or_other_label.csv")) 
```

<br>

Save fully cleaned datasets ready for further analysis: **`save_cleaned_result()`**. Datasets are then automatically saved in "**`Projectname_1/data/cleaned`**" as `.xlsx, .csv,` and `.rds` files.

```{r}
save_cleaned_result(data, filename_prefix = "cleaned_data") 

# For separator = ";", set: "write_csv2 = TRUE"
save_cleaned_result(data, filename_prefix = "cleaned_data", 
                    write_csv2 = TRUE)
```


**Folder output:**

``` text
data/cleaned/
│    └── cleaned_data_2025-04-11.rds
│    └── cleaned_data_2025-04-11.csv
│    └── cleaned_data_2025-04-11.xlsx
```

<br>

### Secure Management of Keys or Tokens

With the `.Renviron` file you create a template for securely storing sensitive tokens or keys, which you can access via

``` r
api_token <- Sys.getenv("API_TOKEN")
```

<br>

### Integrating GitHub

A `.gitignore` file is created for automatic upload to exclude files containing sensitive information, such as API tokens (saved in `.Renviron`).

**Important:**
Currently excludes `.Rhistory, .RData, .Rproj.user, renv/library, data/raw, results/, .Renviron, .env, PDF, HTML_reports`. Please adapt as needed if you add other files/folders.

<br>

### Project Documentation  
- **README.qmd**  
  For important project documentation, especially for others, a `README` file is included. This is only created once and is a Quarto file, so it can be rendered to PDF or HTML.
  <br>
- **Notebooks**  
  For clean documentation, notes are helpful. Notebooks can be created with the function `create_notebook_wd()`.  
  Default name = "`_notes.qmd`"  
  ``` r
  create_notebook_wd("notes")
  ```
  Automatically places notes into "**`Projectname_1/notebooks`**".

<br>

### Reports

A Quarto report template is automatically created (`Projectname_report.qmd`).
This template includes all necessary links to functions and libraries.  
For reports, the **`.quarto.yml`** file is relevant because it contains basic project/person info.
Must be adapted as needed.

<br>

### Central Pipeline - runall.R

The runall.R script standardizes your analytical workflow:  

<br>

**1. Setup**  
``` r
source('scripts/load_lib.R')
source('M:/Projects/global_tools/project_starter.R')
source('M:/Projects/global_tools/functions.R')
```

**2. Data Cleaning**  
``` r
source('scripts/cleaning_data.R')
```  
Load cleaned data  
`cleaned_data` is a list with the latest files in the path `/data/cleaned/`  
``` r
cleaned_data <- read_latest_cleaned_data(path = here::here("data", "cleaned"))
head(cleaned_data)
data_clean_1 <- cleaned_data$...
data_clean_2 <- cleaned_data$...
names(cleaned_data)
```

**3. Analysis scripts**  

``` r
source('scripts/descriptiv_stats.R')
source('scripts/run_survival.R')
```  

**4. Render Report**  

``` r
if(!requireNamespace('quarto', quietly = TRUE)) install.packages('quarto')
quarto::quarto_render('Some_Report.qmd')
```
