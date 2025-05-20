# Projects
All projects with additionally R workflow

Creates a complete project folder structure according to William S. Noble
Noble WS (2009) A Quick Guide to Organizing Computational Biology Projects. PLoS Comput Biol 5(7): e1000424. doi:10.1371/journal.pcbi.1000424

<br>

# Folder Overview

## Structure of Projects

``` text
Projects/                          # Parent directory for all projects
├── Projectname_1/
│    └── Projectname_1.Rproj
├── Projectname_2/
├── ...
├── global_tools/                  # Important!
│    └── project_starter.R
│    └── helper_functions.R        # other global functions
│    └── stats_functions.R 
```

<br>

## Subdivision of a Project Folder

``` text
Projectname_1/
├── data/
│    └── data/raw/                 # Raw data; should not be changed
│    └── data/cleaned/             # Cleaned data 
├── notebooks/                       
│    └── 2025-04-10_notes.md
├── results/                         
│    └── 2025-04-10_cleaning/
│    └── 2025-04-11_descriptive/
├── reports/                         
├── scripts/
│    └── runall_script.R           # Default
│    └── load_lib.R                # Default
│    └── read_config.R             # Relevant for Projectname_1_report.qmd (report_file)
│    └── cleaning_script.R      
│    └── descriptiv_stats.R
│    └── other_scripts.R
├── tests/                            
│    └── validate_input_data.R 
├── Projectname_1_report.qmd       # Default
├── Projectname_1_report.html
├── .gitignore                       
├── Projectname_1.Rproj              
├── _quarto.yml                    # General info about the project/person (README)
├── README.qmd                       
├── renv.lock                      # If include_renv = TRUE
```


# Workflow

## Preparation

A parent folder called **`"Projects"`** should have been created. Within `"path/to/Projects"` a further folder `"global_tools"` should exist, where the files `"project_starter.R"`, `"helper_functions.R"` and `"stats_functions.R"` are stored.

-   Use `getwd()` to get your current working directory

-   Use `setwd()` if necessary

-   Working directory should be "path/to/Projects"

<br>

```{r}
getwd()
setwd("your_path__/Projects")   # if needed
```

<br>
You can then source the scripts as follows:

```{r}
# For example: global functions 
source("path_to_folder/global_tools/helper_functions.R")
source("path_to_folder/global_tools/stats_functions.R")

# Here is the script with the important functions
source("path_to_folder/global_tools/project_starter.R") 
```

<br>

## Create a Project

fhe following function will automatically create a project folder named "Projectname_1" with all subfolders listed above inside the "Projects" directory.

```{r}
create_new_project("Projectname_1")
```

Output: 
- ✅ README.qmd has been created.
- ✅ Project created with renv. / ✅ Project created without renv."
- ✅ Project 'Projectname_1' created at: path/to/Projects/Projectname_1
- ✅ .Renviron file created for API keys and secrets."
- ✅ Projectname_1_report.qmd has been created in /Projects/Projectname_1/reports/Projectname_1_report.qmd
- ✅ testthat skeleton created.
- ✅ .gitignore has been created.

<br>

## R Files

<br>

### load_lib.R Script

To avoid loading multiple projects' packages globally, it's useful to have a **`load_lib.R`** script inside each project's **`Projects/Projectname_1/scripts`** folder.

```{r}
source("path/to/Projects/Projectname_1/load.lib.R") 
```

<br>

### Working files

In a script such as "**`cleaning_script.R`**", before exporting results, use the following function:

```{r}
# cleaning Data
results_dir <- create_results_wd("cleaning") 

# or analysis: 
results_dir <- create_results_wd("descriptive_analysis") 

# or other names 
results_dir <- create_results_wd("other_label") 
```

This will automatically create a new dated subfolder under "**`Projectname_1/results`**" and save exports there. The file will use your specified label, e.g. "**cleaning**".

<br>

**Suggestion for exporting interim datasets:**

```{r}
# export dataset 
# results_dir as defined above
openxlsx::write.xlsx(data, file = file.path(results_dir, 
                                            "result_1_data.csv")) 

openxlsx::write.xlsx(data, file = file.path(results_dir, 
                                            "data_check_or_other_label.csv")) 

quarto_render("reports/_Label_Report.qmd", 
              output_file = "_Label_Report.html", 
              output_dir = "reports")
```

<br>

If your dataset is fully cleaned and analysis should follow, use the function: **`save_cleaned_result()`**.

Datasets are then automatically saved in "**`Projectname_1/data/cleaned`**" as `.xlsx, .csv,` and `.rds` files.

```{r}
save_cleaned_result(data, filename_prefix = "cleaned_data") 

# For separator = ";", set: "write_csv2 = TRUE"
save_cleaned_result(data, filename_prefix = "cleaned_data", 
                    write_csv2 = TRUE)
```

<br>

**Folder output:**

``` text
data/cleaned/
│    └── cleaned_data_2025-04-11.rds
│    └── cleaned_data_2025-04-11.csv
│    └── cleaned_data_2025-04-11.xlsx
```

<br>

### Sensitive Keys or Tokens

With the `.Renviron` file you create a template for securely storing sensitive tokens or keys, which you can access via

``` r
api_token <- Sys.getenv("API_TOKEN")
```

zu zugreifen.

<br>

### Github

A `.gitignore` file is created for automatic upload to exclude files containing sensitive information, such as API tokens (saved in `.Renviron`).

**Important:**
Currently excludes `.Rhistory, .RData, .Rproj.user, renv/library, data/raw, results/, .Renviron, .env, PDF, HTML_reports`. Please adapt as needed if you add other files/folders.


Push to Github via bash.

<br>

### README

For important project documentation, especially for others, a `README` file is included. This is only created once and is a Quarto file, so it can be rendered to PDF or HTML.

<br>

### Notebooks

For clean documentation, notes are helpful. Notebooks can be created with the function `create_notebook_wd`.\
\
Default name = "`_notes.qmd`"

``` r
create_notebook_wd("notes")
```

This will automatically create a new file with the current date and your chosen label in "**`Projectname_1/notebooks`**".

<br>

### Reports

A Quarto report template is automatically created (`Projectname_report.qmd`).\
This template includes all necessary links to functions and libraries.

``` r
source('M:/Projects/global_tools/project_starter.R') 
source('M:/Projects/global_tools/functions.R') 
source(here('scripts', 'load_lib.R'))
```

For reports, the **`.quarto.yml`** file is relevant because it contains basic project/person info.\
Must be adapted as needed.

<br>

### runall.R – Central Pipeline

Additionally, a "**`runall.R`**" script with the following structure can be used:

<br>

**1. Setup**

``` r
source('scripts/load_lib.R')
source('M:/Projects/global_tools/project_starter.R')
source('M:/Projects/global_tools/functions.R')
```

**2. Clean Data**

``` r
source('scripts/cleaning_data.R')
```

Load cleaned data\
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
