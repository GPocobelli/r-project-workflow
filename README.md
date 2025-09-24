

# R Project Workflow Guide

This document outlines a clear and efficient workflow for organizing R projects and reproducible project, inspired by William S. Noble's recommendations.  
The project structure is created by a comprehensive function `create_new_project()`

<br>

**Reference:**  
```Noble WS (2009) A Quick Guide to Organizing Computational Biology Projects. PLoS Comput Biol 5(7): e1000424```  
<br>


## Structure of Projects

``` text

Projects/                                              # Parent directory for all projects
    ├── Projectname_1/
         ├── data/
             ├── raw/                                  # Original data (do not modify)
             └── cleaned/                              # Processed data ready for analysis
         ├── doc/
         ├── notebook/
             └── YYYY-MM-DD_notes.md                   # Project notes and observations
         ├── results/
             ├── YYYY-MM-DD_cleaning/
             └── YYYY-MM-DD_analysis/
         ├── reports/
             ├── Projectname_1_report.qmd 
         ├── R/
             ├── cleaning/
                    ├── pipeline.R               # central pipeline using R-scripts with functions
                    ├── 1_preparation.R          # R-script with specific functions used in the pipeline
                    ├── 2_transformation.R       # R-script with specific functions used in the pipeline
             ├── load_lib.R                      # Load required libraries
             ├── read_config.R                   # Project-specific configurations
             ├── runall_script.R                 # Optional - Central pipeline
             └── other_scripts.R             
         ├── tests/
             └── testthat/ # Rscripte die alles jeweils testen.
         ├── tmp/
         ├── .gitignore
         ├── .Renviron
         ├── _targets.R                          # Optional - When not needed, central pipeline runall.R is created. 
         ├── README.qmd
         └── Projectname_1.Rproj 

    ├── Projectname_2/
    ├── ...
    ├── globaltools                           # Package - Important!
         ├── DESCRIPTION
         ├── NAMESPACE
         ├── R/
         └── man/

# ----- Optional: -------------------

    ├── global_tools/                         # Important!
         ├── targets_project_setup.R          # Important!
         ├── project_helper_functions.R       # Important!
         └── survival_functions.R             # other global functions

```

<br>
<br>  
<br>

## Overview


 * **renv**:  
   Is used to initialize isolated and mobile packages.  
   Initialization via: `renv::init()` and `renv::snapshot()` to store the  
   version of the installed packages.  
   `sessionInfo()` is used to see all used packages and versions.  
   
 * **Targets‑Outputs**:  
   a `_targets.R`‑Skeleton is created (optional),  
   supported by `gittargets`.  
   Example: `tar_git_init()` and `tar_git_snapshot()`  

 * **Modules**:  
   The function `create_new_project()` uses small helper functions.  
   Each function has a roxgen2 comment block.  

 * **Documentation**:  
   README and Notebook are created, which can be used for  
   important documentation, like references, research questions, objectives,  
   (statistical) methods, without a specific presetting to keep it more general.  

 * **Tests**:  
   A testthat skeleton is created. Here, data checks & pipeline tests  
   can be done.  


<br>
<br>  

# Workflow  

<br>

## Procedure  

### 1. Ensure the following structure exists  
 - Create a parent path called **`"Path/to/Projects"`**
 - Prerequisite:  
   Needed packages: **fs**, **here**, **renv**, **yaml**, **withr**, **gittargets**, **targets** and **tarchetypes**.  
   If missing it will be installed automatically.  

 - Download the package file **`"globaltools"`** and save it in the `"Projects"` folder.  
   Then open **`globaltools.Rproj`**. Then **`Build`** -> **`Install Package`**.  
   The functions are available when loading the package **`"globaltools"`**:  

```{r}
library(globaltools)
```
<br>
<br> 



> **Optional:**  
> Use the R-scripts directly instead of a package.
> - Create a subpath **`Path/to/Projects/global_tools/`**
> - `"global_tools/"` conains the files `"targets_project_setup.R"` & `"helper_functions.R"` (and other relevent files e.g.:`"stats_functions.R"`)
>
>
> Load global scripts:
```{r}
source("path_to_folder/Projects/global_tools/helper_functions.R")
source("path_to_folder/Projects/global_tools/stats_functions.R")
source("path_to_folder/Projects/global_tools/targets_project_setup.R") 
```

<br><br><br>


### 2. Set your working directory to the main project folder:  
  
Working directory should be `"path/to/Projects"`  

```{r}
getwd()                          # current working directory
setwd("your_path_to/Projects")   # if needed
```



<br>
<br>
<br>



### 3. Creating a new Project  

Automatically set up a structured project:  
```{r}
create_new_project("Projectname_1")
```
  
Example output:  
✔ Project directory has been created.  
✔ .Rproj file has been created  
✔ Project was created using renv.  
✔ renv was initialized. Please use `renv::snapshot()` after a package installation.  
✔ _targets.R file has been created  

<br>

Open the newly created `"Projectname_1.Rproj"`, then:
```{r}
targets::tar_make()
```

<br>

```text
More targets commandos:

        | Commandos               | Explanation                               |
        | ----------------------- | ----------------------------------------- |
        | `tar_make()`            | Run pipeline                              |
        | `tar_visnetwork()`      | Visualization of dependencies             |
        | `tar_read(target_name)` | Read single target                        |
        | `tar_load(target_name)` | Load target in workspace                  |
        | `tar_git_snapshot()`    | Save Git snapshot                         |
        | `tar_destroy()`         | Delete everything (`_targets/` folder)    |
```  

<br>

<br>

> **Optional:**  
> targets & renv are ajustable.  
> use i.e.:   
```{r}
create_new_project("Projectname_1", include_renv = FALSE, use_targets = TRUE)
```
> If `"use_targets = FALSE "` then a central pipeline `runall.R` is created (see below).  
> Here it is possible to run all relevant R-scripts.  
> This method is not very efficient, because all scripts have to run all over again. 

<br>
<br>
<br>



 
  
### 4. Loading Libraries  

`load_lib.R` will be created at **`Projects/Projectname_1/scripts`**. Should be inserted in the respective project, not globally, to avoid unnecessary loading of multiple packages.  
Load `load_lib.R` script:

```{r}
source("path/to/Projects/Projectname_1/load.lib.R")
```


<br>
<br>
<br>


### 5. Managing Results  

Clearly organize result outputs:  
If you use a R-script with outputs / results, before exporting, use the following function: **`create_results_wd()`** from `"globaltools"` / `"global_tools/helper_functions.R"`.  
This will automatically create a new new date-stamped folder under "**`Projectname_1/results`**" and save exports there. The file will use your specified label, e.g. "**cleaning**".  

```{r}
# cleaning Data
results_dir <- create_results_wd("cleaning") 

# or analysis: 
results_dir <- create_results_wd("descriptive_analysis") 

# or other names 
results_dir <- create_results_wd("other_label") 
```

<br>
<br>
<br>

### 6. Suggestion for exporting interim datasets  

Save fully cleaned datasets ready for further analysis: **`save_cleaned_result()`** from `"globaltools"` / `"global_tools/helper_functions.R"`.  
Datasets are then automatically saved in "**`Projectname_1/data/cleaned`**" as `.xlsx, .csv,` and `.rds` files.

```{r}
save_cleaned_result(data, filename_prefix = "data_name") 

# For separator = ";", set: "write_csv2 = TRUE"
save_cleaned_result(data, filename_prefix = "data_name", write_csv2 = TRUE)
```


**Folder output:**

``` text
data/cleaned/
    └── data_name_2025-04-11.rds
    └── data_name_2025-04-11.csv
    └── data_name_2025-04-11.xlsx
```

<br>
<br>
<br>

### 7. Secure Management of Keys or Tokens  

With the `.Renviron` file a template is created for securely storing sensitive tokens or keys, which you can access via:  

``` r
api_token <- Sys.getenv("api_token")
api_url <- Sys.getenv("api_url")

# or via:
source(".Renviron")
```

<br><br><br>

### 8. Integrating GitHub 

A `.gitignore` file is created for automatic upload to exclude files containing sensitive information, such as API tokens (saved in `.Renviron`).

**Important:**
Currently excludes `.Rhistory, .RData, .Rproj.user, renv/library, data/raw, results/, .Renviron, .env, PDF, HTML_reports`. Please adapt as needed if you add other files/folders.

<br><br><br>



## Project Documentation  
- **README.qmd**  
  For important project documentation, especially for others, a `README` file is included. This is only created once and is a Quarto file, so it can be rendered to PDF or HTML.
  <br>
- **Notebooks**  
  For clean documentation, notes are helpful. Notebooks can be created with the function `create_notebook_wd()`.  
  Default name = "`YYYY-mm-dd_notes.qmd`"
  
  ``` r
  create_notebook_wd()
  create_notebook_wd("1.note")
  ```
  Automatically places notes into "**`Projectname_1/notebooks`**":
    
  ✔ notebook.qmd has been created: path/to/Projects/Projectname_1/notebooks/2025-09-23_notes.qmd  
  ✔ notebook.qmd has been created: path/to/Projects/Projectname_1/notebooks/2025-09-23_1.note.qmd

  

<br><br><br>

## Reports

A Quarto report template is automatically created (`Projectname_1_report.qmd`).  
This template includes all necessary links to functions and libraries.  

**Important:** It is based on a targets pipeline. So if runall.R pipeline is used, this has to be adjusted!  

For reports, the **`.quarto.yml`** file is relevant because it contains basic project/person info.  
Must be adapted as needed.  

<br><br><br><br>



## Central Pipeline - runall.R

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
cleaned_data <- read_latest_cleaned_data()
head(cleaned_data)
data_clean_1 <- cleaned_data$specific_name
data_clean_2 <- cleaned_data$specific_name
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
