# Import the operating models from SS3 Output


# ---- Setup ----

library(MSEtool)
source('analysis/00-Specifications.R')


## ---- Paths ----
ssdir_base

sens_dir  <- here::here('data-raw', 'assessment', 'Sensitivities')
sens_xlsx <- file.path(sens_dir, 'Sensitivity analysis_SWO.xlsx')

om_dir      <- here::here('objects', 'OM')
sens_om_dir <- file.path(om_dir, 'Sensitivities')

if (!dir.exists(sens_om_dir)) dir.create(sens_om_dir, recursive = TRUE)


# ---- Import Base-Case Operating Model ----

OM_Base <- MSEtool::ImportSS(
  SSDir       = ssdir_base,
  Name        = Name,
  nSim        = nSim,
  pYear       = pYear,
  Region      = Region,
  StockName   = StockName,
  CommonName  = 'Swordfish',
  Species     = Species,
  Interval    = Interval,
  DataLag     = DataLag,
  MPStartYear = MPStartYear
)

Hist <- Simulate(OM_Base)

MSEtool::Save(OM_Base, file.path(om_dir, 'Base.om'), overwrite = TRUE)


# ---- Build Sensitivity Run Manifest ----

## ---- Read run table from xlsx ----

# Main table: RUN (1-19) / NAME / DESCRIPTION, with category header rows
# and a blank spacer row interspersed (RUN, NAME and DESCRIPTION all
# blank/non-numeric on those rows)
main_table <- readxl::read_excel(sens_xlsx, range = "B3:F28",
                                 col_names = c('run', 'x1', 'name', 'x2', 'description'),
                                 col_types = 'text') |>
  dplyr::select(run, name, description)

# Forward-fill category headers (rows where NAME and DESCRIPTION are both
# blank) onto the data rows that follow them, then drop the header/spacer
# rows themselves
main_table <- main_table |>
  dplyr::mutate(category = ifelse(is.na(name) & is.na(description), run, NA_character_)) |>
  tidyr::fill(category) |>
  dplyr::filter(!is.na(name))

# Run 18 ("Single CPUE scenario") has six sub-variants (18a-18f) listed in a
# separate lookup table (RUN / DESCRIPTION only) rather than the main table
run18_table <- readxl::read_excel(sens_xlsx, range = "B30:D35",
                                  col_names = c('run', 'x1', 'description'),
                                  col_types = 'text') |>
  dplyr::select(run, description) |>
  dplyr::mutate(name     = main_table$name[main_table$run == '18'],
                category = main_table$category[main_table$run == '18'])

sensitivity_manifest <- main_table |>
  dplyr::filter(run != '18') |>
  dplyr::bind_rows(run18_table) |>
  dplyr::mutate(run_num = as.integer(sub('[a-z]$', '', run))) |>
  dplyr::arrange(run_num, run) |>
  dplyr::select(-run_num)


## ---- Match runs to SS3 directories ----

run_dirs <- list.dirs(sens_dir, recursive = FALSE)
run_ids  <- sub('^([0-9]+[a-z]?)_.*', '\\1', basename(run_dirs))

dir_lookup <- data.frame(run = run_ids, run_dir = run_dirs, stringsAsFactors = FALSE)

sensitivity_manifest <- sensitivity_manifest |>
  dplyr::left_join(dir_lookup, by = 'run')

# Every row in the xlsx table must resolve to an SS3 directory, and every
# SS3 directory must be described by a row in the xlsx table
missing_dirs <- sensitivity_manifest$run[is.na(sensitivity_manifest$run_dir)]
if (length(missing_dirs) > 0) {
  stop('No SS3 directory found for sensitivity run(s): ',
       paste(missing_dirs, collapse = ', '))
}

unmatched_dirs <- dir_lookup$run_dir[!dir_lookup$run %in% sensitivity_manifest$run]
if (length(unmatched_dirs) > 0) {
  stop('SS3 directory(ies) do not match any row in the sensitivity table: ',
       paste(unmatched_dirs, collapse = ', '))
}




# ---- Import Sensitivity Operating Models ----

for (i in seq_len(nrow(sensitivity_manifest))) {

  run_i <- sensitivity_manifest[i, ]

  OM_i <- MSEtool::ImportSS(
    SSDir      = run_i$run_dir,
    Name       = paste(Name, '-', run_i$description),
    nSim       = nSim,
    pYear      = pYear,
    Agency     = Agency,
    Author     = Author,
    Email      = Email,
    Region     = Region,
    StockName  = Name,
    CommonName = 'Swordfish',
    Species    = Species,
    Interval   = Interval,
    DataLag    = DataLag
  )

  om_filename <- paste0(gsub(' ', '_', basename(run_i$run_dir)), '.om')
  MSEtool::Save(OM_i, file.path(sens_om_dir, om_filename), overwrite = TRUE)
}


# ---- Save Run Manifest ----

sensitivity_manifest$om_file <- file.path(
  'Sensitivities',
  paste0(gsub(' ', '_', basename(sensitivity_manifest$run_dir)), '.om')
)

write.csv(sensitivity_manifest,
          file.path(om_dir, 'sensitivity_run_manifest.csv'),
          row.names = FALSE)
