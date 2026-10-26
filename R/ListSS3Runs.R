#' Create a data frame with details of SS3 Models
#'
#' Used to import OMs into the MSE framework
#'
#' @param SSDir_Base character. File path to the base case output
#' @param SSDir_Sens character. File path to the directory containing
#' the sensitivities
#'
#' @returns a `data.frame` with details on available models. `Description`
#'   is the change from the base case in the SS3 input files of each run, and
#'   `Include` is `FALSE` for SS3 runs not used as operating models, with the
#'   reason in `Note`.
#' @export
ListSS3Runs <- function(SSDir_Base = "../WCNPOSWO-2023/Final Base-case",
                        SSDir_Sens =file.path(SSDir_Base, 'Sensitivities')) {

  # changes from the base case in the SS3 input files (control, data, starter) of
  # each run. The run 1 and 2 folders are named highM and lowM, but M is 10% lower
  # in run 1 and 10% higher in run 2. The S6 recruitment index is fitted in every run
  Descriptions <- c(
    '01'  = 'M 10% lower',
    '02'  = 'M 10% higher',
    '03'  = 'Steepness 0.70',
    '04'  = 'Steepness 0.81',
    '05'  = 'Steepness 0.99',
    '06'  = 'Maximum size 10% larger',
    '07'  = 'Growth from Sun et al. (2002)',
    '08'  = 'L50 10% higher',
    '09'  = 'L50 10% lower',
    '10'  = 'L50 from Wang et al. (2003)',
    '11'  = 'Drop Vanuatu and Chinese catch',
    '12'  = 'All North Pacific catch',
    '13'  = 'Include catch of unclaimed area',
    '14'  = 'Amin = 1.0',
    '15'  = 'F6 length composition lambda = 0',
    '16'  = 'Double-normal selectivity for F2 (Taiwanese longline)',
    '17'  = 'Add F9 size data (earlier data file; S5 and S8 fitted)',
    '18a' = 'CPUE: S1 and S2 only',
    '18b' = 'CPUE: S3 and S4 only',
    '18c' = 'CPUE: S5 only',
    '18d' = 'CPUE: S7 only',
    '18e' = 'CPUE: S8 only',
    '18f' = 'CPUE: S7 and S8 only',
    '19'  = 'All CPUE (S5 and S8 fitted)'
  )

  # SS3 runs not used as operating models
  Excluded <- c(
    '03' = 'The control file sets steepness to 0.70, but the starter file initialises the parameters from the base case parameter file (steepness 0.9), so the SS3 output is identical to the base case.',
    '05' = 'The SS3 fit estimates an implausibly large stock (R0 and unfished spawning biomass many orders of magnitude larger than the base case), so the stock is unaffected by the catch.',
    '06' = 'The SS3 fit estimates an implausibly large stock (R0 and unfished spawning biomass many orders of magnitude larger than the base case), so the stock is unaffected by the catch.'
  )

  # base case
  df_BC <- data.frame(
    OM          = '00',
    Description = 'Base Case',
    Path        = SSDir_Base,
    stringsAsFactors = FALSE
  )

  # sensitivity models
  ssdir_sens <- list.dirs(SSDir_Sens, recursive = FALSE)
  bn         <- basename(ssdir_sens)
  m          <- regmatches(bn, regexec("^(\\d+)([a-zA-Z]?)_(.*)$", bn))
  num        <- sapply(m, `[`, 2)
  suffix     <- sapply(m, `[`, 3)
  desc       <- sapply(m, `[`, 4)
  desc       <- gsub("_", " ", desc)

  model_num <- sprintf("%02d%s", as.integer(num), suffix)

  df_Sense <- data.frame(
    OM       = model_num,
    Description = desc,
    Path        = ssdir_sens,
    stringsAsFactors = FALSE
  ) |> dplyr::arrange(OM)

  dplyr::bind_rows(df_BC, df_Sense) |>
    dplyr::mutate(Description = dplyr::coalesce(unname(Descriptions[OM]), Description),
                  ReRunSS = FALSE,
                  Import  = TRUE,
                  Include = !OM %in% names(Excluded),
                  Note    = dplyr::coalesce(unname(Excluded[OM]), ''))
}
