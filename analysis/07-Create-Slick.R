# Build the Slick object for results visualisation
library(wcpfcNSWO)

MSEDir  <- 'objects/MSE'
TuneDir <- 'objects/Tune'
FigDir  <- 'analysis/figures/Slick'

# Reference levels of the candidate management objectives
source('analysis/00a-PM-Specifications.R')

# CMPs in the Slick object (NULL for all CMPs tuned by 05d-Tune-CMPs.R)
SelectedCMPs <- NULL

TunedCMPs       <- readRDS(file.path(TuneDir, 'TunedCMPs.rds'))
TuneSummary     <- utils::read.csv(file.path(TuneDir, 'Summary.csv'))
ConfigsFile     <- file.path(TuneDir, 'Configs.rds')
SelectedConfigs <- if (file.exists(ConfigsFile)) readRDS(ConfigsFile) else list()

# ---- MP metadata ----
MP_all <- names(TunedCMPs)
if (!is.null(SelectedCMPs)) {
  if (length(setdiff(SelectedCMPs, MP_all)))
    cli::cli_abort("Not tuned CMPs: {.val {setdiff(SelectedCMPs, MP_all)}}.")
  MP_all <- SelectedCMPs
}
nMP    <- length(MP_all)
MPInfo <- TuneSummary[match(MP_all, TuneSummary$Name), ]
MPInfo$FTarget <- paste0('F', round(100 * MPInfo$FTargetSPR))
MPInfo$Group   <- paste(MPInfo$MP, MPInfo$IndexSet, sep = '_')
MPFTarget      <- stats::setNames(MPInfo$FTarget, MP_all)

# CMPs are saved in groups (MP and index set) by 06-Run-Projections.R
Groups <- split(MP_all, factor(MPInfo$Group, unique(MPInfo$Group)))

MP_Type <- c(IR = 'Index Rate', IT = 'Index Target', SP = 'Surplus Production')
IndexSetLabel <- c(JPN = 'JPN', JPNUS = 'JPN + US', JPNTWN = 'JPN + TWN', All = 'JPN + US + TWN')
IndexSetDescription <- c(
  JPN    = 'Japanese late longline CPUE (Areas 1 and 2; S2 and S4)',
  JPNUS  = 'Japanese late longline CPUE (S2 and S4) and US shallow-set longline CPUE (S8)',
  JPNTWN = 'Japanese late longline CPUE (S2 and S4) and Taiwanese longline CPUE (S5)',
  All    = 'Japanese late longline CPUE (S2 and S4), US shallow-set longline CPUE (S8), and Taiwanese longline CPUE (S5)'
)

MP_TypeDescription <- c(
  IR = 'Index-based MP: the TAC is a catch/index rate multiplied by the current index. The rate is reduced linearly from the threshold reference point (ThRP) to Fmin (F90%) at the limit reference point (LRP), with stock status as the index relative to its level at SB0.',
  IT = 'Index-based MP: the TAC is a reference catch, constant above the threshold reference point (ThRP) and reduced linearly to the catch of Fmin (F90%) at the limit reference point (LRP), with stock status as the index relative to its level at SB0.',
  SP = 'Model-based MP: a surplus production model is fitted to the catch and index, and F is a multiple of the estimated FMSY, reduced linearly from the threshold reference point (ThRP) to Fmin (F90%) at the limit reference point (LRP), with stock status as the estimated B/K.'
)

ConfigLabel <- vapply(names(MP_Type), \(mp) {
  Config <- SelectedConfigs[[mp]]
  if (!length(Config)) return('')
  paste0(' Settings: ', paste(names(Config), vapply(Config, deparse, character(1)),
                              sep = ' = ', collapse = ', '), '.')
}, character(1))

MP_Code  <- MP_all
MP_Label <- sprintf('%s HCR%d %s %d%%', MPInfo$MP, MPInfo$HCR, IndexSetLabel[MPInfo$IndexSet],
                    round(100 * MPInfo$TACChange))
MP_Description <- sprintf(
  '%s HCR %d: F target = %s, ThRP = %d%% SB0 and LRP = %d%% SB0, with TAC changes limited to %d%% between management cycles. Indices: %s%s.%s Tuned in the base case OM to maximise mean catch subject to P(SB > LRP in every year) >= %.2g and P(F <= %s) >= %.2g (tunepar = %.3g).',
  MP_TypeDescription[MPInfo$MP], MPInfo$HCR, MPInfo$FTarget, round(100 * MPInfo$ThRP), round(100 * LRP),
  round(100 * MPInfo$TACChange), IndexSetDescription[MPInfo$IndexSet],
  ifelse(MPInfo$MP == 'SP', ', and the early Japanese longline CPUE (S1 and S3)', ''),
  ConfigLabel[MPInfo$MP], PI_Min[['Safety']], MPInfo$FTarget, PI_Min[['Status']], MPInfo$tunepar)

# ---- OMs with MSE results ----
OM_Design <- ListSS3Runs() |>
  dplyr::filter(Include) |>
  dplyr::mutate(HistFile = file.path('objects/Hist', paste0(OM, '.hist'))) |>
  dplyr::filter(dir.exists(file.path(MSEDir, OM)))
if (!nrow(OM_Design))
  cli::cli_abort("No MSE results in {.file {MSEDir}}; run 06-Run-Projections.R.")
OM_Design$OM_Name <- paste0(OM_Design$OM, ': ', OM_Design$Description)
nOM <- nrow(OM_Design)

# ---- Performance indicators ----
# Candidate management objectives (Table 1): Safety, Status, Stability,
# probability of no management change, and Yield
StatusNames <- names(StatusFTargetSPR)
Table1_Codes <- c('Safety', 'Status', 'FTarget_F', 'Stability', 'NoChange', names(CatchRefYears))

PM_Codes <- c(Table1_Codes,
              paste0('Status_', StatusNames),
              paste0('FTarget_F_', StatusNames),

              'Yield_All',
              'Yield_Short',
              'Yield_Medium',
              'Yield_Long',

              'Safety_Short',
              'Safety_Medium',
              'Safety_Long',

              'Depletion_All',
              'Depletion_Short',
              'Depletion_Medium',
              'Depletion_Long',
              'Depletion_Min',

              'Kobe_Green',
              'SB_SBMSY_All',
              'F_FMSY_All',

              'AAVY')

nPI_pm <- length(PM_Codes)

YearsLabel <- vapply(CatchRefYears, \(yrs) paste(range(yrs), collapse = '-'), character(1))

PM_Label <- c(
  Safety           = 'Probability SB > LRP',
  Status           = 'Probability F <= F target (of the HCR)',
  FTarget_F        = 'F target (of the HCR) / F',
  Stability        = paste0('Catch stability (TAC change <= ', 100 * StabilityThreshold, '%)'),
  NoChange         = 'Probability of no management change',
  stats::setNames(paste0('Probability catch >= ', YearsLabel, ' catch'), names(CatchRefYears)),
  stats::setNames(paste0('Probability F <= ', StatusNames, '%'), paste0('Status_', StatusNames)),
  stats::setNames(paste0(StatusNames, '% / F'), paste0('FTarget_F_', StatusNames)),

  Yield_All        = 'Mean catch (all years)',
  Yield_Short      = 'Mean catch (years 1-10)',
  Yield_Medium     = 'Mean catch (years 11-20)',
  Yield_Long       = 'Mean catch (years 21-30)',

  Safety_Short     = 'Probability SB > LRP (years 1-10)',
  Safety_Medium    = 'Probability SB > LRP (years 11-20)',
  Safety_Long      = 'Probability SB > LRP (years 21-30)',

  Depletion_All    = 'SB/SB0 (all years)',
  Depletion_Short  = 'SB/SB0 (years 1-10)',
  Depletion_Medium = 'SB/SB0 (years 11-20)',
  Depletion_Long   = 'SB/SB0 (years 21-30)',
  Depletion_Min    = 'Minimum SB/SB0',

  Kobe_Green       = 'Probability Kobe green quadrant',
  SB_SBMSY_All     = 'SB/SBMSY (all years)',
  F_FMSY_All       = 'F/FMSY (all years)',

  AAVY             = 'Average TAC variability',

  Rel_Yield        = 'Relative mean catch'
)

Safety_Description <- paste0('Probability that spawning biomass remains above the limit reference point (LRP; ', 100 * LRP,
                             '% SB0) in every year')

PM_Description <- c(
  Safety           = paste0(Safety_Description, ' of the projection period: a simulation that falls below the LRP in any year is a breach.'),
  Status           = 'Probability that fishing mortality is at or below the F target of the HCR of the CMP (F40%, F35%, or F30%), over all projection years.',
  FTarget_F        = 'Median ratio of the F target of the HCR of the CMP to fishing mortality over the projection years; values above 1 mean F is below the target.',
  Stability        = paste0('Probability that the TAC changes by no more than ', 100 * StabilityThreshold, '% between consecutive management cycles.'),
  NoChange         = paste0('Probability of no management change: the probability that the TAC changes by no more than ', 100 * NoChangeThreshold, '% between consecutive management cycles.'),
  stats::setNames(paste0('Probability that the annual catch is at least the mean historical catch in ', YearsLabel,
                         ', over all projection years.'), names(CatchRefYears)),
  stats::setNames(paste0('Probability that fishing mortality is at or below ', StatusNames,
                         '% (candidate target reference point), over all projection years.'),
                  paste0('Status_', StatusNames)),
  stats::setNames(paste0('Median ratio of ', StatusNames,
                         '% to fishing mortality over the projection years; values above 1 mean F is below ', StatusNames, '%.'),
                  paste0('FTarget_F_', StatusNames)),

  Yield_All        = 'Mean annual catch (t) over all projection years.',
  Yield_Short      = 'Mean annual catch (t) over the first 10 projection years (short-term).',
  Yield_Medium     = 'Mean annual catch (t) over the middle 10 projection years (medium-term).',
  Yield_Long       = 'Mean annual catch (t) over the final 10 projection years (long-term).',

  Safety_Short     = paste0(Safety_Description, ' of the first 10 projection years (short-term).'),
  Safety_Medium    = paste0(Safety_Description, ' of the middle 10 projection years (medium-term).'),
  Safety_Long      = paste0(Safety_Description, ' of the final 10 projection years (long-term).'),

  Depletion_All    = 'Mean spawning biomass relative to the equilibrium unfished spawning biomass (SB/SB0) over all projection years.',
  Depletion_Short  = 'Mean SB/SB0 over the first 10 projection years (short-term).',
  Depletion_Medium = 'Mean SB/SB0 over the middle 10 projection years (medium-term).',
  Depletion_Long   = 'Mean SB/SB0 over the final 10 projection years (long-term).',
  Depletion_Min    = 'Lowest annual SB/SB0 reached during the projection.',

  Kobe_Green       = 'Probability that SB > SBMSY and F < FMSY in the same year, over all projection years.',
  SB_SBMSY_All     = 'Mean annual SB/SBMSY over all projection years.',
  F_FMSY_All       = 'Mean annual F/FMSY over all projection years.',

  AAVY             = 'Mean absolute relative change in the TAC between management cycles.',

  Rel_Yield        = 'Mean annual catch (all years) relative to the highest-yielding MP in the same OM.'
)

Spider_Codes   <- c('Safety', 'Status', 'Stability', 'NoChange', names(CatchRefYears), 'Rel_Yield')
Headline_Codes <- c(Table1_Codes, 'Yield_All')
Tradeoff_Codes <- c('Safety', 'Yield_All', 'Status', 'Stability', 'NoChange', names(CatchRefYears), 'Depletion_All')

TS_FCodes <- paste0('F_', StatusNames)
TS_Codes  <- c('SB_SB0', TS_FCodes, 'Yield', 'SB_SBMSY', 'F_FMSY')
TS_Label  <- c(SB_SB0 = 'SB/SB0', stats::setNames(paste0('F/', StatusNames, '%'), TS_FCodes), Yield = 'Yield',
               SB_SBMSY = 'SB/SBMSY', F_FMSY = 'F/FMSY')
TS_Description <- c(
  SB_SB0   = 'Spawning biomass relative to the equilibrium unfished spawning biomass.',
  stats::setNames(paste0('Fishing mortality relative to ', StatusNames, '%.'), TS_FCodes),
  Yield     = 'Total annual catch (t) across all fleets.',
  SB_SBMSY  = 'Spawning biomass relative to SBMSY.',
  F_FMSY    = 'Fishing mortality relative to FMSY.'
)
TS_Target <- c(SB_SB0 = NA,  stats::setNames(rep(1, length(TS_FCodes)), TS_FCodes),  Yield = NA, SB_SBMSY = 1,  F_FMSY = NA)
TS_Limit  <- c(SB_SB0 = LRP, stats::setNames(rep(NA, length(TS_FCodes)), TS_FCodes), Yield = NA, SB_SBMSY = NA, F_FMSY = 1)

# ---- Sequence through one `.mse` in memory at a time ----
PM_Value <- NULL
TS_Value <- NULL

for (i in seq_len(nOM)) {
  cli::cli_progress_step("[{i}/{nOM}] OM {OM_Design$OM_Name[i]}")
  FTarget_FMSY <- PM_FTarget_FMSY(readRDS(OM_Design$HistFile[i]))

  for (g in names(Groups)) {
    File <- file.path(MSEDir, OM_Design$OM[i], paste0(g, '.mse'))
    if (!file.exists(File))
      cli::cli_abort("OM {OM_Design$OM[i]} has no results for {.val {g}}; re-run 06-Run-Projections.R.")
    MSE <- readRDS(File)
    Missing <- setdiff(Groups[[g]], names(MSE@MPs))
    if (length(Missing))
      cli::cli_abort("OM {OM_Design$OM[i]} has no results for {.val {Missing}}; re-run 06-Run-Projections.R.")

    TS    <- Extract_MSE_Timeseries(MSE, FTarget_FMSY = FTarget_FMSY)
    Stats <- Calc_PM_Stats(MSE, Lim = LRP, StabilityThreshold = StabilityThreshold,
                           NoChangeThreshold = NoChangeThreshold, MPFTarget = MPFTarget,
                           CatchRefYears = CatchRefYears, TS = TS)
    Stats <- Stats[Stats$MP %in% Groups[[g]], ]

    if (is.null(PM_Value)) {
      nsim        <- MSE@OM@nSim
      MPStartYear <- MSE@OM@MPStartYear
      Time        <- sort(unique(TS$Year))
      TimeNow     <- max(TS$Year[TS$Period == 'Historical'])

      PM_Value <- array(NA_real_, dim = c(nsim, nOM, nMP, nPI_pm),
                        dimnames = list(Sim = seq_len(nsim), OM = OM_Design$OM_Name,
                                        MP = MP_all, PI = PM_Codes))
      TS_Value <- array(NA_real_, dim = c(nsim, nOM, nMP, length(TS_Codes), length(Time)),
                        dimnames = list(Sim = seq_len(nsim), OM = OM_Design$OM_Name,
                                        MP = MP_all, PI = TS_Codes, Time = Time))
    }

    ## ---- PIs (per sim) -> Boxplot / Quilt / Spider / Tradeoff ----
    pi_idx <- match(Stats$PI, PM_Codes)
    if (anyNA(pi_idx))
      cli::cli_abort("`Calc_PM_Stats()` returned PI{?s} not in {.arg PM_Codes}: {.val {unique(Stats$PI[is.na(pi_idx)])}}.")
    PM_Value[cbind(Stats$Sim, i, match(Stats$MP, MP_all), pi_idx)] <- Stats$Value

    ## ---- Time series (per sim, per year) -> Timeseries / Kobe ----
    TS <- TS[TS$Year %in% Time & TS$Variable %in% TS_Codes, ]
    Proj <- TS[TS$Period == 'Projection' & TS$MP %in% Groups[[g]], ]
    TS_Value[cbind(Proj$Sim, i, match(Proj$MP, MP_all),
                   match(Proj$Variable, TS_Codes), match(Proj$Year, Time))] <- Proj$Value

    Hist <- TS[TS$Period == 'Historical', ]
    for (mp in match(Groups[[g]], MP_all))
      TS_Value[cbind(Hist$Sim, i, mp, match(Hist$Variable, TS_Codes), match(Hist$Year, Time))] <- Hist$Value

    rm(MSE, TS, Stats, Proj, Hist)
    gc()
  }
}
cli::cli_progress_done()

# ---- OMs ----
OMs_obj <- Slick::OMs()

Slick::Factors(OMs_obj) <- data.frame(
  Factor      = 'OM',
  Level       = OM_Design$OM,
  Description = OM_Design$Description
)

Slick::Design(OMs_obj) <- data.frame(OM = OM_Design$OM)
rownames(Slick::Design(OMs_obj)) <- OM_Design$OM_Name

Slick::Preset(OMs_obj) <- c(
  if ('00' %in% OM_Design$OM) list(`Base Case` = list(match('00', OM_Design$OM))),
  list(All = list(seq_len(nOM)))
)

# ---- MPs ----
MPs_obj <- Slick::MPs(Code = MP_Code, Label = MP_Label, Description = MP_Description)

Slick::Preset(MPs_obj) <- c(
  list(All = seq_len(nMP)),
  split(seq_len(nMP), MP_Type[MPInfo$MP]),
  split(seq_len(nMP), paste('HCR', MPInfo$HCR)),
  split(seq_len(nMP), paste('F target', MPInfo$FTarget)),
  split(seq_len(nMP), paste('Indices:', IndexSetLabel[MPInfo$IndexSet])),
  split(seq_len(nMP), paste0(round(100 * MPInfo$TACChange), '% TAC change'))
)

# ---- Boxplot / Quilt ----
Boxplot_obj <- Slick::Boxplot(Code = PM_Codes, Label = unname(PM_Label[PM_Codes]),
                              Description = unname(PM_Description[PM_Codes]),
                              Value = PM_Value)

Prob01_Codes <- c('Safety', 'Safety_Short', 'Safety_Medium', 'Safety_Long', 'Status',
                  paste0('Status_', StatusNames), 'Stability', 'NoChange', names(CatchRefYears),
                  'Kobe_Green')

Quilt_Codes <- setdiff(PM_Codes, c('AAVY', 'Depletion_Min'))
nPI_quilt   <- length(Quilt_Codes)

Quilt_MinValue <- rep(NA_real_, nPI_quilt)
Quilt_MaxValue <- rep(NA_real_, nPI_quilt)
Quilt_MinValue[match(Prob01_Codes, Quilt_Codes)] <- 0
Quilt_MaxValue[match(Prob01_Codes, Quilt_Codes)] <- 1

Quilt_obj <- Slick::Quilt(Code = Quilt_Codes, Label = unname(PM_Label[Quilt_Codes]),
                          Description = unname(PM_Description[Quilt_Codes]),
                          Value = PM_Value[, , , Quilt_Codes, drop = FALSE],
                          MinValue = Quilt_MinValue, MaxValue = Quilt_MaxValue)

non_windowed_PM <- c('Depletion_Min', 'AAVY', 'Stability')

WindowPreset <- function(Codes, Keep) {
  list(Summary    = match(intersect(Headline_Codes, Codes), Codes),
       Status     = match(intersect(c('Status', 'FTarget_F', paste0('Status_', StatusNames),
                                      paste0('FTarget_F_', StatusNames)), Codes), Codes),
       All        = seq_along(Codes),
       ShortTerm  = match(c(grep('_Short$',  Codes, value = TRUE), intersect(Keep, Codes)), Codes),
       MediumTerm = match(c(grep('_Medium$', Codes, value = TRUE), intersect(Keep, Codes)), Codes),
       LongTerm   = match(c(grep('_Long$',   Codes, value = TRUE), intersect(Keep, Codes)), Codes))
}

Slick::Preset(Boxplot_obj) <- WindowPreset(PM_Codes, non_windowed_PM)
Slick::Preset(Quilt_obj)   <- WindowPreset(Quilt_Codes, non_windowed_PM)

# ---- Spider / Tradeoff ----
.Agg_PM <- function(codes) {
  arr <- array(NA_real_, dim = c(nOM, nMP, length(codes)),
               dimnames = list(OM = OM_Design$OM_Name, MP = MP_all, PI = codes))
  for (code in codes) {
    FUN <- if (code == 'Depletion_Min') min else mean
    arr[, , code] <- apply(PM_Value[, , , code, drop = FALSE], c(2, 3), FUN, na.rm = TRUE)
  }
  arr
}

Spider_PM_Codes <- setdiff(Spider_Codes, 'Rel_Yield')
Spider_Value <- array(NA_real_, dim = c(nOM, nMP, length(Spider_Codes)),
                      dimnames = list(OM = OM_Design$OM_Name, MP = MP_all, PI = Spider_Codes))
Spider_Value[, , Spider_PM_Codes] <- .Agg_PM(Spider_PM_Codes)

Yield_OM_MP <- apply(PM_Value[, , , 'Yield_All', drop = FALSE], c(2, 3), mean, na.rm = TRUE)
Spider_Value[, , 'Rel_Yield'] <- Yield_OM_MP / apply(Yield_OM_MP, 1, max, na.rm = TRUE)

Spider_obj <- Slick::Spider(Code = Spider_Codes, Label = unname(PM_Label[Spider_Codes]),
                            Description = unname(PM_Description[Spider_Codes]),
                            Value = Spider_Value)

Tradeoff_obj <- Slick::Tradeoff(Code = Tradeoff_Codes, Label = unname(PM_Label[Tradeoff_Codes]),
                                Description = unname(PM_Description[Tradeoff_Codes]),
                                Value = .Agg_PM(Tradeoff_Codes))

Slick::Preset(Tradeoff_obj) <- list(
  All         = seq_along(Tradeoff_Codes),
  CatchSafety = match(c('Yield_All', 'Safety'), Tradeoff_Codes),
  CatchStatus = match(c('Yield_All', 'Status'), Tradeoff_Codes)
)

# ---- Timeseries / Kobe ----
Timeseries_obj <- Slick::Timeseries(Code = TS_Codes, Label = unname(TS_Label[TS_Codes]),
                                    Description = unname(TS_Description[TS_Codes]),
                                    Value = TS_Value, Time = Time, TimeNow = TimeNow,
                                    Target = unname(TS_Target[TS_Codes]),
                                    Limit  = unname(TS_Limit[TS_Codes]))

KobeTime <- Time[Time >= MPStartYear]
Kobe_obj <- Slick::Kobe(Code = c('SB_SBMSY', 'F_FMSY'),
                        Label = unname(TS_Label[c('SB_SBMSY', 'F_FMSY')]),
                        Description = unname(TS_Description[c('SB_SBMSY', 'F_FMSY')]),
                        Value = TS_Value[, , , c('SB_SBMSY', 'F_FMSY'), as.character(KobeTime), drop = FALSE],
                        Time = KobeTime, Target = c(1, 1))

# ---- Assemble and save ----
Introduction <- paste0("
Preliminary results of the management strategy evaluation (MSE) for North Pacific swordfish, developed for the Northern Committee of the Western and Central Pacific Fisheries Commission (WCPFC).

The operating models (OMs) are conditioned on the base case and sensitivity runs of the 2023 ISC stock assessment. The candidate management procedures (CMPs) are two index-based MPs (Index Rate and Index Target) and a model-based MP (Surplus Production), each with the six candidate harvest control rules (HCRs) of the Northern Committee (NC22 Summary Report, Attachment 7): F target = F40% with a threshold reference point (ThRP) of 35%, 30%, or 25% SB0 (HCRs 1-3), F35% with a ThRP of 30% or 25% SB0 (HCRs 4-5), and F30% with a ThRP of 25% SB0 (HCR 6), where SB0 is the equilibrium unfished spawning biomass. Below the limit reference point (LRP = 17% SB0), fishing is reduced to Fmin = F90%. Each CMP uses one of four sets of indices of abundance: the Japanese late longline CPUE (JPN), with the US shallow-set longline CPUE (JPN + US), with the Taiwanese longline CPUE (JPN + TWN), or all three (JPN + US + TWN). The TAC change between management cycles is limited to 10% or 20%. Each CMP was tuned in the base case OM to maximise mean catch subject to the Safety (probability of at least ", PI_Min[['Safety']], ") and Status (probability of at least ", PI_Min[['Status']], " that F is at or below the F target of its HCR) objectives.

The performance indicators of the candidate management objectives are: Safety, the probability that spawning biomass remains above the LRP in every year of the projection period; Status, the probability that F is at or below the F target of the HCR (Status is also reported against each candidate target reference point, F40%, F35%, and F30%); Stability, the probability that the TAC changes by no more than 10% between management cycles; the probability of no management change (the TAC changes by no more than 1% between management cycles); and Yield, the probability that the annual catch is at least the mean 2008-2010 catch, and the mean 2016-2021 catch.

**These results are preliminary and do not necessarily reflect the views of the WCPFC or its members.**
")

slick <- Slick::Slick(Title = 'North Pacific Swordfish',
                      Author = 'Adrian Hordyk',
                      Email = 'adrian@bluematterscience.com',
                      Introduction = Introduction,
                      MPs = MPs_obj,
                      OMs = OMs_obj,
                      Boxplot = Boxplot_obj,
                      Kobe = Kobe_obj,
                      Quilt = Quilt_obj,
                      Spider = Spider_obj,
                      Tradeoff = Tradeoff_obj,
                      Timeseries = Timeseries_obj)

print(Slick::Check(slick))

MSEtool::Save(slick, 'objects/slick.rds', overwrite = TRUE)
# MSEtool::Save(slick, '../SlickLibrary/Slick_Objects/North_Pacific_Swordfish.slick', overwrite = TRUE)

# ---- Run App ----
if (interactive())
  Slick::App(slick = slick)

# ---- Make manual plots ----
# By CMP group (MP and index set), for all OMs combined and the base case
OM_Sets <- c(list(All = seq_len(nOM)),
             if ('00' %in% OM_Design$OM) list(OM00 = match('00', OM_Design$OM)))

## ---- Timeseries ----
for (g in names(Groups)) {
  mp_idx <- match(Groups[[g]], MP_all)
  for (om_set in names(OM_Sets)) {
    slick_sub <- Slick::FilterSlick(slick, MPs = mp_idx, OMs = OM_Sets[[om_set]], plot = 'Timeseries')
    for (code in TS_Codes) {
      p <- Slick::plotTimeseries(slick_sub, PI = match(code, TS_Codes), byMP = TRUE, includeHist = FALSE)
      ggplot2::ggsave(file.path(FigDir, 'Timeseries', om_set, g, paste0(code, '.png')),
                      p, width = 12, height = 8, dpi = 300, create.dir = TRUE)
    }
  }
}

## ---- Kobe and Kobe-time ----
for (g in names(Groups)) {
  mp_idx <- match(Groups[[g]], MP_all)
  for (om_set in names(OM_Sets)) {
    slick_sub <- Slick::FilterSlick(slick, MPs = mp_idx, OMs = OM_Sets[[om_set]], plot = 'Kobe')

    p_kobe <- Slick::plotKobe(slick_sub, xPI = 1, yPI = 2, Time = FALSE, percentile = NULL)
    ggplot2::ggsave(file.path(FigDir, 'Kobe', om_set, paste0(g, '.png')),
                    p_kobe, width = 8, height = 7, dpi = 300, create.dir = TRUE)

    p_kobe_time <- Slick::plotKobe(slick_sub, xPI = 1, yPI = 2, Time = TRUE)
    ggplot2::ggsave(file.path(FigDir, 'Kobe', om_set, paste0(g, '_Time.png')),
                    p_kobe_time, width = 12, height = 8, dpi = 300, create.dir = TRUE)
  }
}

## ---- Quilt tables ----
# All CMPs, for all OMs combined and each OM
Quilt_OM_Sets <- c(list(All = seq_len(nOM)),
                   stats::setNames(as.list(seq_len(nOM)), paste0('OM', OM_Design$OM)))

Quilt_Primary_Codes  <- intersect(Headline_Codes, Quilt_Codes)
Quilt_Extended_Codes <- c(Quilt_Primary_Codes,
                          paste0('Status_', StatusNames),
                          'Yield_Short', 'Yield_Medium', 'Yield_Long',
                          'Safety_Short', 'Safety_Medium', 'Safety_Long',
                          'Depletion_All', 'Depletion_Long')

Save_Quilt_Table <- function(pi_codes, om_idx, file_stub) {
  slick_sub <- Slick::FilterSlick(slick, OMs = om_idx, PIs = match(pi_codes, Quilt_Codes), plot = 'Quilt')
  tbl       <- Slick::plotQuilt(slick_sub, kable = TRUE)

  prob_cols <- intersect(Prob01_Codes, pi_codes)
  if (length(prob_cols))
    tbl <- flextable::colformat_double(tbl, j = prob_cols, digits = 2)

  dir <- file.path(FigDir, 'Quilt')
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  htmltools::save_html(flextable::htmltools_value(tbl), file = file.path(dir, paste0(file_stub, '.html')))
  flextable::save_as_docx(tbl, path = file.path(dir, paste0(file_stub, '.docx')))
}

for (om_set in names(Quilt_OM_Sets)) {
  Save_Quilt_Table(Quilt_Primary_Codes,  Quilt_OM_Sets[[om_set]], paste0(om_set, '_Primary'))
  Save_Quilt_Table(Quilt_Extended_Codes, Quilt_OM_Sets[[om_set]], paste0(om_set, '_Extended'))
}

## ---- Comparison across OMs ----
OM_Colours <- scales::hue_pal()(nOM)
OM_Colours[OM_Design$OM == '00'] <- 'black'
names(OM_Colours) <- OM_Design$OM_Name

# Median time series in each OM, one panel per CMP, one figure per CMP group
TS_Median <- apply(TS_Value, c(2, 3, 4, 5), stats::median, na.rm = TRUE) |>
  as.data.frame.table(responseName = 'Value', stringsAsFactors = FALSE)
TS_Median$Time <- as.numeric(TS_Median$Time)
TS_Median$OM   <- factor(TS_Median$OM, levels = OM_Design$OM_Name)

for (g in names(Groups)) {
  for (code in TS_Codes) {
    d <- TS_Median[TS_Median$PI == code & TS_Median$MP %in% Groups[[g]], ]
    d$MP <- factor(d$MP, levels = Groups[[g]])
    p <- ggplot2::ggplot(d, ggplot2::aes(Time, Value, colour = OM)) +
      ggplot2::geom_vline(xintercept = MPStartYear, linetype = 3, colour = 'grey40') +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::facet_wrap(~MP, ncol = 4, scales = 'free_y') +
      ggplot2::scale_colour_manual(values = OM_Colours) +
      ggplot2::expand_limits(y = 0) +
      ggplot2::labs(x = NULL, y = TS_Label[[code]], colour = NULL,
                    caption = 'Median over simulations. Dotted line: first year of MP advice.') +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = 'bottom')
    if (!is.na(TS_Target[[code]]))
      p <- p + ggplot2::geom_hline(yintercept = TS_Target[[code]], linetype = 2, colour = 'darkgreen')
    if (!is.na(TS_Limit[[code]]))
      p <- p + ggplot2::geom_hline(yintercept = TS_Limit[[code]], linetype = 2, colour = 'red')
    ggplot2::ggsave(file.path(FigDir, 'Timeseries', 'ByOM', g, paste0(code, '.png')),
                    p, width = 14, height = 9, dpi = 300, create.dir = TRUE)
  }
}

# Performance indicators of the candidate management objectives in each OM, one figure per CMP group
Summary_Codes <- c(Table1_Codes, 'Yield_All')
PI_OM <- apply(PM_Value, c(2, 3, 4), mean, na.rm = TRUE) |>
  as.data.frame.table(responseName = 'Value', stringsAsFactors = FALSE)

Min <- data.frame(PI = factor(PM_Label[names(PI_Min)], levels = PM_Label[Summary_Codes]), Value = PI_Min)
for (g in names(Groups)) {
  d <- PI_OM[PI_OM$PI %in% Summary_Codes & PI_OM$MP %in% Groups[[g]], ]
  d$PI <- factor(PM_Label[d$PI], levels = PM_Label[Summary_Codes])
  d$MP <- factor(d$MP, levels = Groups[[g]])
  d$OM <- factor(d$OM, levels = OM_Design$OM_Name)

  p <- ggplot2::ggplot(d, ggplot2::aes(MP, Value, colour = OM)) +
    ggplot2::geom_hline(data = Min, ggplot2::aes(yintercept = Value), linetype = 2, colour = 'red') +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.7), size = 2) +
    ggplot2::facet_wrap(~PI, ncol = 2, scales = 'free_y', labeller = ggplot2::label_wrap_gen(45)) +
    ggplot2::scale_colour_manual(values = OM_Colours) +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL,
                  caption = 'Mean over simulations. Dashed lines: minimum probabilities of the Safety and Status objectives.') +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom', axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  ggplot2::ggsave(file.path(FigDir, 'Summary', paste0('PI_by_OM_', g, '.png')), p, width = 11, height = 12, dpi = 300,
                  create.dir = TRUE)
}

# Table: one row per PI and CMP, one column per OM, and the mean and minimum over OMs
Tab <- stats::reshape(PI_OM, idvar = c('PI', 'MP'), timevar = 'OM', direction = 'wide')
names(Tab) <- sub('^Value\\.', '', names(Tab))
Tab <- Tab[order(match(Tab$PI, PM_Codes), match(Tab$MP, MP_all)), ]
Tab$Mean <- rowMeans(Tab[OM_Design$OM_Name])
Tab$Min  <- apply(Tab[OM_Design$OM_Name], 1, min)
utils::write.csv(Tab, file.path(FigDir, 'Summary', 'PI_by_OM.csv'), row.names = FALSE)
