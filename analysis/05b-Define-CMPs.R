# Define the candidate management procedures
library(wcpfcNSWO)

Hist <- readRDS('objects/Hist/00.hist')

# ---- Reference Levels ----
LRP       <- 0.17               # limit reference point: SB / SB0 (equilibrium unfished)
PSafety   <- 0.8                # minimum P(SB > LRP in every year) when tuning
PStatus   <- 0.5                # minimum P(F <= F target) when tuning
FminSPR   <- 0.9                # Fmin = F90%
TACChange <- c(0.1, 0.2)        # maximum TAC change (up or down) between management cycles

# Candidate HCRs: Table 2 of the candidate inputs for the MSE (NC22 Summary
# Report, Attachment 7). The F target of each HCR is set by tuning (tunepar):
# each CMP is tuned so that P(F <= F target) >= PStatus
HCRs <- data.frame(HCR        = 1:6,
                   FTargetSPR = c(0.40, 0.40, 0.40, 0.35, 0.35, 0.30),
                   ThRP       = c(0.35, 0.30, 0.25, 0.30, 0.25, 0.25))

FTargetSPRs <- sort(unique(HCRs$FTargetSPR), decreasing = TRUE)
FLabel      <- function(SPR) paste0('F', round(100 * SPR))   # e.g. 'F40'

## Fmin as a fraction of the target F of each HCR: F90% / F target
Hist      <- MSEtool::CalcRefPoints(Hist, SPRTarget = c(FTargetSPRs, FminSPR), silent = TRUE)
FSPRArr   <- MSEtool::FSPR(Hist)
FminRatio <- vapply(FTargetSPRs, \(SPR)
  stats::median(FSPRArr[, 'Female', , as.character(FminSPR)] / FSPRArr[, 'Female', , as.character(SPR)]),
  numeric(1)) |> stats::setNames(FLabel(FTargetSPRs))
HCRs$FminRatio <- unname(FminRatio[FLabel(HCRs$FTargetSPR)])

## F targets relative to FMSY, for the Status constraint
FTarget_FMSY <- lapply(stats::setNames(FTargetSPRs, FLabel(FTargetSPRs)),
                       \(SPR) Calc_FTarget_FMSY(Hist, SPR = SPR))

# ---- Indices ----
# Index sets of the index-based MPs (IR, IT): the Japanese late longline CPUE
# (Areas 1 and 2) alone and with the US shallow-set (S8) and/or Taiwanese (S5)
# longline CPUE. S6 is a recruitment index. S5 and S8 have lambda = 0 in the
# base case assessment
IndexSets <- list(JPN    = c('S2_JPN_WCNPO_OSDWCOLL_late_Area1', 'S4_JPN_WCNPO_OSDWLL_late_Area2'),
                  JPNUS  = c('S2_JPN_WCNPO_OSDWCOLL_late_Area1', 'S4_JPN_WCNPO_OSDWLL_late_Area2',
                             'S8_US_WCNPO_LL_shallow_late'),
                  JPNTWN = c('S2_JPN_WCNPO_OSDWCOLL_late_Area1', 'S4_JPN_WCNPO_OSDWLL_late_Area2',
                             'S5_TWN_WCNPO_DWLL_late'),
                  All    = c('S2_JPN_WCNPO_OSDWCOLL_late_Area1', 'S4_JPN_WCNPO_OSDWLL_late_Area2',
                             'S5_TWN_WCNPO_DWLL_late', 'S8_US_WCNPO_LL_shallow_late'))
IndexSetLabel <- c(JPN = 'JPN', JPNUS = 'JPN + US', JPNTWN = 'JPN + TWN', All = 'JPN + US + TWN')

MPIndices <- function(MP, IndexSet) IndexSets[[IndexSet]]

# ---- Surplus Production Model Settings ----
# The catch and indices contain little information on stock size: catch is
# near-constant since 1975 and the indices have no trend.

# With biomass fixed at K at the start of the fitted years (the MSEtool default),
# MSY and K run to their upper bounds and most fits fail.
#
# So the model is fitted from the start of the late Japanese series (1994;
# the early series S1 and S3 end in 1993), the depletion in 1994 is estimated,
# and weakly informative lognormal priors are set on depletion (B/K) and MSY
# (median twice the mean historical catch)
# mean annual catch over the historical data years (1975-2021)

SPMeanCatch <- mean(rowSums(MSEtool::AnnualData(Hist@Data[[1]][[1]])@Landings@Value, na.rm = TRUE))
SPSettings <- list(Shape        = 2,           # Schaefer; the settings below were tested with Shape 2 only
                   FitYears     = 1994:2100,   # 1994 to the last data year
                   EstDepletion = TRUE,
                   Depletion    = 0.5,         # starting value
                   Priors       = list(FMSY      = c(0.2, 2),     # MSEtool default
                                       Depletion = c(0.5, 0.5),
                                       MSY       = c(2 * SPMeanCatch, 1)))

## Index level at SB0 of each index (IndexSB0), so the status of the index CMPs,
# index / IndexSB0, is in units of SB/SB0, as are the HCR control points.
# Calibrated on the projection of the base case OM (05a-Calibrate-IndexSB0.R:
# mean index / mean SB/SB0, 2022-2051, constant catch), where the HCR operates.
# Calculated once, from the base case, and used, as MP arguments, with every OM.
# Calibrated on the historical years (below, IndexSB0Hist), the CMPs' status ran
# at only ~0.77 x OM SB/SB0 in the projection (with a 2017-2021 window, ~0.72)
IndexNames <- unique(unlist(IndexSets))

Survey    <- Hist@Data[[1]][[1]]@Survey
stopifnot(all(IndexNames %in% Survey@Name))
IndexYear <- floor(as.numeric(rownames(Survey@Value)) + 1e-6)

Dep <- MSEtool::SB_SB0(Hist, type = 'Equilibrium', df = TRUE, Reduce = FALSE)
Dep <- Dep[Dep$Stock == 'Female', ]
Dep$Year <- floor(as.numeric(Dep$Year) + 1e-6)

IndexValue    <- lapply(stats::setNames(nm = IndexNames), \(nm) Survey@Value[, match(nm, Survey@Name)])
IndexSB0Years <- lapply(IndexValue, \(v) sort(unique(IndexYear[is.finite(v) & v > 0])))
DepCalib <- vapply(IndexSB0Years, \(Yrs) {
  d <- Dep[Dep$Year %in% Yrs, ]
  stats::median(tapply(d$Value, d$Sim, mean))
}, numeric(1))
# historical calibration: each index over all its observed historical years
IndexSB0Hist <- vapply(IndexNames, \(nm) mean(IndexValue[[nm]][IndexYear %in% IndexSB0Years[[nm]]], na.rm = TRUE) /
                         DepCalib[[nm]], numeric(1))

IndexSB0File <- file.path('objects', 'IndexSB0.rds')
if (!file.exists(IndexSB0File))
  stop('Run analysis/05a-Calibrate-IndexSB0.R first: ', IndexSB0File, ' not found')
IndexSB0Calib <- readRDS(IndexSB0File)
stopifnot(all(IndexNames %in% names(IndexSB0Calib$IndexSB0)))
IndexSB0 <- IndexSB0Calib$IndexSB0[IndexNames]

# ---- Tuning Specifications ----
TuneDir <- file.path(normalizePath('objects'), 'Tune')
dir.create(TuneDir, showWarnings = FALSE)

# PM_Removals sums catch to calendar years, so its Mean is the mean annual catch
Objective <- MSEtool::TuneObjective(MSEtool::PM_Removals, Name = 'MeanCatch')

# Candidate management objectives: Safety, P(SB > LRP in every year) >= PSafety,
# and Status, P(F <= F target of the HCR) >= PStatus
TuneConstraints <- function(FTargetSPR) {
  list(
    MSEtool::TuneConstraint(PM_SafetyNoBreach, Lim = LRP, Min = PSafety, Name = 'Safety'),
    MSEtool::TuneConstraint(PM_StatusF, FTarget_FMSY = FTarget_FMSY[[FLabel(FTargetSPR)]],
                            Min = PStatus, Name = 'Status')
  )
}

TuneInterval <- c(0.25, 4)  # initial range of tunepar

## Checks of saved tuning results
# MP arguments other than tunepar; a saved result is re-used only if these are unchanged
MPSettings <- function(MP) {
  Args <- formals(MP)
  Args[!names(Args) %in% c('tunepar', 'plot')]
}

# Objective and constraints (e.g. the definition of the LRP in PM_SafetyNoBreach);
# a saved result is also re-used only if these are unchanged
MetricSpec <- function(Metrics) {
  lapply(Metrics, \(m) list(Name = m$Name, Type = m$Type, Min = m$Min, Max = m$Max,
                            Args = m$Args, PM = deparse(body(m$PM))))
}
TuneSpec <- function(FTargetSPR) MetricSpec(c(list(Objective), TuneConstraints(FTargetSPR)))

# TuneMP records a failed projection as infeasible without the error
AllFailed <- function(Tuned) all(Tuned@History$FailRate >= 1)

# A saved result is current if its MP settings and tuning specification match
# those of CMP i. Compared with a numerical tolerance, as values derived from
# reference points (e.g. FminRatio) can differ in the last digits between machines
IsCurrent <- function(Tuned, i) {
  isTRUE(all.equal(MPSettings(Tuned@MP), MPSettings(CMPs$Fn[[i]]), tolerance = 1e-8)) &&
    isTRUE(all.equal(MetricSpec(Tuned@Settings$Metrics), TuneSpec(CMPs$FTargetSPR[i]),
                     tolerance = 1e-8)) &&
    !AllFailed(Tuned)
}

# ---- MP Settings Selected by 05c-Select-Configs.R ----
# Candidate values of the MP settings not set by tuning or by the HCR. The
# values selected for each MP (ConfigsFile) are used for all its CMPs; MSEtool
# defaults until then. Selected with ConfigCMP
ConfigCandidates <- list(IR = list(RecentYears = c(1, 3), Smooth = c(TRUE, FALSE)),
                         IT = list(RecentYears = c(1, 3), Smooth = c(TRUE, FALSE)))
ConfigCMP <- list(HCR = 2, IndexSet = 'JPN', TACChange = 0.1)

ConfigsFile     <- file.path(TuneDir, 'Configs.rds')
SelectedConfigs <- if (file.exists(ConfigsFile)) readRDS(ConfigsFile) else list()
SelectedConfigs <- lapply(stats::setNames(nm = names(ConfigCandidates)),
                          \(mp) SelectedConfigs[[mp]][names(SelectedConfigs[[mp]]) %in%
                                                        names(ConfigCandidates[[mp]])])

ConfigLabel <- function(Config) {
  if (!length(Config)) return('MSEtool defaults')
  paste(names(Config), vapply(Config, \(x) paste(x, collapse = ', '), character(1)), sep = ' = ', collapse = ', ')
}
for (mp in names(ConfigCandidates))
  cli::cli_inform("{mp} settings: {ConfigLabel(SelectedConfigs[[mp]])}")

# BMSY/K of the surplus production model
BMSY_K <- function(Shape) if (abs(Shape - 1) < 1e-6) exp(-1) else Shape^(1 / (1 - Shape))

# ---- Candidate MPs ----
# All HCRs: Fmin below the LRP, linear increase to the target at the ThRP
# - IR: catch/index rate (fishing intensity) vs index / index at SB0
# - IT: catch vs index / index at SB0; constant catch above the ThRP, and
#   the catch of Fmin at the LRP below it (FminRatio * LRP / ThRP)
# - SP: F (multiple of estimated FMSY) vs estimated B/BMSY; control points
#   converted from B/K with BMSY/K of the model Shape
# Indices are equally weighted

# MP arguments of an HCR and configuration: the HCR control points (from the
# ThRP and Fmin of the HCR and, for SP, the model Shape) and the other settings
ConfigArgs <- function(MP, HCR, Config) {
  ThRP      <- HCRs$ThRP[HCRs$HCR == HCR]
  FminRatio <- HCRs$FminRatio[HCRs$HCR == HCR]
  HCRArgs <- switch(MP,
                    IR = list(HCRControlPointsIndex   = c(LRP, ThRP),
                              HCRControlPointsRate    = c(FminRatio, 1)),
                    IT = list(HCRControlPointsIndex   = c(LRP, ThRP),
                              HCRControlPointsRate    = c(FminRatio * LRP / ThRP, 1)),
                    SP = list(HCRControlPointsBiomass = c(LRP, ThRP) / BMSY_K(SPSettings$Shape),
                              HCRControlPointsRate    = c(FminRatio, 1)))
  c(HCRArgs, Config)
}

MakeCMP <- function(MP, HCR, IndexSet, TACChange, Config = SelectedConfigs[[MP]]) {
  Delta   <- c(0.01, TACChange)
  Indices <- MPIndices(MP, IndexSet)
  Base <- switch(MP,
                 IR = list(MSEtool::IndexRate, Indices = Indices, IndexTarget = unname(IndexSB0[Indices])),
                 IT = list(MSEtool::IndexTarget, Indices = Indices, IndexTarget = unname(IndexSB0[Indices])),
                 SP = c(list(MSEtool::SurplusProduction, Indices = Indices), SPSettings))
  do.call(MSEtool::SetMPArgs, c(Base, list(DeltaDown = Delta, DeltaUp = Delta), ConfigArgs(MP, HCR, Config)))
}

# Each MP with each HCR, index set, and TAC change limit
CMPs <- expand.grid(MP = c('IR', 'IT', 'SP'), IndexSet = names(IndexSets), HCR = HCRs$HCR,
                    TACChange = TACChange, stringsAsFactors = FALSE)
CMPs <- CMPs[order(CMPs$TACChange, CMPs$HCR, match(CMPs$IndexSet, names(IndexSets)),
                   match(CMPs$MP, c('IR', 'IT', 'SP'))), ]
rownames(CMPs) <- NULL
CMPs$FTargetSPR <- HCRs$FTargetSPR[match(CMPs$HCR, HCRs$HCR)]
CMPs$ThRP       <- HCRs$ThRP[match(CMPs$HCR, HCRs$HCR)]
CMPs$Name <- sprintf('%s_H%d_%s_%02d', CMPs$MP, CMPs$HCR, CMPs$IndexSet, round(100 * CMPs$TACChange))
stopifnot(!anyDuplicated(CMPs$Name))

CMPs$Fn <- purrr::pmap(CMPs[c('MP', 'HCR', 'IndexSet', 'TACChange')], MakeCMP)
