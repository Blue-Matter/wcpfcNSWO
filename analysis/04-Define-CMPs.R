# Define the candidate management procedures
library(wcpfcNSWO)

Hist <- readRDS('objects/Hist/00.hist')

# ---- Reference Levels ----
LRP        <- 0.17              # limit reference point: SB / SB0 (equilibrium unfished)
PSafety    <- 0.8               # minimum P(SB > LRP in every year) when tuning
PStatus    <- 0.5               # minimum P(F <= F target) when tuning
FTargetSPR <- 0.4               # F target of HCRs 1-3: F40%
FminSPR    <- 0.9               # Fmin = F90%
TACChange  <- c(0.1, 0.2, 0.4)  # maximum TAC change (up or down) between management cycles

# Control points of Table 2 HCRs 1-3. HCRs 4-6 differ only in the F target,
# which is set by tuning (tunepar) for every MP. The HCR of each MP is selected
# with its other settings (04a-Select-Configs.R)
HCRs <- data.frame(HCR = 1:3, ThRP = c(0.35, 0.30, 0.25))

## Fmin as a fraction of the target F: F90% / F40%
Hist      <- MSEtool::CalcRefPoints(Hist, SPRTarget = c(FTargetSPR, FminSPR), silent = TRUE)
FSPRArr   <- MSEtool::FSPR(Hist)
FminRatio <- stats::median(FSPRArr[, 'Female', , as.character(FminSPR)] /
                             FSPRArr[, 'Female', , as.character(FTargetSPR)])

## F target relative to FMSY, for the Status constraint
FTarget_FMSY <- Calc_FTarget_FMSY(Hist, SPR = FTargetSPR)

# ---- Indices ----
# Japanese late longline CPUE (Areas 1 and 2): fitted in the base case and
# available to the terminal year. S6 is a recruitment index; S5 and S8 have
# lambda = 0 in the base case
IndexNames <- c('S2_JPN_WCNPO_OSDWCOLL_late_Area1', 'S4_JPN_WCNPO_OSDWLL_late_Area2')

# The surplus production model also fits the early Japanese series (1976-1993)
SPIndexNames <- c('S1_JPN_WCNPO_OSDWLL_early_Area1', 'S2_JPN_WCNPO_OSDWCOLL_late_Area1',
                  'S3_JPN_WCNPO_OSDWLL_early_Area2', 'S4_JPN_WCNPO_OSDWLL_late_Area2')

## Index level at SB0: mean index / mean SB/SB0 (base case OM) over IndexSB0Years,
# so the index HCR control points are in units of SB/SB0. Reference windows from
# 2019-2021 to 1994-2021 change it by -12% to -20% (S2) and -2% to +6% (S4)
IndexSB0Years <- 2017:2021

Survey    <- Hist@Data[[1]][[1]]@Survey
stopifnot(all(SPIndexNames %in% Survey@Name))
IndexYear <- floor(as.numeric(rownames(Survey@Value)))
Index     <- Survey@Value[IndexYear %in% IndexSB0Years, match(IndexNames, Survey@Name), drop = FALSE]

Dep <- MSEtool::SB_SB0(Hist, type = 'Equilibrium', df = TRUE, Reduce = FALSE)
Dep <- Dep[Dep$Stock == 'Female' & floor(Dep$Year) %in% IndexSB0Years, ]
DepCalib <- stats::median(tapply(Dep$Value, Dep$Sim, mean))

IndexSB0 <- unname(colMeans(Index, na.rm = TRUE)) / DepCalib

# ---- Tuning Specifications ----
TuneDir <- file.path(normalizePath('objects'), 'Tune')
dir.create(TuneDir, showWarnings = FALSE)

# PM_Removals is the mean catch per time step (quarter); x Seasons gives the mean annual catch
Objective <- MSEtool::TuneObjective(MSEtool::PM_Removals, Name = 'MeanCatch',
                                    Value = local({
                                      Seasons <- Hist@OM@Seasons
                                      function(pm) pm@Mean * Seasons
                                    }))
# Objective <- MSEtool::TuneObjective(MSEtool::PM_LogYield, Type = 'Removals', Name = 'LogYield')

# Candidate management objectives: Safety, P(SB > LRP in every year) >= PSafety,
# and Status, P(F <= F40%) >= PStatus
Constraints <- list(
  MSEtool::TuneConstraint(PM_SafetyNoBreach, Lim = LRP, Min = PSafety, Name = 'Safety'),
  MSEtool::TuneConstraint(PM_StatusF, FTarget_FMSY = FTarget_FMSY, Min = PStatus, Name = 'Status')
)

TuneInterval <- c(0.25, 4)  # initial range of tunepar

# ---- MP Settings Selected by 04a-Select-Configs.R ----
# Candidate values of the settings not set by tuning, including the HCR (HCRs
# 1-3). The values selected for each MP (ConfigsFile) are used for all its
# CMPs; HCR 2 and MSEtool defaults until then
ConfigCandidates <- list(IR = list(HCR = HCRs$HCR, RecentYears = c(1, 3), Smooth = c(TRUE, FALSE)),
                         IT = list(HCR = HCRs$HCR, RecentYears = c(1, 3), Smooth = c(TRUE, FALSE)),
                         SP = list(HCR = HCRs$HCR, Shape = c(0.5, 1, 2)))
DefaultConfig <- list(HCR = 2)

ConfigsFile     <- file.path(TuneDir, 'Configs.rds')
SelectedConfigs <- if (file.exists(ConfigsFile)) readRDS(ConfigsFile) else list()
SelectedConfigs <- lapply(stats::setNames(nm = names(ConfigCandidates)),
                          \(mp) utils::modifyList(DefaultConfig, SelectedConfigs[[mp]] %||% list()))

ConfigLabel <- function(Config) {
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

# MP arguments of a configuration: the HCR control points (from the ThRP of
# the HCR and, for SP, the model Shape) and the other settings
ConfigArgs <- function(MP, Config) {
  Config <- utils::modifyList(DefaultConfig, Config)
  ThRP   <- HCRs$ThRP[HCRs$HCR == Config$HCR]
  Other  <- Config[setdiff(names(Config), 'HCR')]
  HCRArgs <- switch(MP,
                    IR = list(HCRControlPointsIndex   = c(LRP, ThRP),
                              HCRControlPointsRate    = c(FminRatio, 1)),
                    IT = list(HCRControlPointsIndex   = c(LRP, ThRP),
                              HCRControlPointsRate    = c(FminRatio * LRP / ThRP, 1)),
                    SP = list(HCRControlPointsBiomass = c(LRP, ThRP) / BMSY_K(Other$Shape %||% 2),
                              HCRControlPointsRate    = c(FminRatio, 1)))
  c(HCRArgs, Other)
}

MakeCMP <- function(MP, TACChange, Config = SelectedConfigs[[MP]]) {
  Delta <- c(0.01, TACChange)
  Base <- switch(MP,
                 IR = list(MSEtool::IndexRate, Indices = IndexNames, IndexTarget = IndexSB0),
                 IT = list(MSEtool::IndexTarget, Indices = IndexNames, IndexTarget = IndexSB0),
                 SP = list(MSEtool::SurplusProduction, Indices = SPIndexNames))
  do.call(MSEtool::SetMPArgs, c(Base, list(DeltaDown = Delta, DeltaUp = Delta), ConfigArgs(MP, Config)))
}

# Each MP with its selected HCR and settings, and each TAC change limit
CMPs <- expand.grid(MP = c('IR', 'IT', 'SP'), TACChange = TACChange, stringsAsFactors = FALSE)
CMPs$HCR  <- vapply(CMPs$MP, \(mp) as.integer(SelectedConfigs[[mp]]$HCR), integer(1))
CMPs$ThRP <- HCRs$ThRP[match(CMPs$HCR, HCRs$HCR)]
CMPs$Name <- sprintf('%s_H%d_%02d', CMPs$MP, CMPs$HCR, round(100 * CMPs$TACChange))
CMPs <- CMPs[order(CMPs$MP, CMPs$TACChange), ]
rownames(CMPs) <- NULL

CMPs$Fn <- purrr::map2(CMPs$MP, CMPs$TACChange, MakeCMP)
