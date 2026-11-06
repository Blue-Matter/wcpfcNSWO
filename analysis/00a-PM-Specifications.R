# Reference levels of the performance indicators, used by 06-Run-Projections.R
# and 07-Create-Slick.R

LRP                <- 0.17       # limit reference point: SB / SB0
StabilityThreshold <- 0.1        # maximum TAC change between management cycles for `Stability`
NoChangeThreshold  <- 0.01       # maximum TAC change counted as no management change
PI_Min             <- c(Safety = 0.8, Status = 0.5)  # minimum probabilities of the objectives

# Reference catch for the probability that catch >= historical catch
CatchRefYears <- list(Yield_Hist   = 2008:2010,
                      Yield_Recent = 2016:2021)

# Candidate target reference points (NC22 Summary Report, Attachment 7), the
# F targets of the HCRs. Status is reported against the F target of the HCR of
# each CMP, and against each of these
StatusFTargetSPR <- c(0.40, 0.35, 0.30)
names(StatusFTargetSPR) <- paste0('F', round(100 * StatusFTargetSPR))

#' F targets relative to FMSY in an OM (Calc_FTarget_FMSY()), named e.g. `F40`
PM_FTarget_FMSY <- function(Hist) {
  lapply(StatusFTargetSPR, \(SPR) Calc_FTarget_FMSY(Hist, SPR = SPR))
}
