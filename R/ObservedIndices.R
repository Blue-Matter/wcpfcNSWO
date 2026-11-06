#' Use the observed indices in an OM conditioned on adjusted indices
#'
#' For an OM conditioned on index values that were adjusted before the SS3 fit
#' (e.g. for effort creep, see [AddEffortCreep()]), the MPs should see the
#' indices as observed. `Simulate()` fits the index catchability and residuals
#' to the indices in `OM@Data`, so these must stay as the adjusted values.
#'
#' `StoreObservedIndices()` stores the observed indices in `OM@Misc`, with the
#' ratio of the observed to the adjusted value in the last year of each index.
#' `ApplyObservedIndices()` is used on the `Hist` object after `Simulate()`:
#' it replaces the historical indices in `Hist@Data` with the observed values,
#' and multiplies the catchability (`Efficiency`) of each index by its ratio, so
#' the projected indices continue from the last observed value (i.e., the
#' adjustment, such as effort creep, only applies to the historical period).
#'
#' @param OM an `om` object conditioned on the adjusted indices
#' @param Observed an `indicesdata` object with the observed indices (e.g.,
#'   `OM@Data[[1]]@Survey` of the base case OM)
#' @param Hist a `hist` object from `Simulate()`
#'
#' @returns the modified `OM` or `Hist` object
#' @export
StoreObservedIndices <- function(OM, Observed) {
  Adjusted <- OM@Data[[1]]@Survey
  if (!identical(Adjusted@Name, Observed@Name) ||
      !identical(dimnames(Adjusted@Value), dimnames(Observed@Value)))
    cli::cli_abort('The observed and adjusted indices must have the same indices and years')

  Ratio <- vapply(seq_along(Observed@Name), \(fl) {
    last <- max(which(!is.na(Observed@Value[, fl]) & !is.na(Adjusted@Value[, fl])))
    Observed@Value[last, fl] / Adjusted@Value[last, fl]
  }, numeric(1))
  names(Ratio) <- Observed@Name

  OM@Misc$ObservedIndices <- list(Survey = Observed@Value, Ratio = Ratio)
  OM
}

#' @rdname StoreObservedIndices
#' @export
ApplyObservedIndices <- function(Hist) {
  Stored <- Hist@OM@Misc$ObservedIndices
  if (is.null(Stored)) return(Hist)

  for (sim in seq_along(Hist@Data)) {
    Survey <- Hist@Data[[sim]][[1]]@Survey
    rows   <- intersect(rownames(Survey@Value), rownames(Stored$Survey))
    cols   <- match(Survey@Name, colnames(Stored$Survey))
    Survey@Value[rows, ] <- Stored$Survey[rows, cols]
    Hist@Data[[sim]][[1]]@Survey <- Survey
  }

  for (fl in names(Stored$Ratio)) {
    Hist@OM@Obs[[1]][[fl]]@Survey@Efficiency <-
      Hist@OM@Obs[[1]][[fl]]@Survey@Efficiency * Stored$Ratio[[fl]]
  }
  Hist
}
