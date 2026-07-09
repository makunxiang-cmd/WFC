#' Calibrate weights with a selected method
#'
#' Dispatches to the requested calibration engine while preserving the common
#' `wf_weights` return contract.
#'
#' @param sample Sample data frame.
#' @param target A `wf_target` object.
#' @param method Calibration method, either `"raking"` or `"poststrat"`.
#' @param ... Method-specific arguments passed to `wf_rake()` or
#'   `wf_poststrat()`.
#'
#' @return A `wf_weights` object.
#' @export
wf_calibrate <- function(sample, target, method = "raking", ...) {
  supported <- c("raking", "poststrat")
  if (length(method) != 1 || !method %in% supported) {
    shown <- if (length(method) == 0) "<empty>" else as.character(method[[1]])
    wf_abort(
      sprintf(
        "Unsupported calibration method '%s'. Supported methods: raking, poststrat.",
        shown
      ),
      "wf_error_input",
      list(method = method)
    )
  }

  if (method == "raking") {
    out <- wf_rake(sample, target, ...)
    out$provenance$method <- "raking"
    return(out)
  }

  wf_poststrat(sample, target, ...)
}
