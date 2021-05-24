#' Checkmark, dash, or x depending on logical value
#'
#' @param x Logical value
#' @return HTML with the code for a green checkmark or red X depending on the value of 'x'.
#' @export
chk <- function(x) {
  if (x) "&#9989;" else "&#10006;"
}

#' Build a table showing numeric values against solution
#'
#' @param vnames Character vector of names of variables to be checked.
#' @param results List of results from \code{\link{num_vals_close}}
#' @param nums Vector of numbers from solution (NAs if missing).
#' @param solenv Solution environment.
#' @param format Format of the output (argument passed to  \code{knitr::kable}).
#' @param digits Number of digits for formatting output.
#' @param tolerance Tolerance for call to \code{\link{num_vals_close}}.
#' @details Checking performed with `code{\link{attempted}}` and `\code{\link{num_vals_close}}`.
#' @export
chk_num <- function(vnames, results, nums,
                    solenv, format = "markdown", digits = 3) {
  names(vnames) <- vnames
  result <- dplyr::bind_rows(purrr::pmap(list(results, vnames, nums), function(.yy, .zz, .nn) {
    subval <- if (is.na(.nn)) {
                "not defined"
              } else if (!.yy[1]) {
                "not a single-element numeric vector"
              } else {
                format(as.numeric(.nn),
                       digits = digits, nsmall = digits)
              }
    ltest <- !is.na(.nn[1]) && all(.yy)
    list(` ` = chk(ltest),
         `  ` = .zz,
         solution = format(as.numeric(get(.zz, solenv)),
                           digits = digits, nsmall = digits),
         `your answer` = subval)
    }))
  knitr::kable(result, format, align = "llrr") %>%
    kableExtra::kable_styling(full_width = FALSE)
}

getnum <- function(.v) {
  res <- safe_get_num(.v, env = parent.frame(), inherits = TRUE, add = FALSE)
  if (is.null(res)) NA_real_ else res
}

getdf <- function(.t) {
  .res <- safe_get_type(.t, "htest", env = parent.frame(), inherits = TRUE, add = FALSE)
  if (!is.null(.res)) broom::tidy(.res)[["parameter"]] else NA_real_
}
