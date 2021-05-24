#' Build a table showing numeric values against solution
#'
#' @param x Character vector of names of variables to be checked.
#' @param solenv Solution environment.
#' @param digits Number of digits for formatting output.
#' @param tolerance Tolerance for call to \code{\link{num_vals_close}}.
#' @details Checking performed with `code{\link{attempted}}` and `\code{\link{num_vals_close}}`.
#' @export
chk_num <- function(x, solenv, digits = 3, tolerance = .002) {
  names(x) <- x
  att <- lapply(x, attempted, add = FALSE, inherits = TRUE)
  res <- lapply(x, num_vals_close, sol_env = solenv,
                tolerance = tolerance, add = FALSE, inherits = TRUE)
  result <- unlist(purrr::pmap(list(att, res, x), function(.x, .y, .z) {
    subval <- dplyr::case_when(!.x ~ "not defined",
                        !.y[[1]] ~ "not a single-element numeric vector",
                        TRUE ~ 
                          if (is.null(zz <- safe_get_num(.z, inherits = TRUE, add = FALSE))) {
                            "not a number"
                          } else {
                            format(as.numeric(zz), digits = digits, nsmall = digits)
                          })
    paste("  <tr>",
          paste0("    <td>", chk(.x && all(unlist(.y))), "</td>",
                 "<td style=\"text-align:left;\">", .z, "</td>",
                 "<td style=\"text-align:right;\">",
                 format(as.numeric(get(.z, solenv)),
                        digits = digits, nsmall = digits),
                 "</td><td style=\"text-align:right\">",
                 subval,
                 "</td>"),
          "  </tr>", sep = "\n")
  }))
  c("<table>",
    " <thead>", "  <tr>",
    "<th></th><th></th><th>solution</th><th>your result</th>",
    "  </tr>", " </thead>",
    " <tbody>", result, " </tbody>",
    "</table>")
  ##return(list(att, res))
}
