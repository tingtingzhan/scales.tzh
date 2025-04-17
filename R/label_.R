
#' @title Additional Labeling Functions
#' 
#' @description 
#' Additional labeling `label_*()` functions.
#' 
#' @param scale,suffix,... see function \link[scales]{label_number}
#' 
#' @details
#' To mimic the behavior of function \link[scales]{label_percent}.
#' 
#' @note
#' tzh does not think there is a parameter of function \link[scales]{label_number}
#' to specify the coding of `NA_real_`.
#' 
#' @examples
#' 3.62e-3 |> label_permille(accuracy = .01)()
#' c(3.62e-3, NA_real_) |> label_permille(accuracy = .1)()
#' @name label_
#' @aliases label_permille
#' @keywords internal
#' @importFrom scales label_number
#' @export
label_permille <- function(scale = 1e3, suffix = '\u2030', ...) {
  label_number(scale = scale, suffix = suffix, ...)
}
  

#' @rdname label_
#' @examples
#' 4.2e-4 |> label_per10thousand(accuracy = .1)()
#' @aliases label_per10thousand
#' @importFrom scales label_number
#' @export
label_per10thousand <- function(scale = 1e4, suffix = '\u2031', ...) {
  label_number(scale = scale, suffix = suffix, ...)
}



