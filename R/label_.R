
#' @title Additional `label_*` Functions
#' 
#' @description ..
#' 
#' @param scale,suffix,... see function \link[scales]{number_format}
#' 
#' @details
#' To mimic the behavior of function \link[scales]{label_percent}.
#' 
#' @examples
#' label_permille(accuracy = .1)(3.6e-3)
#' @name label_
#' @aliases label_permille
#' @keywords internal
#' @importFrom scales number_format
#' @export
label_permille <- function(scale = 1e3, suffix = '\u2030', ...) {
  number_format(scale = scale, suffix = suffix, ...)
}
  

#' @rdname label_
#' @examples
#' label_per10thousand(accuracy = .1)(4.2e-4)
#' @aliases label_per10thousand
#' @importFrom scales number_format
#' @export
label_per10thousand <- function(scale = 1e4, suffix = '\u2031', ...) {
  number_format(scale = scale, suffix = suffix, ...)
}



if (FALSE) {
  # Functions \link[scales]{percent} and \link[scales]{percent_format} are now **superseded** by \link[scales]{label_percent}.
  identical(scales::percent_format, scales::label_percent) |> stopifnot()
}