
#' @title Additional Labeling Functions
#' 
#' @description 
#' Additional labeling `label_*()` functions.
#' 
#' @param ... parameters of function \link[scales]{label_number},
#' other than `scale` and `suffix`
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
label_permille <- function(...) {
  label_number(scale = 1e3, suffix = '\u2030', ...)
}
  

#' @rdname label_
#' @examples
#' 4.2e-4 |> label_per10thousand(accuracy = .1)()
#' @aliases label_per10thousand
#' @importFrom scales label_number
#' @export
label_per10thousand <- function(...) {
  label_number(scale = 1e4, suffix = '\u2031', ...)
}





#' @title Label \eqn{p}-values with Significance Symbol
#' 
#' @description
#' Label \eqn{p}-values with significance symbol.
#' 
#' @param ... parameters of function \link[scales]{label_pvalue}
#' 
#' @details
#' 
#' Pipeline:
#' 
#' Step 1: \link[scales]{label_pvalue}.
#' 
#' Step 2: Drop leading zeros.
#' 
#' Step 3: \link[stats]{symnum} (also see function \link[stats]{printCoefmat}).
#' 
#' @note
#' Much prettier and more flexable than function \link[base]{format.pval}.
#' 
#' @returns 
#' Function [label_pvalue_sym()] returns a \link[base]{function}.
#' 
#' @examples 
#' p1 = p2 = c(pi^-100, .02, .05, .1, .9999, NA_real_)
#' 
#' dim(p1) = c(2, 3); p1
#' p1 |> label_pvalue_sym()() # attr-dim kept
#' p1 |> label_pvalue_sym(add_p = TRUE)()
#' 
#' names(p2) = letters[1:6]; p2
#' p2 |> label_pvalue_sym()() # attr-name kept
#' 
#' # below: exception handling
#' double() |> scales::label_pvalue()() # do not like!
#' double() |> label_pvalue_sym()()
#' @keywords internal
#' @importFrom scales label_pvalue
#' @importFrom stats symnum
#' @export
label_pvalue_sym <- function(...) {
  
  function(x) {
    
    ret <- x
    storage.mode(ret) <- 'character'
    
    if (!length(x)) return(ret)
    
    ret[] <- x |> 
      label_pvalue(...)() |>
      sub(pattern = '([-]?)0[.]', replacement = '\\1.') # http://stackoverflow.com/questions/12643391
    
    sym <- symnum(
      x, corr = FALSE, na = FALSE, 
      cutpoints = c(0, .001, .01, .05, .1, 1), 
      symbols = if (getOption('use_unicode')) {
        #c('\u2605\u2605\u2605', '\u2605\u2605', '\u2605', '\u2606', '') # star
        c('\u2b51\u2b51\u2b51', '\u2b51\u2b51', '\u2b51', '\u2b52', '') # small star
      } else c("***", "**", "*", ".", " ")
    ) # see ?stats::printCoefmat
    ret[] <- ret |> paste(sym) |> trimws()
    
    ret[is.na(x)] <- '' # *not* NA_character_
    
    return(ret)
      
  }

}


