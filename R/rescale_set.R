

#' @title rescale syntactic sugar
#' 
#' @description ..
#' 
#' @param x ..
#' 
#' @param value syntactic sugar
#' 
#' @param ... ..
#' 
#' @examples 
#' rescale(swiss) = c(100, 0)
#' head(swiss)
#' @name rescale_set
#' @importFrom scales rescale
#' @export
`rescale<-` <- function(x, ..., value) UseMethod(generic = 'rescale<-')

#' @rdname rescale_set
#' @export rescale<-.default
#' @export
`rescale<-.default` <- function(x, ..., value) {
  rescale(x = x, to = value, ...)
}


#' @rdname rescale_set
#' @export rescale<-.list
#' @export
`rescale<-.list` <- function(x, ..., value) {
  x[] <- x |> 
    unclass() |> 
    lapply(FUN = rescale, to = value, ...)
  return(x)
}

#' @rdname rescale_set
#' @export rescale<-.data.frame
#' @export
`rescale<-.data.frame` <- `rescale<-.list`




