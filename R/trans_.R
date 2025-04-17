
# many functions here needs re-think..

#' @title More \link{scales}[trans]
#' 
#' @description ..
#' 
#' @param distribution see function \link[scales]{transform_probability}
#' 
# @param base see \link[scales]{log_trans} and \link[scales]{exp_trans}
#' 
#' @param ... potential parameters, currently not in use
#' 
#' @details 
#' Function [transform_quantile()] is the inverse of \link[scales]{transform_probability}.
#' 
#' Function [transform_expit()] transforms logits to proportions, i.e., 
#' reverts the transformation \link[scales]{transform_logit}.
#' 
#' Function [transform_quantit()] transforms standard normal quantiles to probabilities, i.e.,
#' reverts the transformation \link[scales]{transform_probit}.
#' 
#' Function [transform_cloglog()] transforms proportions to complementary-log-log,
#' 
# Function [transform_loglog()] transforms proportions to log-log.
#' 
#' @note
#' \link[scales]{date_trans} is only compatible with \link[base]{Date}.
#' 
#' @examples 
#' x = seq.int(from = .1, to = .9, by = .1)
#' library(ggplot2)
#' (p = ggplot() + geom_path(mapping = aes(x = x, y = seq_along(x))))
#' 
#' p + coord_trans(y = 'sqrt') # scales::transform_sqrt
#' p + coord_trans(y = 'exp') # scales::transform_exp; why warning?
#' p + coord_trans(y = 'log') # scales::transform_log
#' 
#' p + coord_trans(x = 'logit') # scales::transform_logit
#' p + coord_trans(x = 'logit') + coord_trans(x = 'expit') # nice!
#' 
#' p + coord_trans(x = 'probit') # scales::transform_probit
#' p + coord_trans(x = 'probit') + coord_trans(x = 'quantit') # not completely straight?
#' 
#' p + coord_trans(x = 'cloglog')
#' 
#' @keywords internal
#' @name trans_
#' @importFrom scales new_transform
NULL

if (FALSE) {
  identical(scales::trans_new, scales::new_transform) |> stopifnot()
  # but [new_transform()] is used in function definitions in \pkg{scales}
  
  identical(scales::as.trans, scales::as.transform) |> stopifnot()
  
  identical(scales::log_trans, scales::transform_log) |> stopifnot()
  # ?scales::as.transform (e.g., inside ?ggplot2::coord_trans)
  # does `paste0('transform_', x)`  
  identical(scales::exp_trans, scales::transform_exp) |> stopifnot()
  
  identical(scales::transform_probability, scales::probability_trans) |> stopifnot()
  identical(scales::transform_logit, scales::logit_trans) |> stopifnot()
}


# any model return 'loge' or 'ln' as link, should be turned to 'log' manually

#' @rdname trans_
#' @export
transform_quantile <- function(distribution, ...) { # from ?scales::transform_probability
  qfun <- match.fun(paste0('q', distribution))
  pfun <- match.fun(paste0('p', distribution))
  dfun <- match.fun(paste0('d', distribution))
  new_transform(
    name = paste0('quantile-', distribution), 
    transform = function(x) pfun(x, ...),
    inverse = function(x) {
      # inspired by `stats::make.link('cloglog')$linkinv`. important!
      x <- pmax.int(pmin.int(x, 1 - .Machine$double.eps), .Machine$double.eps)
      qfun(x, ...)
    }, 
    d_transform = function(x) dfun(x, ...), # `d_inverse` of ?scales::transform_probability
    d_inverse = function(x) 1/dfun(qfun(x, ...), ...), # `d_transform` of ?scales::transform_probability
    domain = c(-Inf, Inf)
  )
}

#' @rdname trans_
#' @export
transform_quantit <- function() transform_quantile(distribution = 'norm')


#' @rdname trans_
#' @export
transform_expit <- function() transform_quantile(distribution = 'logis')




#' @importFrom stats make.link
link2transform <- function(link, domain) {
  
  if (is.character(link)) {
    if (length(link) != 1L || is.na(link) || !nzchar(link)) stop('illegal `link`') 
    link <- link |> make.link()
  }
  
  if (!inherits(link, what = 'link-glm')) stop('`link` must be convertible to a `link-glm` object')
  
  new_transform(
    name = link$name, 
    transform = link$linkfun, 
    inverse = link$linkinv,
    d_transform = function(x) {
      # see ?scales::transform_probability carefully!
      # also, https://en.wikipedia.org/wiki/Inverse_function_rule
      1 / link$mu.eta(link$linkfun(x))
    },
    d_inverse = link$mu.eta,
    domain = domain # currently requires human input!!!
  )
  
}


#' @rdname trans_
#' @export
transform_cloglog <- function() {
  link2transform(link = 'cloglog', domain = c(0, 1))
}




# remotes::install_github('trobinj/trtools') # *not* on CRAN
# @rdname trans_
# @importFrom trtools loglog
# @export
#transform_loglog <- function() {
#  link2transform(link = trtools::loglog, domain = c(0, 1))
#}





