
# many functions here needs re-think..

#' @title More \link{scales}[trans]
#' 
#' @description ..
#' 
#' @param distribution see function \link[scales]{probability_trans}
#' 
# @param base see \link[scales]{log_trans} and \link[scales]{exp_trans}
#' 
#' @param ... potential parameters, currently not in use
#' 
#' @details 
#' Function [expit_trans] transforms logits to proportions
#' (inverse is \link[scales]{logit_trans})
#' 
#' Function [quantile_trans] is the inverse of \link[scales]{probability_trans}
#' 
#' Function [quantit_trans] is the inverse of \link[scales]{probit_trans}
#' (which transforms proportions to standard normal quantiles)
#' 
#' Function [cloglog_trans] transforms proportions to complementary-log-log,
#' and function [loglog_trans] transforms proportions to log-log.
#' 
#' @note
#' \link[scales]{date_trans} is only compatible with \link[base]{Date}.
#' 
#' @examples 
#' x = seq.int(from = .1, to = .9, by = .1)
#' 
#' library(ggplot2)
#' (p = ggplot() + geom_path(mapping = aes(x = x, y = seq_along(x))))
#' 
#' p + coord_trans(y = 'sqrt') # scales::sqrt_trans
#' p + coord_trans(y = 'exp') # scales::exp_trans
#' p + coord_trans(y = 'log') # scales::log_trans
#' 
#' p + coord_trans(x = 'logit') # scales::logit_trans
#' p + coord_trans(x = 'probit') # scales::probit_trans
#' 
#' \dontrun{
#' p + coord_trans(y = 'quantit') # what does this mean?
#' p + coord_trans(y = 'cloglog')
#' p + coord_trans(y = 'probit') + coord_trans(y = 'quantit')
#' # why not completely straight?
#' # probably not the correct way to use coord_trans()
#' }
#' 
#' @importFrom scales trans_new
#' @name trans_
NULL

if (FALSE) {
  identical(scales::trans_new, scales::new_transform) |> stopifnot()
  # but [new_transform()] is used in [transform_probability()] and [probability_trans()]
  identical(scales::transform_probability, scales::probability_trans) |> stopifnot()
  identical(scales::transform_logit, scales::logit_trans) |> stopifnot()
}


# any model return 'loge' or 'ln' as link, should be turned to 'log' manually

#' @rdname trans_
#' @export
quantile_trans <- function(distribution, ...) { # from \link[scales]{probability_trans}
  qfun <- match.fun(paste0('q', distribution))
  pfun <- match.fun(paste0('p', distribution))
  trans_new(name = paste0('quantile-', distribution), 
            transform = function(x) pfun(x, ...),
            inverse = function(x) {
              # inspired by `make.link('cloglog')$linkinv`. important!
              x <- pmax.int(pmin.int(x, 1 - .Machine$double.eps), .Machine$double.eps)
              qfun(x, ...)
            })
}



#' @rdname trans_
#' @export
quantit_trans <- function() quantile_trans('norm')
#' @rdname trans_
#' @export
expit_trans <- function() quantile_trans('logis')


#' @rdname trans_
#' @importFrom stats make.link
#' @export
cloglog_trans <- function() {
  out <- make.link(link = 'cloglog')
  trans_new(name = 'cloglog', transform = out$linkfun, inverse = out$linkinv)
}

#' @rdname trans_
#' @export
loglog_trans <- function() {
  out <- make_link2(link = 'loglog')
  trans_new(name = 'loglog', transform = out$linkfun, inverse = out$linkinv)
}




#' @title Create More Links
#' 
#' @description ..
#' 
#' @param link \link[base]{character} scalar.  Currently `'loglog'` is supported 
#' 
#' @details 
#' ..
#' 
#' @returns 
#' Function [make_link2] returns a `'link-glm'` object, just like \link[stats]{make.link}
#' 
#' @seealso 
#' \link[stats]{make.link}
#' 
#' @export
make_link2 <- function(link) {
  switch(link, loglog = {
    # remotes::install_github('trobinj/trtools')
    # ?trtools::loglog: not sure if this is correct
    # ?VGAM::logloglink: looks not right?
    # http://people.stat.sfu.ca/~raltman/stat402/402L13.pdf # following based on
    linkfun <- function(mu) log(-log(mu))
    linkinv <- function(eta) pmax.int(pmin.int(exp(-exp(eta)), 1 - .Machine$double.eps), .Machine$double.eps)
    mu.eta <- function(eta) {
      # simply be the negation of `make.link('cloglog')$mu.eta`
      eta <- pmin.int(eta, 700)
      pmin.int(- exp(eta - exp(eta)), -.Machine$double.eps)
    }
    valideta <- function(eta) TRUE
  }, stop('illegal link: ', sQuote(link)))
  environment(linkfun) <- environment(linkinv) <- environment(mu.eta) <- environment(valideta) <- asNamespace('tzh')
  out <- list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta, valideta = valideta, name = link)
  class(out) <- 'link-glm'
  return(out)
}



