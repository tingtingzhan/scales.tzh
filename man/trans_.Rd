% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trans_.R
\name{trans_}
\alias{trans_}
\alias{transform_quantile}
\alias{transform_quantit}
\alias{transform_expit}
\alias{transform_cloglog}
\title{More \link[scales]{trans}formation Functions}
\usage{
transform_quantile(distribution, ...)

transform_quantit()

transform_expit()

transform_cloglog()
}
\arguments{
\item{distribution}{see function \link[scales]{transform_probability}}

\item{...}{potential parameters, currently not in use}
}
\description{
..
}
\details{
Function \code{\link[=transform_quantile]{transform_quantile()}} is the inverse of \link[scales]{transform_probability}.

Function \code{\link[=transform_expit]{transform_expit()}} transforms logits to proportions, i.e.,
reverts the transformation \link[scales]{transform_logit}.

Function \code{\link[=transform_quantit]{transform_quantit()}} transforms standard normal quantiles to probabilities, i.e.,
reverts the transformation \link[scales]{transform_probit}.

Function \code{\link[=transform_cloglog]{transform_cloglog()}} transforms proportions to complementary-log-log,
}
\examples{
x = seq.int(from = .1, to = .9, by = .1)
library(ggplot2)
(p = ggplot() + geom_path(mapping = aes(x = x, y = seq_along(x))))

p + coord_trans(y = 'sqrt') # scales::transform_sqrt
p + coord_trans(y = 'exp') # scales::transform_exp; why warning?
p + coord_trans(y = 'log') # scales::transform_log

p + coord_trans(x = 'logit') # scales::transform_logit
p + coord_trans(x = 'logit') + coord_trans(x = 'expit') # nice!

p + coord_trans(x = 'probit') # scales::transform_probit
p + coord_trans(x = 'probit') + coord_trans(x = 'quantit') # not completely straight?

p + coord_trans(x = 'cloglog')

}
\keyword{internal}
