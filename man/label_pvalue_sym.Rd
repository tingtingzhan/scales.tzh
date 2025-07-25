% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/label_pvalue_sym.R
\name{label_pvalue_sym}
\alias{label_pvalue_sym}
\title{Label \eqn{p}-values with Significance Symbol}
\usage{
label_pvalue_sym(...)
}
\arguments{
\item{...}{parameters of function \link[scales]{label_pvalue}}
}
\value{
Function \code{\link[=label_pvalue_sym]{label_pvalue_sym()}} returns a \link[base]{function}.
}
\description{
Label \eqn{p}-values with significance symbol.
}
\details{
Pipeline:

Step 1: \link[scales]{label_pvalue}.

Step 2: Drop leading zeros.

Step 3: \link[stats]{symnum} (also see function \link[stats]{printCoefmat}).
}
\note{
Much prettier and more flexable than function \link[base]{format.pval}.
}
\examples{
p1 = p2 = c(pi^-100, .02, .05, .1, .9999, NA_real_)

dim(p1) = c(2, 3); p1
p1 |> label_pvalue_sym()() # attr-dim kept
p1 |> label_pvalue_sym(add_p = TRUE)()

names(p2) = letters[1:6]; p2
p2 |> label_pvalue_sym()() # attr-name kept

# below: exception handling
double() |> scales::label_pvalue()() 
# do not like!
# ?scales::pvalue is bad; ?scales::number is fine!

double() |> label_pvalue_sym()()
}
\keyword{internal}
