% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Shout.R
\name{ShoutLT}
\alias{ShoutLT}
\title{Shout option valuation via lattice tree (LT)}
\usage{
ShoutLT(o = OptPx(Opt(Style = "Shout")), IncBT = TRUE)
}
\arguments{
\item{o}{An object of class \code{OptPx}}

\item{IncBT}{TRUE/FALSE indicating whether to include binomial tree (list object) with output}
}
\value{
A list of class \code{ShoutLT} consisting of the original \code{OptPx} object,
             binomial tree step\code{BT} and the computed price \code{PxBS}.
}
\description{
A shout option is a European option where the holder can shout to the writer at one time during its life.
             At the end of the life of the option, the option holder receives either the usual payoff from a European option
             or the instrinsic value at the time of the shout, which ever is greater.
             \eqn{max(0,S_T-S_tau)+(S_tau-K)}
}
\examples{
(o = ShoutLT( OptPx(Opt(Style='Shout'))))$PxLT

o = Opt(Style='Shout', Right='Call', S0=60, ttm=.25, K=60)
ShoutLT( OptPx(o,r=.1, q=.02, vol=.45, NSteps=10))

o = Opt(Style='Shout', Right='Call', S0=60, ttm=.25, K=60)
}
\author{
Le You, Department of Statistics, Rice University, Spring 2015
}
\references{
Hull, J.C., \emph{Options, Futures and Other Derivatives}, 9ed, 2014. Prentice Hall.
ISBN 978-0-13-345631-8, \url{http://www-2.rotman.utoronto.ca/~hull/ofod}. \url{http://amzn.com/0133456315}
}

