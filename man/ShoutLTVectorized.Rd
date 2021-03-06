% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Shout.R
\name{ShoutLTVectorized}
\alias{ShoutLTVectorized}
\title{Shout option valuation via lattice tree (LT)}
\usage{
ShoutLTVectorized(o = OptPx(o = Opt(Style = "Shout")))
}
\arguments{
\item{o}{An object of class \code{OptPx}}
}
\value{
A list of class \code{ShoutLT} consisting of the original \code{OptPx} object,
             binomial tree step \code{BT} and the computed price \code{PxBS}.
}
\description{
A shout option is a European option where the holder can shout to the writer at one time during its life.
             At the end of the life of the option, the option holder receives either the usual payoff from a European option
             or the instrinsic value at the time of the shout, which ever is greater.
             \eqn{max(0,S_T-S_tau)+(S_tau-K)}
}
\examples{
(o = ShoutLTVectorized( OptPx(Opt(Style='Shout'))))$PxLT

o = Opt(Style='Shout')
(o = ShoutLTVectorized( OptPx(o, r=.1, q=.02, vol=.45, NSteps=10)))$PxLT
}
\author{
Le You, Department of Statistics, Rice University, Spring 2015
}
\references{
Hull, J.C., \emph{Options, Futures and Other Derivatives}, 9ed, 2014. Prentice Hall.
ISBN 978-0-13-345631-8, \url{http://www-2.rotman.utoronto.ca/~hull/ofod}. \url{http://amzn.com/0133456315}
}

