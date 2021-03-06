% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Quotient.R
\name{QuotientBS}
\alias{QuotientBS}
\title{Quotient option valuation via Black-Scholes (BS) model}
\usage{
QuotientBS(o = OptPx(Opt(Style = "Quotient")), I1 = 100, I2 = 100,
  g1 = 0.04, g2 = 0.03, sigma1 = 0.18, sigma2 = 0.15, rho = 0.75)
}
\arguments{
\item{o}{An object of class \code{OptPx}}

\item{I1}{A spot price of the underlying security 1 (usually I1)}

\item{I2}{A spot price of the underlying security 2 (usually I2)}

\item{g1}{Payout rate of the first stock}

\item{g2}{Payout rate of the 2nd stock}

\item{sigma1}{a vector of implied volatilities for the associated security 1}

\item{sigma2}{a vector of implied volatilities for the associated security 2}

\item{rho}{is the correlation between asset 1 and asset 2}
}
\value{
A list of class \code{QuotientBS} consisting of the original \code{OptPx} object
and the option pricing parameters \code{I1},\code{I2}, \code{Type}, \code{isForeign}, and \code{isDomestic}
as well as the computed price \code{PxBS}.
}
\description{
Quotient Option via Black-Scholes (BS) model
}
\examples{
(o = QuotientBS())$PxBS

o = OptPx(Opt(Style = 'Quotient', Right = "Put"), r= 0.05)
(o = QuotientBS(o, I1=100, I2=100, g1=0.04, g2=0.03, sigma1=0.18,sigma2=0.15, rho=0.75))$PxBS

o = OptPx(Opt(Style = 'Quotient',  Right = "Put", ttm=1, K=1), r= 0.05)
QuotientBS(o, I1=100, I2=100, g1=0.04, g2=0.03, sigma1=0.18,sigma2=0.15, rho=0.75)

o = OptPx(Opt(Style = 'Quotient',  Right = "Call", ttm=1, K=1), r= 0.05)
QuotientBS(o, I1=100, I2=100, g1=0.04, g2=0.03, sigma1=0.18,sigma2=0.15, rho=0.75)
}
\author{
Chengwei Ge, Department of Statistics, Rice University, Spring 2015
}
\references{
Zhang Peter G., \emph{Exotic Options}, 2nd, 1998. \url{http://amzn.com/9810235216}.
}

