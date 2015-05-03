# packaging steps and troubleshooting ####
# install.packages('devtools'); library(devtools); 
# install.packages('roxygen2'); library(roxygen2); 
# install.packages('testthat'); library(testthat)
# devtools::install_github("rstudio/packrat")
# Tools|Project Options|Packrat|Use Packrat with this project
# Tools|Project Options|Build Tools|Configure
# Replace DESCRIPTION file; Delete "Read and delete me" file
# devtools::document()
# roxygen2::roxygenize('QFRM')
# .libPaths(c("z:/", .libPaths()))
# system("R CMD INSTALL QFRM")
# Uninstal "inconsolata" MiKTeX package (in Package Manager)
# run "R CMD Rd2pdf --pdf QFRM" in z:
# options: --as-cran --no-manual --no-vignettes --no-clean, support.rstudio.com/hc/en-us/articles/200486518
# run `updmap` on Win command line to fix inconsoladate MikTeX package issue with "Font ts1-zi4r at 540 not found"

## Opt #####
#' \code{Opt} object constructor
#' @description An S3 object constructor for an option contract (financial derivative)
#' @author Oleg Melnikov, Department of Statistics, Rice University, Spring 2015
#' 
#' @param Style   An option style: \code{European} or \code{American}. Partial names are allowed, eg. \code{E} or \code{A}
#' @param Right   An option right: \code{Call} or \code{Put}. Partial names are allowed.
#' @param S0      A spot price of the underlying security (usually, today's stock price, \eqn{S_0})
#' @param ttm     A time to maturity, in units of time matching r units; usually years
#' @param K       A strike price 
#' @param Curr    An optional currency units for monetary values of the underlying security and an option
#' @param ContrSize A contract size, i.e. number of option shares per contract
#' @param SName    A (optional) descriptful name of the underlying. Eg. \emph{Microsoft Corp}
#' @param SSymbol  An (optional) official ticker of the underlying. Eg. \emph{MSFT}
#' 
#' @return A list of class \code{Opt}
#' @examples
#' Opt()  #Creates an S3 object for an option contract  
#' Opt(Right='Put')   #See J.C.Hull, OFOD'2014, 9-ed, Fig.13.10, p.289
#' @export
#' 
Opt = function(Style=c('European','American','Asian','Binary','AverageStrike','Barrier',
                       'Chooser','Compound','DeferredPayment','ForeignEquity','ForwardStart','Gap','HolderExtendible',
                       'Ladder','Lookback','MOPM','Perpetual','Quotient','Rainbow','Shout','SimpleChooser','VarianceSwap'), 
               Right=c('Call','Put','Other'), S0=50, ttm=2, K=52, 
               Curr='$', ContrSize=100, SName='A stock share', SSymbol='') { 


  Style = match.arg(Style)
  s=list(Name=Style)
  s$Vanilla = (Style == 'European' ||  Style == 'American')
  s$Exotic = !s$Vanilla
  s$European = (Style == 'European')
  s$American = (Style == 'American')
  s$Asian = (Style == 'Asian')
  s$Binary = (Style == 'Binary')
  s$AverageStrike = (Style == 'AverageStrike')
  s$Barrier = (Style == 'Barrier')
  s$Chooser = (Style == 'Chooser')
  s$Compound = (Style == 'Compound')
  s$DP = (Style == 'DeferredPayment')
  s$ForeignEquity = (Style == 'ForeignEquity')
  s$ForwardStart = (Style == 'ForwardStart')
  s$Gap = (Style == 'Gap')
  s$HolderExtendible = (Style == 'HolderExtendible')
  s$Ladder = (Style == 'Ladder')
  s$Lookback = (Style == 'Lookback')
  s$MOPM = (Style == 'MOPM')
  s$Perpetual = (Style == 'Perpetual')
  s$Quotient = (Style == 'Quotient')
  s$Rainbow = (Style == 'Rainbow')
  s$Shout = (Style == 'Shout')
  s$SimpleChooser = (Style == 'SimpleChooser')
  s$VarianceSwap = (Style == 'VarianceSwap')
  
  Right = match.arg(Right)
  r=list(Name=Right)
  r$Call = (Right == 'Call')
  r$Put = (Right == 'Put')
  r$Other = (Right == 'Other')
  r$SignCP = (r$Call*2-1)* if (r$Other) NA else 1   # sign: 1 for Call, -1 for Put
  
  o = list(S0=S0, ttm=ttm, K=K,Style=s, Right=r, Curr=Curr, ContrSize=ContrSize, SName=SName, SSymbol=SSymbol);
  
  class(o)='Opt'; # give your list a (class) name
  return(o)
}

## OptPx #####
#' \code{OptPx} object constructor
#' @description An S3 object constructor for lattice-pricing specifications for an option contract. \code{Opt} object is inhereted.
#' @author Oleg Melnikov, Department of Statistics, Rice University, Spring 2015
#' 
#' @param o   An object of class \code{Opt}
#' @param r   A risk free rate (annualized)
#' @param q   A dividend yield (as annualized rate), Hull/p291
#' @param rf  A foreign risk free rate (annualized), Hull/p.292
#' @param vol A volaility (as Sd.Dev, sigma)
#' @param n   A number of time steps in BOPM calculation
#' 
#' @return A list of class \code{OptPx} with parameters supplied to \code{Opt} and \code{OptPx} constructors
#' @examples
#' OptPx()  #Creates an S3 object for an option contract
#' 
#' #See J.C.Hull, OFOD'2014, 9-ed, Fig.13.10, p.289  
#' OptPx(Opt(Right='Put'))  
#' 
#' o = OptPx(Opt(Right='Call', S0=42, ttm=.5, K=40), r=.1, vol=.2)  
#' @export
#' 
OptPx = function(o=Opt(), r=0.05, q=0, rf=0, vol=.30, n=2) { 
  stopifnot(is.Opt(o), is.numeric(r), r>0, r<1, is.numeric(vol), is.numeric(q), is.numeric(rf))
  
  dt=o$ttm/n           # time interval between consequtive two time steps
  u=exp(vol*sqrt(dt))  # stock price up move factor 
  d=1/u                # stock price down move factor
  SYld = r-q-rf        # yield of the underlying asset, p.458
  a = exp(SYld * dt)   # growth factor, p.452

  o$r = r;   o$q = q;   o$rf = rf;   o$vol = vol
  o$n = n;   o$u = u;   o$d = d;   o$dt = dt;   o$a = a
  o$p = p=(a-d)/(u-d) # probability of up move over one time interval dt
  o$SYld = SYld
#   o$BS = BS
  o$DF_ttm = exp(-r * o$ttm)
  o$DF_dt = exp(-r * dt)  # discount factor over one time interval dt, i.e. per step
  
  class(o) = c(class(o), 'OptPx')   #-- Child 'OptPx' inherits properties of parent 'Opt'
  return(o)
}

## OptPos #####
#' \code{OptPos} object constructor
#' @description S3 object constructor for lattice-pricing specs of an option contract. Inherits \code{Opt} object.
#' @author Oleg Melnikov, Department of Statistics, Rice University, Spring 2015
#' 
#' @param o     An object of class \code{Opt}s
#' @param Pos   A position direction (to the holder) with values \code{Long} for owned option contract and \code{Short} for shorted contract.
#' @param Prem  A option premim (i.e. market cost or price), a non-negative amount to be paid for the option contract being modeled.  
#' 
#' @return A list of class \code{OptPx}
#' @examples 
#' OptPos()  # Creates an S3 object for an option contract
#' OptPos(Opt(Right='Put'))  #See J.C.Hull, OFOD'2014, 9-ed, Fig.13.10, p.289
#' @export
#' 
OptPos = function(o=Opt(), Pos=c('Long', 'Short'), Prem=0){
  stopifnot(is.Opt(o))  #assure that Opt object is passed in
  
  Pos = match.arg(Pos)
  p = list(Name=Pos)
  p$Long = (Pos == 'Long')
  p$Short = (Pos == 'Short')
  p$Other = !(p$Long || p$Short)
  p$SignLS=(2*p$Long-1) * (if (p$Other) NA else 1)  #sign indicator. 1 for Long, -1 for short
  
  o$Pos = p
  o$Prem = Prem
  
  class(o)=c(class(o), 'OptPos')   #-- Child 'OptPos' inherits properties of a parent
  return(o)
}

## is.* #####
#' Is an object \code{Opt}?
#' @description Tests the argument for the specific class type.
#' @author Oleg Melnikov
#' @param o     Any object
#' @return TRUE if and only if an argument is of \code{Opt} class.
#' @examples 
#' is.Opt(Opt())  #verifies that Opt() returns an object of class \code{Opt}
#' is.Opt(1:3)    #verifies that code{1:3} is not an object of class \code{Opt}
#' @export
#' 
is.Opt = function(o) is(o,'Opt')

#' Is an object \code{OptPx}?
#' @description Tests the argument for the specific class type.
#' @author Oleg Melnikov
#' @param o     Any object
#' @return TRUE if and only if an argument is of \code{OptPx} class.
#' @examples 
#' is.OptPx(OptPx(Opt(S0=20), r=0.12))
#' @export
#' 
is.OptPx = function(o) is(o,'OptPx')

#' Is an object \code{OptPos}?
#' @description Tests the argument for the specific class type.
#' @author Oleg Melnikov
#' @param o     Any object
#' @return TRUE if and only if an argument is of \code{OptPos} class.
#' @examples 
#' is.OptPos(OptPos())
#' @export
#' 
is.OptPos = function(o) is(o, 'OptPos')

#' Coerce an argument to \code{OptPos} class.
#' @author Oleg Melnikov
#' @param o    A \code{Opt} or \code{OptPx} object
#' @param Pos Specify position direction in your portfolio. \code{Long} indicates that you own security (it's an asset). \code{Short} that you shorted (short sold) security (it's a liability).
#' @param Prem Option premium, i.e. cost of an option purchased or to be purchased.
#' @return An object of class \code{OptPos}.
#' @examples 
#' as.OptPos(Opt())
#' @export
#' 
as.OptPos = function(o=Opt(), Pos=c('Long', 'Short'), Prem=0){
  stopifnot((is.OptPos(o) || is.Opt(o) || is.OptPx(o)))
  if (!is.OptPos(o)) o = OptPos(o) 
  return(o)
}

## BOPM_Eu #####
#' European option valuation (vectorized computation).
#' @description A helper function to price European options via a vectorized (fast, memory efficient) approach.
#' @author Oleg Melnikov, Department of Statistics, Rice University, Spring 2015
#' Code adopted Gilli & Schumann's R implementation to \code{Opt*} objects
#' @param o   An \code{OptPx} object
#' @seealso \url{http://papers.ssrn.com/sol3/papers.cfm?abstract_id=1341181} for original paper, \code{\link{BOPM}} for American option pricing.
#' @return A list of class \code{OptPx} with an element \code{PxBT}, which is an option price value (type \code{double}, class \code{numeric})
#' 
#' @references Gili, M. and Schumann, E. (2009) \emph{Implementing Binomial Trees}, COMISEF Working Papers Series
#' @examples 
#' #Fig.13.11, Hull/9e/p291:
#' o = Opt(Style='European', Right='Call', S0=810, ttm=.5, K=800)
#' (o <- BOPM_Eu( OptPx(o, r=.05, q=.02, vol=.2, n=2)))$PxBT
#' 
#' o = Opt('Eu', 'C', 0.61, .5, 0.6, SName='USD/AUD')
#' o = OptPx(o, r=.05, q=.02, vol=.12, n=2)
#' (o <- BOPM_Eu(o))$PxBT
#' @export
#' 
BOPM_Eu = function(o=OptPx()){
  stopifnot(is.OptPx(o), o$Style$European) # this function needs OptPx object and European option
  
  with(o, {
    S = S0*d^(n:0)*u^(0:n)            # vector of terminal stock prices, lowest to highest (@t=ttm)
    O = pmax(o$Right$SignCP*(S - K), 0)          # vector of terminal option payouts (@t=ttm)
    csl = cumsum(log(c(1,1:n)))       #-- logs avoid overflow & truncation
    tmp = csl[n+1] - csl - csl[(n+1):1] + log(p)*(0:n) + log(1-p)*(n:0)
    o$PxBT = DF_ttm * sum(exp(tmp)*O)
    return(o) #-- spot option price (at time 0)
  })
}

# BS #####
#' Black-Scholes (BS) pricing model 
#' @description a wrapper function for BS_Simple; uses \code{OptPx} object as input.
#' @author Oleg Melnikov, Department of Statistics, Rice University, Spring 2015
#' @param o   An \code{OptPx} object
#' @return An original \code{OptPx} object with \code{BS} list as components of Black-Scholes formular. 
#'   See \code{BS_Simple}.
#' @references Hull, J.C., \emph{Options, Futures and Other Derivatives}, 9ed, 2014. Prentice Hall. 
#' ISBN 978-0-13-345631-8, \url{http://www-2.rotman.utoronto.ca/~hull/ofod/index.html}.
#' @examples 
#' #See Hull, p.338, Ex.15.6. #Create an option and price it
#' o = Opt(Style='Eu', Right='Call', S0 = 42, ttm = .5, K = 40) 
#' o = BS( OptPx(o, r=.1, vol=.2, n=NA)) 
#' o$PxBS #print call option price computed by Black-Scholes pricing model
#' o$BS$Px$Put #print put option price computed by Black-Scholes pricing model
#' 
#' @export
#' 
BS = function(o=OptPx()){ #o=OptPx()
  stopifnot(is.OptPx(o)); # algorithm requires that a OptPx object is provided

  o$BS = BS_Simple(o$S0, o$K, o$r, o$q, o$ttm, o$vol)  # add BS components to o object
  o$PxBS = with(o, if (Right$Call) BS$Px$Call else {if (Right$Put) BS$Px$Put else NA})
  return(o)
}

# BS_Simple input ####
#' Black-Scholes formula
#' @description Black-Scholes (aka Black-Scholes-Merton, BS, BSM) formula for simple parameters
#' @author Robert Abramov, Department of Statistics, Rice University, Spring 2015
#'
#' @details
#' Uses BS formula to calculate call/put option values and elements of BS model
#' 
#' @param S0  The spot price of the underlying security
#' @param K  The srike price of the underlying (same currency as S0)
#' @param ttm,  The time to maturity, fraction of a year (annualized)
#' @param r  The annualized risk free interest rate, as annual percent / 100 
#' (i.e. fractional form. 0.1 is 10 percent per annum)
#' @param q  The annualized dividiend yield, same units as \code{r}
#' @param vol The volatility, in units of standard deviation.
#' 
#' @return a list of BS formula elements and BS price, 
#' such as \code{d1} for \eqn{d_1}, \code{d2} for \eqn{d_2}, \code{Nd1} for \eqn{N(d_1)}, 
#' \code{Nd2} for \eqn{N(d_2)}, N\code{CallPxBS} for BSM call price, \code{PutPxBS} for BSM put price
#' 
#' @references Hull, J.C., \emph{Options, Futures and Other Derivatives}, 9ed, 2014. Prentice Hall. 
#' ISBN 978-0-13-345631-8, \url{http://www-2.rotman.utoronto.ca/~hull/ofod/index.html}.
#' \url{http://www.theresearchkitchen.com/archives/106}
#'
#' @examples
#' #See Hull p.339, Ex.15.6.
#' (o <- BS_Simple(S0=42,K=40,r=.1,q=0,ttm=.5,vol=.2))$Px$Call #returns 4.759422
#' o$Px$Put # returns 0.8085994 as the price of the put
#' 
#' BS_Simple(100,90,0.05,0,2,0.30)
#' BS_Simple(50,60,0.1,.2,3,0.25)
#' BS_Simple(90,90,0.15,0,.5,0.20)
#' BS_Simple(15,15,.01,0.0,0.5,.5)
#' @export
#' 
BS_Simple <- function(S0=42, K=40, r=.1, q=0, ttm=.5, vol=.2) {
  stopifnot(is.numeric(S0), S0>=0, 
            is.numeric(K), K>0, 
            is.numeric(r), r>=0 && r<=1, 
            is.numeric(q), q>=0 && q<=1,
            is.numeric(ttm), ttm>0, 
            is.numeric(vol), vol>0)
  
  d1 = (log(S0/K)+(r-q+(vol^2)/2)*ttm)/(vol*sqrt(ttm))
  d2 = d1 - vol * sqrt(ttm)
  Nd1 = pnorm(d1)
  Nd2 = pnorm(d2) #probability that a call option is exercised in a risk-neutral world, see Hull, p.337
  
  Call = S0*exp(-q*ttm)*Nd1 - K*exp(-r*ttm)*Nd2  # See Hull, p.335, (15.20); p.373, (17.4)
  Put = K*exp(-r*ttm) * pnorm(-d2) - S0*exp(-q*ttm)*pnorm(-d1) # See Hull, p.335, (15.21); p.373.(17.5)
  Px = list(Call = Call, Put = Put) 
  BS = list(d1=d1, d2=d2, Nd1=Nd1, Nd2=Nd2, Px=Px)
  return(BS)
}

# BOPM #####
#' Binomial option pricing model
#' @description Compute option price via binomial option pricing model (recombining symmetric binomial tree). 
#'  If no tree requested for European option, vectorized algorithm is used. 
#' @author Oleg Melnikov, Department of Statistics, Rice University, Spring 2015
#' @param o   An \code{OptPx} object
#' @param IncBT Values \code{TRUE} or \code{FALSE} indicating whether to include a list of all option tree values (underlying and derivative prices) in the returned \code{OptPx} object. 
#' @seealso \code{\link{BOPM_Eu}} for European option via vectorized approach.
#' @return An original \code{OptPx} object with \code{PxBT} field as the binomial-tree-based price of an option 
#' and (an optional) the fullly-generated binomial tree in \code{BT} field.
#'  \code{IncBT = FALSE}: option price value (type \code{double}, class \code{numeric})
#'  \code{IncBT = TRUE}: binomial tree as a list (of length (\code{o$n+1}) of numeric matrices (2 x \code{i}). 
#'    Each matrix is a set of possible i outcomes at time step i 
#'    columns: (underlying prices, option prices)
#' @references Hull, J.C., \emph{Options, Futures and Other Derivatives}, 9ed, 2014. Prentice Hall. 
#' ISBN 978-0-13-345631-8, \url{http://www-2.rotman.utoronto.ca/~hull/ofod/index.html}.
#' @examples 
#' #See Fig.13.11, Hull/9e/p291. #Create an option and price it
#' o = Opt(Style='Eu', Right='C', S0 = 808, ttm = .5, K = 800) 
#' o = BOPM( OptPx(o, r=0.05, q=0.02, vol=0.2, n=2), IncBT=TRUE) 
#' o$PxBT #print added calculated price to PxBT field
#' 
#' #Fig.13.11, Hull/9e/p291:
#' o = Opt(Style='Eu', Right='C', S0=810, ttm=.5, K=800)
#' BOPM( OptPx(o, r=0.05, q=0.02, vol=0.2, n=2), IncBT=TRUE)$PxBT 
#' 
#' #DerivaGem diplays up to 10 steps:
#' o = Opt(Style='Am', Right='C', 810, .5, 800)
#' BOPM( OptPx(o, r=0.05, q=0.02, vol=0.2, n=20), IncBT=TRUE)      
#' 
#' #DerivaGem computes up to 500 steps:
#' o = Opt(Style='American', Right='Put', 810, 0.5, 800)
#' BOPM( OptPx(o, r=0.05, q=0.02, vol=0.2, n=1000), IncBT=FALSE)   
#' @export
#' 
BOPM = function(o=OptPx(), IncBT=TRUE){ #o=OptPx()
  stopifnot(is.OptPx(o), o$Style$Vanilla); # algorithm requires that a OptPx object is provided
  n=o$n; p=o$p; K=o$K
  
  if (o$Style$European && !IncBT) return(BOPM_Eu(o)) else { 
    S = with(o, S0*d^(0:n)*u^(n:0)) # vector of terminal stock prices, lowest to highest (@t=ttm)
    O = pmax(o$Right$SignCP * (S - K), 0) # vector of terminal option payouts (@t=ttm)
    #-- American option pricing
    #-- a vector stores stock prices at any time point
    
    RecalcOSonPriorTimeStep = function(i) { #sapply(1:(i-1), function(j) 
      O <<- o$DF_dt * (p*O[-i-1] + (1-p)*O[-1])  #prior option prices (@time step=i-1)
      S <<- o$d * S[-i-1]                   # prior stock prices (@time step=i-1)
      Payout = pmax(o$Right$SignCP * (S - K), 0)   # payout at time step i-1 (moving backward in time)
      if (o$Style$American) O <<- pmax(O, Payout)    # 
      return(cbind(S, O))
    }
    
    BT = append(list(cbind(S, O)), sapply(n:1, RecalcOSonPriorTimeStep)) #binomial tree
    o$PxBT = BT[[length(BT)]][[2]]  # add BOPM price
    if (IncBT) o$BT = BT
    return(o)
  } 
}

# Profit #####
#' Computes payout/profit values
#' @description Computes payout/profit values
#' @author Oleg Melnikov, Department of Statistics, Rice University, Spring 2015
#' @param o   An object of class \code{Opt*}
#' @param S   A (optional) vector or value of stock price(s) (double) at which to compute profits
#' @return A numeric matrix of size \code{[length(S), 2]}. Columns: stock prices, corresponding option profits
#' @examples 
#' Profit(o=Opt())
#' plot( print( Profit(OptPos(Prem=2.5), S=40:60)), type='l'); grid()
#' @export
#' 
Profit = function(o=OptPos(), S=o$S0){
  stopifnot(is.Opt(o))
  o = as.OptPos(o)  #assure option position object (with default values, if needed)
  
  return(cbind(S, Profit=o$Pos$SignLS * ( pmax(o$Right$SignCP*(S-o$K), 0)-o$Prem)))
} 





#   plot.Opt #####
# #' Plots an option payout/profit diagram
# #' @description Plots an option payout/profit diagram 
# #' @author Oleg Melnikov
# #' @param o   An \code{OptPos} object
# #' @param S   An optional vector of stock prices at which to plot the profits
# #' @param ... graphics parameters to be passed to the plotting routines.
# #' @references Hul, J.C., \emph{Options, Futures and Other Derivatives}, 9ed, 2014. Prentice Hall
# #' @details A plot net profit diagram and OptPos object with Profit matrix used for plotting.
# #' 
# #' @examples
# #' plot.Opt()
# #' plot.Opt(Opt(Right='Put'))
# #' plot.Opt(OptPos(Prem=0.75))
# #' plot.Opt(OptPos(Pos='Short', Prem=0.75))
# #' @export
#' 
# plot.Opt = function(o=OptPos(), S=o$S0, ...) {
#   stopifnot(is.Opt(o)); o = as.OptPos(o)
#   dots = list(...)  #extract additional function arguments 
#   
#   with(o, {
#     vS = sort(c(S, K, K + max(Prem, 1) * 0.02 * (-100:100)))  # vector of stock prices (suitable for plotting)
#     vProfit = Profit(o, vS)   #matrix of stock prices and corresponding profits
#     
# #     if (is.null(main)) main=paste(Pos, Right, 'profit diagram.')
# #     if (is.null(col)) col=switch(Pos, Long='blue', Short='green')
# #     if (is.null(lty)) lty=switch(Right, Call='dashed', Put='dotted')
# #     if (is.null(xlim)) xlim=range(vS)
# #     if (is.null(ylim)) ylim=range(vProfit[,2])
# #     if (is.null(xlab)) xlab=paste('S,',Curr)
# #     if (is.null(ylab)) ylab=paste('Profit,',Curr)
# #     #     if (is.null(type)) type='l'
# #     #     if (is.null(lwd)) type=1
# #     
# #     plot(0,0, xlim=xlim, ylim=ylim, type='n', xlab=xlab, ylab=ylab, main=main,...); # draw blank canvas
# #     grid();
# #     abline(0, 0, col='dark gray')
# #     points(K, 0, col='black', pch=3)
# #     
# #     lines(vProfit, lwd=lwd, type=type, col=col, lty=lty);   # print diagram
# #     text(K, 0, paste('K =',K), pos=3, col=col)
# #     o$Profit = vProfit   # add profit matrix 
# #     
#     return(o)
#   })
# }



# @param main A title for a plot. See graphical parameters
# @param col See graphical parameters
# @param lty See graphical parameters
# @param xlim See graphical parameters
# @param ylim See graphical parameters
# @param xlab See graphical parameters
# @param ylab See graphical parameters
# @param type See graphical parameters
# @param lwd See graphical parameters 

# plot.Opt = function(o=OptPos(), S=o$S0, 
#                     type='l', xlim=NULL, ylim=NULL, main=NULL, xlab=NULL, ylab=NULL, 
#                     col=NULL, lty=NULL, lwd=1,...) {
#   stopifnot(is.Opt(o)); o = as.OptPos(o)
#   dots = list(...)  #extract additional function arguments 
#   
#   with(o, {
#     vS = sort(c(S, K, K + max(Prem, 1) * 0.02 * (-100:100)))  # vector of stock prices (suitable for plotting)
#     vProfit = Profit(o, vS)   #matrix of stock prices and corresponding profits
#     
#     if (is.null(main)) main=paste(Pos, Right, 'profit diagram.')
#     if (is.null(col)) col=switch(Pos, Long='blue', Short='green')
#     if (is.null(lty)) lty=switch(Right, Call='dashed', Put='dotted')
#     if (is.null(xlim)) xlim=range(vS)
#     if (is.null(ylim)) ylim=range(vProfit[,2])
#     if (is.null(xlab)) xlab=paste('S,',Curr)
#     if (is.null(ylab)) ylab=paste('Profit,',Curr)
#     #     if (is.null(type)) type='l'
#     #     if (is.null(lwd)) type=1
#     
#     plot(0,0, xlim=xlim, ylim=ylim, type='n', xlab=xlab, ylab=ylab, main=main,...); # draw blank canvas
#     grid();
#     abline(0, 0, col='dark gray')
#     points(K, 0, col='black', pch=3)
#     
#     lines(vProfit, lwd=lwd, type=type, col=col, lty=lty);   # print diagram
#     text(K, 0, paste('K =',K), pos=3, col=col)
#     o$Profit = vProfit   # add profit matrix 
#     
#     return(o)
#   })
# }






# plot.ts ####
# BOPM.n = function(i) BOPM( OptPx(Opt('Am', 'P', 810, 0.5, 800), r=0.05, q=0.02, vol=0.2, n=i), IncBT=FALSE)$PxBT
# plot.ts(sapply(2:100, BOPM.n))
# 
# BT = BOPM( OptPx(Opt('Am', 'P', 810, 0.5, 800), r=0.05, q=0.02, vol=0.2, n=10))$BT
# 
# lapply(1:10, function(i) BT[[i]][,1])
# 
# 

# source('Z:/QFRM/R/Binary.R')



