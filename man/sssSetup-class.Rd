\name{sssSetup-class}
\Rdversion{1.1}
\docType{class}
\alias{sssSetup-class}
\alias{.writeSetup,sssSetup-method}

\title{Class \code{"sssSetup"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects of class \code{sssSetup} are created by class constructors (see See Also). Defaults of each slot are presented below. Individual slots can be overriden by specifying in ... of the class constructors.
}
\section{Slots}{
  \describe{
    \item{\code{nobservations}:}{Object of class \code{"numeric"} - SET BY MODEL CONSTRUCTORS }
    \item{\code{nvariables}:}{Object of class \code{"numeric"} - SET BY MODEL CONSTRUCTORS }
    \item{\code{datafile}:}{Object of class \code{"character"} - SET BY MODEL CONSTRUCTORS }
    \item{\code{responsefile}:}{Object of class \code{"character"} - SET BY MODEL CONSTRUCTORS }
    \item{\code{weightsfile}:}{Object of class \code{"character"} - SET BY MODEL CONSTRUCTORS }
    \item{\code{censorfile}:}{Object of class \code{"character"} - SET BY MODEL CONSTRUCTORS }
    \item{\code{modtype}:}{Object of class \code{"numeric"} - SET BY MODEL CONSTRUCTORS }
    \item{\code{outfile}:}{Object of class \code{"character"} - SET BY MODEL CONSTRUCTORS }
    \item{\code{iterout}:}{Object of class \code{"character"} - SET BY MODEL CONSTRUCTORS }
    \item{\code{summaryfile}:}{Object of class \code{"character"} - SET BY MODEL CONSTRUCTORS }
    \item{}{}
    \item{\code{priormeanp}:}{Object of class \code{"numeric"} - default is 5 - prior mean of number of included variables (this is the key sparsity control parameter) - priormeanp=v means that each variable is "in the model" with prior probability v/(number of predictors) }
    \item{\code{pstart}:}{Object of class \code{"numeric"} - default is 2 - initial model size (number of predictors for model to start sss) }
    \item{\code{pmax}:}{Object of class \code{"numeric"} - default is 15 - maximum model size (maximum number of predictors in any model) }
    \item{\code{onevar}:}{Object of class \code{"numeric"} - default is 1 - indicator to include all 1-variables models in search }
    \item{\code{iters}:}{Object of class \code{"numeric"} - default is 100 - total number of sss iterations }
    \item{\code{nbest}:}{Object of class \code{"numeric"} - default is 10 - number of best models to be saved/recorded }
    \item{\code{inneranneal1}:}{Object of class \code{"numeric"} - default is 0.6 - annealing parameter for variable replacement (DEFAULTS SHOULD BE USED) }
    \item{\code{inneranneal2}:}{Object of class \code{"numeric"} - default is 0.8 - annealing parameter for variable detection (DEFAULTS SHOULD BE USED) }
    \item{\code{inneranneal3}:}{Object of class \code{"numeric"} - default is 1 - annealing parameter for variable addition (DEFAULTS SHOULD BE USED) }
    \item{\code{outeranneal}:}{Object of class \code{"numeric"} - default is 0.4 - annealing parameter for second level model selection (DEFAULTS SHOULD BE USED) }
  }
}
\references{
http://www.stat.osu.edu/~hans/sss/readme.serial.html
}
\seealso{
  \describe{
    \item{classes}{ \linkS4class{sssBinaryModel}, \linkS4class{sssLinearModel}, \linkS4class{sssSurvivalModel} }
    \item{constructors}{ \linkS4method{BinaryModel}, \linkS4method{LinearModel}, \linkS4method{SurvivalModel} }
    \item{methods}{ \linkS4method{sss} }
  }
\examples{
showClass("sssSetup")
}
\keyword{classes}
