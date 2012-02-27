\name{sss-methods}
\docType{methods}
\alias{sss}
\alias{sss-methods}
\alias{sss,sssModel-method}

\title{ Shotgun Stochastic Search (sss) }

\description{
  Runs a platform-specific version of Shotgun Stochastic Search (sss) from Hans, Dobra, and West.  Writes out temporary files, passes them to an executable, and reads summary files back into R.  Temporary files are discarded once the R session is terminated.
}

\section{Methods}{
  \describe{
    \item{\code{signature(formula = formula, ...)}}{}
    \item{\code{formula}}{
      of the form "response ~ data", where response is one of: (1) numeric vector (\code{sssLinearModel}); (2) numeric vector of 0s and 1s (\code{sssBinaryModel}); or (3) a Surv object (\code{sssSurvivalModel}), and data is a data.frame with one row per observation and one column per predictor.  \code{sss} method dispatches on an object that has class which is an extension of \code{sssModel} as shown above - and is built based on the response in the specified formula.
    }
    \item{\code{...}}{
      optional arguments, including: (1) \code{weights} - a numeric vector specifying weights given to each observation; (2) named arguments overwriting of defaults specified in the slots of class \code{sssSetup} - only for advanced users.
    }
  }
}

\seealso{
  \describe{
    \item{classes}{ \code{sssModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
  }
}

\keyword{methods}
