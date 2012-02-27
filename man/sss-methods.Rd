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
    \item{\code{signature(formula = formula, ...)}}{
      \code{formula} -- of the form "response ~ data", where response is one of: numeric vector (\code{sssLinearModel}); numeric vector of 0s and 1s (\code{sssBinaryModel}); or a Surv object (\code{sssSurvivalModel}). \code{sss} method dispatches on an object that has class which is an extension of \code{sssModel} as shown above - and is built based on the response in the specified formula.
      \code{...} -- optional arguments, including: (1) \code{subset} - a numeric vector specifying any subset of response and data (called "weights" in sss documentation); (2) \code{sssSetup} - named arguments overwriting of defaults specified in class \code{sssSetup} - only for advanced users.
    }
  }
}

\seealso{
  \describe{
    \item{classes}{ \code{sssModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
  }
}

\keyword{methods}
