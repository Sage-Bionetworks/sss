\name{sss-methods}
\docType{methods}
\alias{sss-methods}
\alias{sss,sssModel-method}

\title{ \code{sss} method }

\description{
  Runs a platform-specific version of Shotgun Stochastic Search (sss) from Hans, Dobra, and West.  Writes out temporary files, passes them to an executable, and reads summary files back into R.  Temporary files are discarded once the R session is terminated.
}

\section{Methods}{
  \describe{
    \item{\code{signature(object = "sssBinaryModel")}}{}
    \item{\code{signature(object = "sssLinearModel")}}{}
    \item{\code{signature(object = "sssSurvivalModel")}}{
      Dispatches on an object that has class which is an extension of \code{sssModel} as shown above. Objects passed must be built with either \code{BinaryModel}, \code{LinearModel}, or \code{SurvivalModel}.
    }
  }
}

\seealso{
  \describe{
    \item{classes}{ \code{sssModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{constructors}{ \code{BinaryModel}, \code{LinearModel}, \code{SurvivalModel} }
  }
}

\keyword{methods}
