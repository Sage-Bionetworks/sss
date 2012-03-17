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
      of the form "response ~ data", where response is one of: (1) numeric vector (\code{sssLinearModel}); (2) numeric vector of 0s and 1s (\code{sssBinaryModel}); or (3) a Surv object (\code{sssSurvivalModel}), and data is a matrix with one row per observation and one column per predictor.  \code{sss} method dispatches on an object that has class which is an extension of \code{sssModel} as shown above - and is built based on the response in the specified formula.
    }
    \item{\code{...}}{
      optional arguments, including:
    }
    \item{\code{training}}{
      a numeric indicator (length of number of observations) specifying if an observation is to be used in model training (value of 1) or in the model testing validation (value of 0) - \code{training} is called "weights" in sss documentation
    }
    \item{\code{priormeanp}}{
      prior mean of number of included variables (this is the key sparsity control parameter) - priormeanp=v means that each variable is "in the model" with prior probability v/(number of predictors) - default is 5
    }
    \item{\code{pmax}}{
      maximum model size (maximum number of predictors in any model) - default is 15
    }
    \item{\code{iters}}{
      number of MCMC iterations - default is 100
    }
    \item{\code{nbest}}{
      number of models whose output is saved out of the \code{iters} iterations - default is 10
    }
    \item{\code{outputDir}}{
      directory to save text files from sss executable (R objects will be populated as value of \code{sss} call) - default is \code{tempdir()} which will be removed at end of current R session
    }
    \item{other}{
      named arguments overwriting of defaults specified in the slots of class \code{sssSetup} - only for advanced users.
    }
  }
}

\section{Value}{
  \describe{
    Return value is of class \code{sssResult}.
  }
}

\seealso{
  \describe{
    \item{model classes}{ \code{sssModel}, \code{sssBinaryModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{result classes}{ \code{sssResult} }
    \item{setup class}{ \code{sssSetup} }
  }
}

\keyword{methods}
