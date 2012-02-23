\name{sssModel-constructors}
\docType{methods}
\alias{BinaryModel-methods}
\alias{BinaryModel,numeric,data.frame,missing-method}
\alias{BinaryModel,numeric,data.frame,numeric-method}
\alias{LinearModel-methods}
\alias{LinearModel,numeric,data.frame,missing-method}
\alias{LinearModel,numeric,data.frame,numeric-method}
\alias{SurvivalModel-methods}
\alias{SurvivalModel,numeric,numeric,data.frame,missing-method}
\alias{SurvivalModel,numeric,numeric,data.frame,numeric-method}

\title{ ~~ Methods for sssModel class constructors: \code{BinaryModel}, \code{LinearModel}, \code{SurvivalModel}  ~~}

\section{BinaryModel Methods}{
  \describe{
    \item{\code{signature(response = "numeric", data = "data.frame", weights = "missing", ...)}}{}
    \item{\code{signature(response = "numeric", data = "data.frame", weights = "numeric", ...)}}{
      Constructs a \linkS4class{sssBinaryModel} object given parameters \code{response} (numeric vector), \code{data} (data.frame), and optionally \code{weights} (numeric vector) and further named arguments passed by ... to override the default slots of the class \linkS4class{sssSetup}
    }
  }
}

\section{LinearModel Methods}{
  \describe{
    \item{\code{signature(response = "numeric", data = "data.frame", weights = "missing", ...)}}{}
    \item{\code{signature(response = "numeric", data = "data.frame", weights = "numeric", ...)}}{
      Constructs a \linkS4class{sssLinearModel} object given parameters \code{response} (numeric vector), \code{data} (data.frame), and optionally \code{weights} (numeric vector) and further named arguments passed by ... to override the default slots of the class \linkS4class{sssSetup}
    }
  }
}

\section{SurvivalModel Methods}{
  \describe{
    \item{\code{signature(timeToEvent = "numeric", censor = "numeric", data = "data.frame", weights = "missing", ...)}}{}
    \item{\code{signature(timeToEvent = "numeric", censor = "numeric", data = "data.frame", weights = "numeric", ...)}}{
      Constructs a \linkS4class{sssSurvivalModel} object given parameters \code{timeToEvent} (numeric vector), \code{censor} (numeric vector), \code{data} (data.frame), and optionally \code{weights} (numeric vector) and further named arguments passed by ... to override the default slots of the class \linkS4class{sssSetup}
    }
  }
}

\seealso{
  \describe{
    \item{classes}{ \linkS4class{sssBinaryModel}, \linkS4class{sssLinearModel}, \linkS4class{sssSurvivalModel} }
    \item{constructors}{ \linkS4method{BinaryModel}, \linkS4method{LinearModel}, \linkS4method{SurvivalModel} }
    \item{methods}{ \linkS4method{sss} }
  }
}

\keyword{methods}
