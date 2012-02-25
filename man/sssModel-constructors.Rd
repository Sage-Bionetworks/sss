\name{sssModel-constructors}
\docType{methods}
\alias{BinaryModel}
\alias{BinaryModel-methods}
\alias{BinaryModel,numeric,data.frame,missing-method}
\alias{BinaryModel,numeric,data.frame,numeric-method}
\alias{LinearModel}
\alias{LinearModel-methods}
\alias{LinearModel,numeric,data.frame,missing-method}
\alias{LinearModel,numeric,data.frame,numeric-method}
\alias{SurvivalModel}
\alias{SurvivalModel-methods}
\alias{SurvivalModel,numeric,numeric,data.frame,missing-method}
\alias{SurvivalModel,numeric,numeric,data.frame,numeric-method}

\title{ Methods for sssModel class constructors: \code{BinaryModel}, \code{LinearModel}, \code{SurvivalModel} }

\section{BinaryModel Methods}{
  \describe{
    \item{\code{signature(response = "numeric", data = "data.frame", weights = "missing", ...)}}{}
    \item{\code{signature(response = "numeric", data = "data.frame", weights = "numeric", ...)}}{
      Constructs a \code{sssBinaryModel} object given parameters \code{response} (numeric vector), \code{data} (data.frame), and optionally \code{weights} (numeric vector) and further named arguments passed by ... to override the default slots of the class \code{sssSetup}
    }
  }
}

\section{LinearModel Methods}{
  \describe{
    \item{\code{signature(response = "numeric", data = "data.frame", weights = "missing", ...)}}{}
    \item{\code{signature(response = "numeric", data = "data.frame", weights = "numeric", ...)}}{
      Constructs a \code{sssLinearModel} object given parameters \code{response} (numeric vector), \code{data} (data.frame), and optionally \code{weights} (numeric vector) and further named arguments passed by ... to override the default slots of the class \code{sssSetup}
    }
  }
}

\section{SurvivalModel Methods}{
  \describe{
    \item{\code{signature(timeToEvent = "numeric", censor = "numeric", data = "data.frame", weights = "missing", ...)}}{}
    \item{\code{signature(timeToEvent = "numeric", censor = "numeric", data = "data.frame", weights = "numeric", ...)}}{
      Constructs a \code{sssSurvivalModel} object given parameters \code{timeToEvent} (numeric vector), \code{censor} (numeric vector), \code{data} (data.frame), and optionally \code{weights} (numeric vector) and further named arguments passed by ... to override the default slots of the class \code{sssSetup}
    }
  }
}

\seealso{
  \describe{
    \item{classes}{ \code{sssBinaryModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{constructors}{ \code{BinaryModel}, \code{LinearModel}, \code{SurvivalModel} }
    \item{methods}{ \code{sss} }
  }
}

\keyword{methods}
