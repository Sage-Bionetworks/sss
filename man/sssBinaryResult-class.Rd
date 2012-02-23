\name{sssBinaryResult-class}
\Rdversion{1.1}
\docType{class}
\alias{sssBinaryResult-class}

\title{Class \code{"sssBinaryResult"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}

\section{Objects from the Class}{
Objects are returned by default when an \code{sssBinaryModel} object is passed to the \code{"sss"} algorithm
}

\section{Slots}{
  \describe{
    \item{\code{sssModel}:}{Object of class \code{"sssBinaryModel"} - original object passed to the sss method }
    \item{\code{p}:}{Object of class \code{"list"} - number of predictors for this model (each list entry represents the i-th model) }
    \item{\code{score}:}{Object of class \code{"list"} - log posterior probability of this model (each list entry represents the i-th model) }
    \item{\code{indices}:}{Object of class \code{"list"} - the indices of the p variables in this model (each list entry represents the i-th model) }
    \item{\code{pmode}:}{Object of class \code{"list"} - posterior mode of the regression parameter vector beta including the intercept (each list entry represents the i-th model) }
    \item{\code{pvar}:}{Object of class \code{"list"} - estimated posterior variance matrix of beta in vectorized form including the intercept (each list entry represents the i-th model) }
  }
}

\section{Extends}{
Class \code{"\code{sssResult}"}, directly.
}

\seealso{
  \describe{
    \item{classes}{ \code{sssBinaryModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{constructors}{ \code{BinaryModel}, \code{LinearModel}, \code{SurvivalModel} }
    \item{methods}{ \code{sss} }
  }
}

\examples{
showClass("sssBinaryResult")
}

\keyword{classes}
