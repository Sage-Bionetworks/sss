\name{sssSurvivalResult-class}
\Rdversion{1.1}
\docType{class}
\alias{sssSurvivalResult}
\alias{sssSurvivalResult-class}

\title{Class \code{"sssSurvivalResult"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}

\section{Objects from the Class}{
Objects are returned by default from the \code{"sss"} algorithm
}

\section{Slots}{
  \describe{
    \item{\code{sssModel}:}{Object of class \code{"sssSurvivalModel"} - information about sss model fit }
    \item{\code{p}:}{Object of class \code{"list"} - number of predictors for this model (each list entry represents the i-th model) }
    \item{\code{score}:}{Object of class \code{"list"} - log posterior probability of this model (each list entry represents the i-th model) }
    \item{\code{indices}:}{Object of class \code{"list"} - the indices of the p variables in this model (each list entry represents the i-th model) }
    \item{\code{pmeanalpha}:}{Object of class \code{"list"} - posterior mean of the Weibull index parameter in this model (each list entry represents the i-th model) }
    \item{\code{pmode}:}{Object of class \code{"list"} - posterior mode of the regression parameter vector beta including the intercept (each list entry represents the i-th model) }
    \item{\code{pvar}:}{Object of class \code{"list"} - estimated posterior variance matrix of (alpha, beta) including intercept in vectorized form (each list entry represents the i-th model) }
  }
}

\section{Extends}{
Class \code{"\code{sssResult}"}, directly.
}

\seealso{
  \describe{
    \item{classes}{ \code{sssBinaryModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{methods}{ \code{sss} }
  }
}

\examples{
showClass("sssSurvivalResult")
}

\keyword{classes}
