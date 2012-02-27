\name{sssLinearResult-class}
\Rdversion{1.1}
\docType{class}
\alias{sssLinearResult}
\alias{sssLinearResult-class}

\title{Class \code{"sssLinearResult"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}

\section{Objects from the Class}{
Objects are returned by default from the \code{"sss"} algorithm
}

\section{Slots}{
  \describe{
    \item{\code{sssModel}:}{Object of class \code{"sssLinearModel"} - information about sss model fit }
    \item{\code{p}:}{Object of class \code{"list"} - number of predictors for this model (each list entry represents the i-th model) }
    \item{\code{score}:}{Object of class \code{"list"} - log posterior probability of this model (each list entry represents the i-th model) }
    \item{\code{indices}:}{Object of class \code{"list"} - the indices of the p variables in this model (each list entry represents the i-th model) }
    \item{\code{pmean}:}{Object of class \code{"list"} - posterior mean of the regression parameter vector beta excluding the intercept (each list entry represents the i-th model) }
    \item{\code{pvar}:}{Object of class \code{"list"} - posterior variance matrix of beta in vectorized form excluding the intercept (each list entry represents the i-th model) }
    \item{\code{residsd}:}{Object of class \code{"list"} - residual SD estimate (each list entry represents the i-th model) }
    \item{\code{postdf}:}{Object of class \code{"list"} posterior degrees of freedom (each list entry represents the i-th model) }
  }
}

\section{Extends}{
Class \code{"sssResult"}, directly.
}

\seealso{
  \describe{
    \item{classes}{ \code{sssBinaryModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{methods}{ \code{sss} }
  }
}

\examples{
showClass("sssLinearResult")
}

\keyword{classes}
