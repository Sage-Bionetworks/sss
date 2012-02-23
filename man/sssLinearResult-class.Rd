\name{sssLinearResult-class}
\Rdversion{1.1}
\docType{class}
\alias{sssLinearResult-class}

\title{Class \code{"sssLinearResult"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects are returned by default when an \linkS4class{sssLinearModel} object is passed to the \linkS4method{"sss"} algorithm
}

\section{Slots}{
  \describe{
    \item{\code{sssModel}:}{Object of class \code{"sssLinearModel"} - original object passed to the sss method }
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
Class \code{"\linkS4class{sssResult}"}, directly.
}

\seealso{
  \describe{
    \item{classes}{ \linkS4class{sssBinaryModel}, \linkS4class{sssLinearModel}, \linkS4class{sssSurvivalModel} }
    \item{constructors}{ \linkS4method{BinaryModel}, \linkS4method{LinearModel}, \linkS4method{SurvivalModel} }
    \item{methods}{ \linkS4method{sss} }
  }
}

\examples{
showClass("sssLinearResult")
}
\keyword{classes}
