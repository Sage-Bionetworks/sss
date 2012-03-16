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
    \item{\code{standScore}:}{Object of class \code{"numeric"} - the standardized scores for the n best models }
    \item{\code{postMargProb}:}{Object of class \code{"numeric"} - posterior marginal probabilities of variables included in the n best models sorted in decending order }
    \item{\code{wAvePredTest}:}{Object of class \code{"numeric"} - weighted average of predictions for each sample passed in the testing set based on \code{standScore} }
    
    \item{\code{sssModel}:}{Object of class \code{"sssSurvivalModel"} - information about sss model fit }
    \item{\code{nBestFits}}{Object of class \code{"list"} - specific information about the n best model fits }
    
    \item{- - - - \code{p}:}{Object of class \code{"list"} - number of predictors for this model (each list entry represents the i-th model) }
    \item{- - - - \code{score}:}{Object of class \code{"list"} - log posterior probability of this model (each list entry represents the i-th model) }
    \item{- - - - \code{indices}:}{Object of class \code{"list"} - the indices of the p variables in this model (each list entry represents the i-th model) }
    \item{- - - - \code{pmeanalpha}:}{Object of class \code{"list"} - posterior mean of the Weibull index parameter in this model (each list entry represents the i-th model) }
    \item{- - - - \code{pmode}:}{Object of class \code{"list"} - posterior mode of the regression parameter vector beta including the intercept (each list entry represents the i-th model) }
    \item{- - - - \code{pvar}:}{Object of class \code{"list"} - estimated posterior variance matrix of (alpha, beta) including intercept in vectorized form (each list entry represents the i-th model) }
    \item{- - - - \code{predTest}:}{Object of class \code{"list"} - predictions on test set (if available) based on models fit by training set }
  }
}

\section{Extends}{
Class \code{"sssResult"}, directly.
}

\seealso{
  \describe{
    \item{model classes}{ \code{sssModel}, \code{sssBinaryModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{result classes}{ \code{sssResult}, \code{sssBinaryResult}, \code{sssLinearResult} }
    \item{setup class}{ \code{sssSetup} }
    \item{methods}{ \code{sss} }
  }
}

\examples{
showClass("sssSurvivalResult")
}

\keyword{classes}
