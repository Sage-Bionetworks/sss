\name{sssResult-class}
\Rdversion{1.1}
\docType{class}
\alias{sssResult}
\alias{sssResult-class}
\alias{show,sssResult-method}

\title{Results from call to sss method - \code{"sssResult"}}

\description{
  A generic result object that contains information about the model that was run as well as the results from running the sss method.
}

\section{Objects from the Class}{
Objects are returned by default from the \code{"sss"} algorithm
}

\section{Slots}{
  \describe{
    \item{\code{standScore}:}{Object of class \code{"numeric"} - the standardized scores for the n best models }
    \item{\code{postMargProb}:}{Object of class \code{"numeric"} - posterior marginal probabilities of variables included in the n best models sorted in decending order }
    \item{\code{testPredictionSummary}:}{Object of class \code{"numeric"} - weighted average of predictions for each sample passed in the testing set based on \code{standScore} }
    
    \item{\code{model}:}{Object which extends on class \code{"sssModel"} depending on type of model fit }
    \item{\code{nBestFits}}{Object of class \code{"list"} - specific information about the n best model fits - see next section for specifics for each type of model. }
  }
}

\section{nBestFits - \code{sssBinaryModel}}{
  \describe{
    \item{\code{p}:}{Object of class \code{"list"} - number of predictors for this model (each list entry represents the i-th model) }
    \item{\code{score}:}{Object of class \code{"list"} - log posterior probability of this model (each list entry represents the i-th model) }
    \item{\code{indices}:}{Object of class \code{"list"} - the indices of the p variables in this model (each list entry represents the i-th model) }
    \item{\code{pmode}:}{Object of class \code{"list"} - posterior mode of the regression parameter vector beta including the intercept (each list entry represents the i-th model) }
    \item{\code{pvar}:}{Object of class \code{"list"} - estimated posterior variance matrix of beta in vectorized form including the intercept (each list entry represents the i-th model) }
    \item{\code{trainPrediction}:}{Object of class \code{"list"} - predictions on internal training set }
    \item{\code{testPrediction}:}{Object of class \code{"list"} - predictions on test set (if available) based on models fit by training set }
  
  }
}

\section{nBestFits - \code{sssLinearModel}}{
  \describe{
    \item{\code{p}:}{Object of class \code{"list"} - number of predictors for this model (each list entry represents the i-th model) }
    \item{\code{score}:}{Object of class \code{"list"} - log posterior probability of this model (each list entry represents the i-th model) }
    \item{\code{indices}:}{Object of class \code{"list"} - the indices of the p variables in this model (each list entry represents the i-th model) }
    \item{\code{pmean}:}{Object of class \code{"list"} - posterior mean of the regression parameter vector beta excluding the intercept (each list entry represents the i-th model) }
    \item{\code{pvar}:}{Object of class \code{"list"} - posterior variance matrix of beta in vectorized form excluding the intercept (each list entry represents the i-th model) }
    \item{\code{residsd}:}{Object of class \code{"list"} - residual SD estimate (each list entry represents the i-th model) }
    \item{\code{postdf}:}{Object of class \code{"list"} posterior degrees of freedom (each list entry represents the i-th model) }
    \item{\code{trainPrediction}:}{Object of class \code{"list"} - predictions on internal training set }
    \item{\code{testPrediction}:}{Object of class \code{"list"} - predictions on test set (if available) based on models fit by training set }
  }
}

\section{nBestFits - \code{sssSurvivalModel}}{
  \describe{
    \item{\code{p}:}{Object of class \code{"list"} - number of predictors for this model (each list entry represents the i-th model) }
    \item{\code{score}:}{Object of class \code{"list"} - log posterior probability of this model (each list entry represents the i-th model) }
    \item{\code{indices}:}{Object of class \code{"list"} - the indices of the p variables in this model (each list entry represents the i-th model) }
    \item{\code{pmeanalpha}:}{Object of class \code{"list"} - posterior mean of the Weibull index parameter in this model (each list entry represents the i-th model) }
    \item{\code{pmode}:}{Object of class \code{"list"} - posterior mode of the regression parameter vector beta including the intercept (each list entry represents the i-th model) }
    \item{\code{pvar}:}{Object of class \code{"list"} - estimated posterior variance matrix of (alpha, beta) including intercept in vectorized form (each list entry represents the i-th model) }
    \item{\code{trainPrediction}:}{Object of class \code{"list"} - predictions on internal training set }
    \item{\code{testPrediction}:}{Object of class \code{"list"} - predictions on test set (if available) based on models fit by training set }
  }
}

\seealso{
  \describe{
    \item{model classes}{ \code{sssModel}, \code{sssBinaryModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{setup class}{ \code{sssSetup} }
    \item{methods}{ \code{sss} }
  }
}

\examples{
showClass("sssResult")
}

\keyword{classes}
