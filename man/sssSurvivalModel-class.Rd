\name{sssSurvivalModel-class}
\Rdversion{1.1}
\docType{class}
\alias{sssSurvivalModel}
\alias{sssSurvivalModel-class}

\title{Class \code{"sssSurvivalModel"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}

\section{Objects from the Class}{
Objects created by the sss method.
}

\section{Slots}{
  \describe{
    \item{\code{call}:}{Object of class \code{"call"} - passed from sss method }
    \item{\code{timeToEvent}:}{Object of class \code{"numeric"} - numeric vector containing the time to event (or censor) for each observation / sample / patient }
    \item{\code{censor}:}{Object of class \code{"numeric"} - numeric vector containing either a 0 for censored observations or a 1 for observations with an observed event }
    \item{\code{data}:}{Object of class \code{"matrix"} - numeric matrix with one row per observation and one column per predictor }
    \item{\code{training}:}{Object of class \code{"numeric"} - optional numeric vector containing an indicator (0 or 1) for each observation indicating which observations should be included training set (1) or the testing set (0) }
    \item{\code{setupSpec}:}{Object of class \code{"sssSetup"} - an object specifying further optional parameters to be passed to the sss method call (see \code{sssSetup} for further information) }
  }
}

\section{Extends}{
Virtual class \code{"\code{sssModel}"}, directly.
}

\seealso{
  \describe{
    \item{classes}{ \code{sssModel}, \code{sssBinaryModel}, \code{sssLinearModel} }
    \item{methods}{ \code{sss} }
  }
}

\examples{
showClass("sssSurvivalModel")
}

\keyword{classes}
