\name{sssBinaryModel-class}
\Rdversion{1.1}
\docType{class}
\alias{sssBinaryModel}
\alias{sssBinaryModel-class}

\title{Class \code{"sssBinaryModel"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}

\section{Objects from the Class}{
Objects created by the sss method.
}

\section{Slots}{
  \describe{
    \item{\code{call}:}{Object of class \code{"call"} - passed from sss method }
    \item{\code{response}:}{Object of class \code{"numeric"} - numeric vector containing the binary response (0 or 1) for each observation / sample / patient to be fit by sss }
    \item{\code{data}:}{Object of class \code{"matrix"} - numeric matrix with one row per observation and one column per predictor }
    \item{\code{training}:}{Object of class \code{"numeric"} - optional numeric vector containing an indicator (0 or 1) for each observation indicating which observations should be included training set (1) or the testing set (0) }
    \item{\code{setupSpec}:}{Object of class \code{"sssSetup"} - an object specifying further optional parameters to be passed to the sss method call (see \code{sssSetup} for further information) }
  }
}

\section{Extends}{
Virtual class \code{"sssModel"}, directly.
}


\seealso{
  \describe{
    \item{model classes}{ \code{sssModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{result classes}{ \code{sssResult} }
    \item{setup class}{ \code{sssSetup} }
    \item{methods}{ \code{sss} }
  }
}

\examples{
showClass("sssBinaryModel")
}

\keyword{classes}
