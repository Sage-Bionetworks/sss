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
Objects can be created by calls to the constructor \code{"SurvivalModel"} or by directly calling \code{new("sssSurvivalModel", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{timeToEvent}:}{Object of class \code{"numeric"} - numeric vector containing the time to event (or censor) for each observation / sample / patient }
    \item{\code{censor}:}{Object of class \code{"numeric"} - numeric vector containing either a 0 for censored observations or a 1 for observations with an observed event }
    \item{\code{data}:}{Object of class \code{"data.frame"} - data frame with one row per observation and one column per predictor }
    \item{\code{weights}:}{Object of class \code{"numeric"} - optional numeric vector containing an indicator (0 or 1) for each observation indicating which samples should be included in the model fit }
    \item{\code{setupSpec}:}{Object of class \code{"sssSetup"} - an object specifying further optional parameters to be passed to the sss method call (see \code{sssSetup} for further information) }
  }
}
\section{Extends}{
Virtual class \code{"\code{sssModel}"}, directly.
}
\section{Methods}{
  \describe{
    \item{sss}{\code{signature(object = "sssModel")}: sss master function dispatches all sss computations based on which extension of \code{sssModel} is passed to it - including \code{sssSurvivalModel} }
   }
}
\seealso{
  \describe{
    \item{classes}{ \code{sssModel}, \code{sssBinaryModel}, \code{sssLinearModel} }
    \item{constructors}{ \code{BinaryModel}, \code{LinearModel}, \code{SurvivalModel} }
    \item{methods}{ \code{sss} }
  }
}
\examples{
showClass("sssSurvivalModel")
}
\keyword{classes}
