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
Objects can be created by calls to the constructor \code{"BinaryModel"} or by directly calling \code{new("sssBinaryModel", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{response}:}{Object of class \code{"numeric"} - numeric vector containing the binary response (0 or 1) for each observation / sample / patient to be fit by sss }
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
    \item{sss}{\code{signature(object = "sssModel")}: sss master function dispatches all sss computations based on which extension of \code{sssModel} is passed to it - including \code{sssBinaryModel} }
	 }
}

\seealso{
  \describe{
    \item{classes}{ \code{sssModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{constructors}{ \code{BinaryModel}, \code{LinearModel}, \code{SurvivalModel} }
    \item{methods}{ \code{sss} }
  }
}

\examples{
showClass("sssBinaryModel")
}

\keyword{classes}
