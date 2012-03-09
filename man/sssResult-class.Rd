\name{sssResult-class}
\Rdversion{1.1}
\docType{class}
\alias{sssResult}
\alias{sssResult-class}
\alias{show,sssResult-method}

\title{Virtual class \code{"sssResult"}}

\description{
  A virtual class on which specific sss model result types (binary, linear, survival) extend upon.
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\seealso{
  \describe{
    \item{model classes}{ \code{sssModel}, \code{sssBinaryModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{result classes}{ \code{sssBinaryResult}, \code{sssLinearResult}, \code{sssSurvivalResult} }
    \item{setup class}{ \code{sssSetup} }
    \item{methods}{ \code{sss} }
  }
}

\examples{
showClass("sssResult")
}

\keyword{classes}
