\name{sssResult-class}
\Rdversion{1.1}
\docType{class}
\alias{sssResult-class}
\alias{show,sssResult-method}

\title{Virtual class \code{"sssResult"}}

\description{
A virtual class on which specific sss model result types (binary, linear, survival) extend upon.  Created for object oriented dispatch of methods.
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\seealso{
  \describe{
    \item{classes}{ \linkS4class{sssBinaryModel}, \linkS4class{sssLinearModel}, \linkS4class{sssSurvivalModel} }
    \item{constructors}{ \linkS4method{BinaryModel}, \linkS4method{LinearModel}, \linkS4method{SurvivalModel} }
    \item{methods}{ \linkS4method{sss} }
  }
}

\examples{
showClass("sssResult")
}

\keyword{classes}
