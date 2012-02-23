\name{sssModel-class}
\Rdversion{1.1}
\docType{class}
\alias{sssModel-class}
\alias{show,sssModel-method}
\alias{sss,sssModel-method}

\title{Virtual class \code{"sssModel"}}

\description{
A virtual class on which specific sss model types (binary, linear, survival) extend upon.  Created for object oriented dispatch of methods.
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
showClass("sssModel")
}

\keyword{classes}

