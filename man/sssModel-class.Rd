\name{sssModel-class}
\Rdversion{1.1}
\docType{class}
\alias{sssModel}
\alias{sssModel-class}
\alias{show,sssModel-method}

\title{Virtual class \code{"sssModel"}}

\description{
  A virtual class on which specific sss model types (binary, linear, survival) extend upon. Created for object oriented dispatch of methods.
}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\seealso{
  \describe{
    \item{classes}{ \code{sssBinaryModel}, \code{sssLinearModel}, \code{sssSurvivalModel} }
    \item{methods}{ \code{sss} }
  }
}

\examples{
showClass("sssModel")
}

\keyword{classes}

