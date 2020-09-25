\name{DB_get_traits}
\alias{DB_get_traits}
\title{Retrieving traits}
\description{
Retrieving traits.
}
\usage{
DB_get_traits()
}
\arguments{
\item{trait_IDs}{a character string indicating which trait you want to retrieve. Traits must belong to the available list of traits.}

\item{agreement}{a numeric}

\item{bias_ref}{Boolean}

\item{bias_deriv}{Boolean}

\item{restricted}{Boolean}
}
\details{
}
\value{
A long-format data frame with 6 columns: `trait_ID`, `work_ID`, `species`, `trait_value`, `agreement` and `references`.
}
\details{}
\section{Note}{}

\examples{
wood <- DB_get_traits(trait_IDs = c("1.1.1", "1.2.1"), agreement = 0.66, bias_ref = FALSE, bias_deriv = FALSE, restricted = FALSE)
}
