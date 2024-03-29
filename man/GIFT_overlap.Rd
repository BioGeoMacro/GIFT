% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GIFT_overlap.R
\name{GIFT_overlap}
\alias{GIFT_overlap}
\title{Spatial overlap between GIFT polygons and external polygons}
\usage{
GIFT_overlap(
  resource = "glonaf",
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/"
)
}
\arguments{
\item{resource}{A character string indicating from which resource the
spatial overlap is calculated. Available options are \code{glonaf} and \code{gmba}.
glonaf stands for Global Naturalized Alien Flora and gmba for Global
Mountain Biodiversity Assessment.}

\item{GIFT_version}{character string defining the version of the GIFT
database to use. The function retrieves by default the \code{latest} stable
version. If set to \code{beta}, the most up-to-date version which is still
subject to changes and edits is used.}

\item{api}{character string defining from which API the data will be
retrieved.}
}
\value{
A data frame with the spatial overlap.
}
\description{
Calculate the spatial overlap between GIFT polygons and shapefiles coming
from other resources
}
\details{
The columns of the data.frame are the following:

\emph{entity_ID} - Identification number of the GIFT polygon\cr
\emph{glonaf_ID} (or \emph{gmba_ID}) - Identification number of the polygon
from the other resource\cr
\emph{overlap12} - Spatial overlap in percentage between GIFT polygon and
the external polygon\cr
\emph{overlap21} - The other way around
}
\examples{
\donttest{
glonaf <- GIFT_overlap(resource = "glonaf")
gmba <- GIFT_overlap(resource = "gmba")
}

}
\references{
\if{html}{\out{<div class="sourceCode">}}\preformatted{ Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
 access the Global Inventory of Floras and Traits. Methods in Ecology
 and Evolution, 14, 2738-2748.
 https://doi.org/10.1111/2041-210X.14213

 Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
 Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
 https://doi.org/10.1111/jbi.13623
}\if{html}{\out{</div>}}
}
\seealso{
\code{\link[=GIFT_lists]{GIFT_lists()}}
}
