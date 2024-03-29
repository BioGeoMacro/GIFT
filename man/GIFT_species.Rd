% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GIFT_species.R
\name{GIFT_species}
\alias{GIFT_species}
\title{Species list in GIFT}
\usage{
GIFT_species(
  api = "https://gift.uni-goettingen.de/api/extended/",
  GIFT_version = "latest"
)
}
\arguments{
\item{api}{character string defining from which API the data will be
retrieved.}

\item{GIFT_version}{character string defining the version of the GIFT
database to use. The function retrieves by default the \code{latest} stable
version. If set to \code{beta}, the most up-to-date version which is still
subject to changes and edits is used.}
}
\value{
A data frame with 5 columns.
}
\description{
Retrieve the whole set of plant species available in GIFT.
}
\details{
Here is what each column refers to:

\emph{work_ID} - Identification number of the species\cr
\emph{genus_ID} - Identification number of the genus\cr
\emph{work_genus} - Genus name after taxonomic harmonization\cr
\emph{work_species} - Species name after taxonomic harmonization\cr
\emph{work_author} - Author who described the species (after taxonomic
harmonization)
}
\examples{
\donttest{
ex <- GIFT_species()
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
\code{\link[=GIFT_checklists]{GIFT_checklists()}}
}
