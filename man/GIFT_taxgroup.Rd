% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GIFT_taxgroup.R
\name{GIFT_taxgroup}
\alias{GIFT_taxgroup}
\title{Taxonomic group of species}
\usage{
GIFT_taxgroup(
  work_ID = NULL,
  taxon_lvl = c("family", "order", "higher_lvl")[1],
  return_ID = FALSE,
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/",
  taxonomy = NULL,
  species = NULL
)
}
\arguments{
\item{work_ID}{A vector defining the IDs of the species to retrieve
taxonomic groups for. \code{NULL} by default.}

\item{taxon_lvl}{taxonomic level to retrieve names for. \code{family} by default.
Check \code{GIFT_taxonomy()} for available levels. In addition to the available
levels one can put \code{higher_lvl} to retrieve the higher level groups
"Anthocerotophyta", "Marchantiophyta", "Bryophyta", "Lycopodiophyta",
"Monilophyta", "Gymnospermae", and "Angiospermae".}

\item{return_ID}{logical indicating whether to give back taxon_IDs instead
of names.}

\item{GIFT_version}{character string defining the version of the GIFT
database to use. The function retrieves by default the \code{latest} stable
version. If set to \code{beta}, the most up-to-date version which is still
subject to changes and edits is used.}

\item{api}{character string defining from which API the data will be
retrieved.}

\item{taxonomy}{option to supply taxonomy object here if loaded already to
avoid double loading. For internal use within GIFT functions. If \code{NULL}
(default) taxonomy will be loaded within this function.}

\item{species}{option to supply species names object here if loaded already
to avoid double loading. For internal use within GIFT functions. If \code{NULL}
(default) species will be loaded within this function.}
}
\value{
A vector with the taxonomic group of the species used as input.
}
\description{
Assign taxonomic groups of various hierarchical level to species from GIFT
(\code{work_ID}).
}
\examples{
\donttest{
ex <- GIFT_taxgroup(work_ID = c(1, 4, 7, 8), taxon_lvl = "family")
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
\code{\link[=GIFT_taxonomy]{GIFT_taxonomy()}}
}
