\name{GIFT_env}
\alias{GIFT_env}
\title{Environmental information}
\description{
Returns miscellaneous and rasters.
}
\usage{
GIFT_env()
}
\arguments{
}
\details{
}
\value{
A data frame.
}
\details{}
\section{Note}{}

\examples{
ex <- GIFT_env(miscellaneous = "perimeter")
ex <- GIFT_env(entity_ID = c(1,5), miscellaneous = c("perimeter", "biome"))

ex <- GIFT_env(entity_ID = c(1,5),
               miscellaneous = c("perimeter", "biome"),
               rasterlayer = c("mn30_grd", "wc2.0_bio_30s_01"),
               sumstat = "mean")

ex <- GIFT_env(entity_ID = c(1,5),
               miscellaneous = c("perimeter", "biome"),
               rasterlayer = c("mn30_grd", "wc2.0_bio_30s_01"),
               sumstat = c("mean", "med"))

ex <- GIFT_env(entity_ID = c(1,5),
               miscellaneous = c("perimeter", "biome"),
               rasterlayer = c("mn30_grd", "wc2.0_bio_30s_01"),
               sumstat = list(c("mean", "med"), "max"))

}
