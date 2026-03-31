# Environmental data for GIFT checklists

Retrieve environmental data associated to each GIFT checklists. Sources
of environmental variables can come from raster layers or from shape
files (miscellaneous). Users need to define what variables they are
interested in and then ask for a set of summary statistics (in case of
raster layers).

## Usage

``` r
GIFT_env(
  entity_ID = NULL,
  miscellaneous = if (is.null(rasterlayer)) "area" else NULL,
  rasterlayer = NULL,
  sumstat = "mean",
  GIFT_version = "latest",
  api = "https://gift.uni-goettingen.de/api/extended/"
)
```

## Arguments

- entity_ID:

  A vector defining the ID of the lists to retrieve. `NULL` by default,
  in that case, every list from GIFT is retrieved.

- miscellaneous:

  character vector or list specifying the miscellaneous data to
  retrieve. . A list of all miscellaneous layers for which precomputed
  information exists in the database can be viewed in the output table
  returned by
  [`GIFT_env_meta_misc()`](https://biogeomacro.github.io/GIFT/reference/GIFT_env_meta_misc.md).

- rasterlayer:

  character vector or list specifying the raster data to retrieve. A
  list of all raster layers for which precomputed information exists in
  the database can be viewed in the output table returned by
  [`GIFT_env_meta_raster()`](https://biogeomacro.github.io/GIFT/reference/GIFT_env_meta_raster.md).

- sumstat:

  Vector or list indicating the desired summary statistics out of
  `c("min", "q05", "q10", "q20", "q25", "q30", "q40", "med", "q60", "q70", "q75", "q80", "q90", "q95", "max", "mean", "sd", "modal", "unique_n", "H", "n")`
  used to aggregate the information coming from the raster layers. If
  `sumstat` is a vector, the same summary statistics are used for all
  raster layers. If `sumstat` is a list, the first element defines the
  summary statistics for the first raster layer, the second for the
  second and so on.  

  **Important note**  
  Some summary statistics may not be informative depending on the
  environmental layer you ask for. For example, it is not relevant to
  retrieve the mean of soil classes for a polygon. The mode or Shannon
  index are more suitable in that case.

- GIFT_version:

  character string defining the version of the GIFT database to use. The
  function retrieves by default the `latest` stable version. If set to
  `beta`, the most up-to-date version which is still subject to changes
  and edits is used.

- api:

  character string defining from which API the data will be retrieved.

## Value

A data frame with the environmental values per polygon (entity_ID).

## Details

The columns of the data.frame are the following:

*entity_ID* - Identification number of the polygon  
*geo_entity* - Name of the polygon  
The other columns relate to the environmental variables the user asked
for.

## References

     Denelle, P., Weigelt, P., & Kreft, H. (2023). GIFT—An R package to
     access the Global Inventory of Floras and Traits. Methods in Ecology
     and Evolution, 14, 2738-2748.
     https://doi.org/10.1111/2041-210X.14213

     Weigelt, P, König, C, Kreft, H. GIFT – A Global Inventory of Floras and
     Traits for macroecology and biogeography. J Biogeogr. 2020; 47: 16– 43.
     https://doi.org/10.1111/jbi.13623

## See also

[`GIFT_env_meta_misc()`](https://biogeomacro.github.io/GIFT/reference/GIFT_env_meta_misc.md)
and
[`GIFT_env_meta_raster()`](https://biogeomacro.github.io/GIFT/reference/GIFT_env_meta_raster.md)

## Examples

``` r
# \donttest{
ex <- GIFT_env(entity_ID = c(1,5),
               miscellaneous = c("perimeter", "biome"),
               rasterlayer = c("mn30_grd", "wc2.0_bio_30s_01"),
               sumstat = list(c("mean", "med"), "max"))
#> You are asking for the latest stable version of GIFT which is 3.2.
# }
```
