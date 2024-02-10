
<!-- README.md is generated from README.Rmd. Please edit that file -->

# meteoR

<!-- badges: start -->

[![R-CMD-check](https://github.com/thieled/meteoR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thieled/meteoR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

meteoR is an unofficial R wrapper around the [OPTED Meteor
API](https://meteor.opted.eu/).

## Installation

You can install the development version of meteoR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("thieled/meteoR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(meteoR)

# Get the country uid from the API
countries <- call_meteor(method = "get",
                            ressource = "query",
                            type = "Country",
                            format = "dataframe",
                            n_max = Inf
)
#> 50 obs. + 50 obs. + 50 obs. + 50 obs. + 50 obs. + 2 obs.


countries_sel <- countries |> dplyr::filter(name %in% c("Austria",
                                                        "Germany",
                                                        "France",
                                                        "Italy"))
# Query the API for news sources from these countries
news_sources <- call_meteor(method = "get",
                            ressource = "query",
                            type = "NewsSource",
                            countries = countries_sel$uid,
                            geographic_scope = c("national", "multinational"),
                            format = "dataframe",
                            publication_kind = c("newspaper", "magazine"),
                            n_max = 50
)
#> 50 obs.


dplyr::glimpse(news_sources)
#> Rows: 50
#> Columns: 20
#> $ `_unique_name`           <chr> "newssource_it_20minuti_print", "newssource_f…
#> $ geographic_scope         <chr> "national", "national", "national", "national…
#> $ name                     <chr> "20 Minuti", "Alternatives Économiques", "Alt…
#> $ uid                      <chr> "0x249fc", "0x22321", "0x22323", "0x4201f", "…
#> $ wikidata_id              <chr> "Q7245532", "Q2840427", NA, "Q860331", NA, NA…
#> $ dgraph.type              <list> "NewsSource", "NewsSource", "NewsSource", "N…
#> $ alternate_names_1        <chr> "20minutes.fr", "Alter éco", "Alternatives éc…
#> $ alternate_names_2        <chr> "vingt minutes", "Alternatives economiques", …
#> $ alternate_names_3        <chr> NA, "Alternatives Economiques", NA, NA, NA, N…
#> $ channel__unique_name     <chr> "print", "print", "facebook", "print", "print…
#> $ channel_name             <chr> "Print", "Print", "Facebook", "Print", "Print…
#> $ channel_uid              <chr> "0x11", "0x11", "0xd", "0x11", "0x11", "0x11"…
#> $ publication_kind_1       <chr> "newspaper", "magazine", "magazine", "magazin…
#> $ publication_kind_2       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
#> $ countries__unique_name_1 <chr> "country_italy", "country_france", "country_f…
#> $ countries__unique_name_2 <chr> NA, NA, NA, "country_switzerland", NA, NA, NA…
#> $ countries_name_1         <chr> "Italy", "France", "France", "Germany", "Germ…
#> $ countries_name_2         <chr> NA, NA, NA, "Switzerland", NA, NA, NA, NA, NA…
#> $ countries_uid_1          <chr> "0x2d", "0x2f", "0x2f", "0x1b", "0x1b", "0x1b…
#> $ countries_uid_2          <chr> NA, NA, NA, "0x32", NA, NA, NA, NA, NA, "0x32…
```
