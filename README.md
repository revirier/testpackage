# testpackage

The goal of testpackage is to learn the use of packages

## Example

This is a basic example which shows you how to summarize and polt FARS data

``` r
fars_read("../data/accident_2013.csv.bz2")

require(dplyr)
fars_summarize_years(c("2013"))
fars_map_state(10,2013)

```