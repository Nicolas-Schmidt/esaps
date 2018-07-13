<!-- README.md is generated from README.Rmd. Please edit that file -->
esaps <img src="logo.png" style="margin-left:10px;margin-bottom:5px;" width="160" align="right"></a>
----------------------------------------------------------------------------------------------------

[![Build
Status](https://travis-ci.org/Nicolas-Schmidt/esaps.svg?branch=master)](https://travis-ci.org/Nicolas-Schmidt/esaps)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/esaps)](https://cran.r-project.org/package=esaps)

### Description

It allows to construct two types of indicators used in the study of
Electoral Systems and Party Systems starting from electoral results
data. The Effective Number of Parties (Laakso and Taagepera (1979)) and
Electoral Volatility in its three versions (Pedersen (1979), Powell and
Tucker (2014) and Torcal and Lago (2015)).

### Installation

You can install the released version of esaps from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("esaps")
```

And the development version from [GitHub](https://cran.r-project.org/)
with:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("Nicolas-Schmidt/esaps")
```

### Example

This is a typical database with electoral results by countries. Between
countries, the number of parties varies as well as the number of
elections. When calculating indicators from databases of different
dimensions, the process becomes cumbersome.

``` r
library(esaps)

votes <- list(data.frame(country = rep("ARG", 3),
                          year = c(1995, 2000, 2005),
                          party_A = c(40,10,20),
                          party_B = c(35,20,40),
                          party_C = c(25,70,40)),
               data.frame(country = rep("URY", 4),
                          year = c(1995, 2000, 2005, 2010),
                          party_A = c(30,30,20,20),
                          party_B = c(30,50,40, 30),
                          party_C = c(30,10,30, 25),
                          party_D = c(10,10,10,25)),
               data.frame(country = rep("BRA", 2),
                          year = c(1993, 1998),
                          party_A = c(30, 55),
                          party_B = c(70, 45)))

print(votes)
#> [[1]]
#>   country year party_A party_B party_C
#> 1     ARG 1995      40      35      25
#> 2     ARG 2000      10      20      70
#> 3     ARG 2005      20      40      40
#> 
#> [[2]]
#>   country year party_A party_B party_C party_D
#> 1     URY 1995      30      30      30      10
#> 2     URY 2000      30      50      10      10
#> 3     URY 2005      20      40      30      10
#> 4     URY 2010      20      30      25      25
#> 
#> [[3]]
#>   country year party_A party_B
#> 1     BRA 1993      30      70
#> 2     BRA 1998      55      45
```

Once you have the data loaded in one or several electronic spreadsheets,
the process of loading, assembly and conversion of the database to start
calculating indicators is done with:

``` r
votes <- esaps_object(dataset = votes, name.country = "country", name.year = "year")
```

Then you can use any of the indicators:

``` r
volatility <- evolat(esapsObject = votes, method = "Pedersen", summary = FALSE)
print(volatility)
#>   country year eVolat
#> 1     ARG 2000     45
#> 2     ARG 2005     30
#> 6     BRA 1998     25
#> 3     URY 2000     20
#> 4     URY 2005     20
#> 5     URY 2010     15
```

You can also get a summary of the data by country:

``` r
votes <- list(data.frame(country = rep("ARG", 3),
                          year = c(1995, 2000, 2005),
                          party_A = c(40,10,20),
                          party_B = c(35,20,40),
                          party_C = c(25,70,30),
                          party_D = c(NA, NA, 10)),        
               data.frame(country = rep("URY", 4),
                          year = c(1995, 2000, 2005, 2010),
                          party_A = c(30,30,20,20),
                          party_B = c(30,50,40, 30),
                          party_C = c(30,10,30, 25),
                          party_D = c(10,10,10,25)),
               data.frame(country = rep("BRA", 2),
                          year = c(1993, 1998),
                          party_A = c(30, 55),
                          party_B = c(70, 45)))
votes <- esaps_object(dataset = votes, name.country = "country", name.year = "year")
volatility <- evolat(esapsObject = votes, method = "Powell and Tucker", summary = TRUE)
print(volatility)
#> [[1]]
#>   country year volat_A volat_B
#> 1     ARG 2000       0      45
#> 2     ARG 2005       5      35
#> 3     BRA 1998       0      25
#> 4     URY 2000       0      20
#> 5     URY 2005       0      20
#> 6     URY 2010       0      15
#> 
#> [[2]]
#>   country min.year max.year election mean_A mean_B
#> 1     ARG     2000     2005        2    2.5  40.00
#> 2     BRA     1998     1998        1    0.0  25.00
#> 3     URY     2000     2010        3    0.0  18.33

votes <- list(data.frame(country = rep("ARG", 3),
                          year = c(1995, 2000, 2005),
                          party_A = c(40,10,20),
                          party_B = c(35,20,40),
                          party_C = c(25,70,30),
                          party_D = c(NA, NA, 10),
                          magnitude = c(2,2,2)),        
               data.frame(country = rep("URY", 4),
                          year = c(1995, 2000, 2005, 2010),
                          party_A = c(30,30,20,20),
                          party_B = c(30,50,40, 30),
                          party_C = c(30,10,30, 25),
                          party_D = c(10,10,10,25),
                          magnitude = c(2,2,2,3)),
               data.frame(country = rep("BRA", 2),
                          year = c(1993, 1998),
                          party_A = c(30, 55),
                          party_B = c(70, 45),
                          magnitude = c(2,2)))

votes <- esaps_object(dataset = votes, name.country = "country", name.year = "year", name.M = "magnitude")
volatility <- evolat(esapsObject = votes, method = "Torcal and Lago", summary = TRUE)
print(volatility)
#> [[1]]
#>   country year exogenous endogenous
#> 1     ARG 2000        15         30
#> 2     ARG 2005        10         30
#> 3     BRA 1998         0         25
#> 4     URY 2000        10         10
#> 5     URY 2005         5         15
#> 6     URY 2010         0         15
#> 
#> [[2]]
#>   country min.year max.year election mean_end mean_exo
#> 1     ARG     2000     2005        2    30.00     12.5
#> 2     BRA     1998     1998        1    25.00      0.0
#> 3     URY     2000     2010        3    13.33      5.0
```

other way of loading data:

``` r
## 1
votes <- esaps_object(path = "...\data",
                      extension = "xlsx",
                      name.file = "votes.xlsx",
                      nCountry = 3, 
                      name.country = "country", 
                      name.year = "year")
## 2
votes <- esaps_object(path = "...\data",
                      extension = "xlsx",
                      name.file = c("votes_ARG.xlsx","votes_URY.xlsx", "votes_BRA.xlsx"),
                      nCountry = 3, 
                      name.country = "country", 
                      name.year = "year")
```

### Citation

To cite esaps in publications, please use:

``` r
citation("esaps")
```

### Author

Nicolas Schmidt (<nschmidt@cienciassociales.edu.uy>)
